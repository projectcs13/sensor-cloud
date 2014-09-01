%% @author Jacob Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%% This module will contain all functions needed to handle
%% http requests done to the webmachine regarding virtual streams
%%
%% @end

-module(virtual_streams).
-export([init/1,
		 allowed_methods/2,
		 content_types_provided/2,
		 content_types_accepted/2,
		 process_post/2,
		 process_search/3,
		 put_stream/2,
		 delete_resource/2,
		 get_vstream/2]).

-include_lib("erlastic_search.hrl").
-include("webmachine.hrl").
-include_lib("amqp_client.hrl").
-include_lib("pubsub.hrl").
-include("field_restrictions.hrl").

-define(ELASTIC_SEARCH_URL, api_help:get_elastic_search_url()).

%% @doc
%% Function: init/1
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end
-spec init([]) -> {ok, undefined}.
init([]) ->
    {ok, undefined}.

%% @doc
%% Function: allowed_methods/2
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end
-spec allowed_methods(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
allowed_methods(ReqData, State) ->
	case api_help:parse_path(wrq:path(ReqData)) of
		[{"vstreams"}] ->
			{['POST', 'GET'], ReqData, State};
		[{"vstreams", _Id}] ->
			{['POST', 'PUT', 'DELETE', 'GET'], ReqData, State};
		[{"vstreams", "_search"}] ->
			{['POST', 'GET'], ReqData, State};
		[{"users", _UserID}, {"vstreams"}] ->
			{['GET'], ReqData, State};
		[error] ->
			{[], ReqData, State}
	end.


%% @doc
%% Function: content_types_provided/2
%% Purpose: based on the Accept header on a 'GET' request, we provide different media types to the client.
%% A code 406 is returned to the client if we cannot return the media-type that the user has requested.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_provided(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
content_types_provided(ReqData, State) ->
		{[{"application/json", get_vstream}], ReqData, State}.


%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is allowed to be sent to the server.
%% A code 406 is returned to the client if we don't accept a media type that the client has sent.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_accepted(ReqData::term(),State::term()) -> {list(), term(), term()}.
content_types_accepted(ReqData, State) ->
	{[{"application/json", put_stream}], ReqData, State}.


%% @doc
%% Function: process_post/2
%% Purpose: decodes a JSON object and creates a virtual stream that aggregates
%% streams using a function
%% It is run automatically for POST requests
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%%
%% Side effects: Inserts vsdatapoints in the database
%% @end
-spec process_post(ReqData::tuple(), State::string()) -> {true, tuple(), string()}.
process_post(ReqData, State) ->
	case openidc:auth_request(ReqData) of
        {error, Msg} -> {{halt, 498}, wrq:set_resp_body(Msg, ReqData), State};
        {ok, _} ->
			case api_help:is_search(ReqData) of
				false ->
					{VirtualStreamJson,_,_} = api_help:json_handler(ReqData, State),
					{{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
					Date = api_help:generate_date([Year,Month,Day]),
					DateAdded = lib_json:add_values(VirtualStreamJson,[{creation_date, list_to_binary(Date)}]),
					StreamsInvolved = lib_json:get_field(VirtualStreamJson, "streams_involved"),
					TimestampFrom = lib_json:get_field(VirtualStreamJson, "timestampfrom"),
					Function = lib_json:get_field(VirtualStreamJson, "function"),
					AllowedFunctions = ["min", "max", "mean", "total", "diff"],
					case api_help:do_only_fields_exist(VirtualStreamJson,?ACCEPTED_FIELDS_VSTREAMS_CREATE) of
						true->
							case lists:member(binary_to_list(lists:nth(1, Function)), AllowedFunctions) of
								true ->
									Func = binary_to_list(lists:nth(1, Function)),
									if
										Func == "diff" andalso length(StreamsInvolved) =/= 1 ->
											{{halt, 404}, wrq:set_resp_body("Wrong number of streams involved for diff", ReqData), State};
										true ->
											{{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
											TimeStamp = binary:list_to_bin(api_help:generate_timestamp([Year,Month,Day,Hour,Minute,Second],0)),
											Initialized = lib_json:add_field(DateAdded,"last_updated",TimeStamp),
											case erlastic_search:index_doc(?INDEX, "virtual_stream", Initialized) of
												{error, Reason} ->
													{{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
												{ok,List} ->
													VirtualStreamId = lib_json:get_field(List, "_id"),
													if
														Func == "diff" ->
															ok;
														true ->
															reduce(VirtualStreamId, StreamsInvolved, TimestampFrom, Function, ReqData, State)
													end,
													StreamList = [{stream, binary_to_list(X)} || X <- StreamsInvolved],
													virtual_stream_process_supervisor:add_child(binary_to_list(VirtualStreamId), StreamList, Function),
													{true, wrq:set_resp_body(lib_json:encode(List), ReqData), State}
											end
									end;
								false ->
									{{halt, 404}, wrq:set_resp_body("404 Function missing or not supported", ReqData), State}
							end;
						false->
							{{halt,403}, wrq:set_resp_body("Unsupported field(s), virtual stream not created", ReqData), State}
					end;
				true ->
					process_search(ReqData, State, post)
			end
	end.


%% @doc
%% Function: reduce/6
%% Purpose: Gets information about which streams will be involved, a timestamp range
%% and the statistical function, it executes a query and posts the vsdatapoints returned
%% to the current virtual stream
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
-spec reduce(VirtualStreamId::string(), Streams::string(), TimestampFrom::string(), Function::string(), ReqData::tuple(), State::string()) ->
		  {true, tuple(), string()}.
reduce(VirtualStreamId, Streams, TimestampFrom, Function, ReqData, State) ->
	Query = create_query(Function, Streams, TimestampFrom),
	case erlastic_search:search_json(#erls_params{},?INDEX, "datapoint", lib_json:to_string(Query)) of
		{error, Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
		{ok,JsonStruct} ->
			DatapointsList = lib_json:get_field(JsonStruct, "facets.statistics.entries"),
			NewDatapoints = lists:map(fun(Json) ->
											FinalDatapoint = lib_json:set_attrs([
																				{"timestamp", list_to_atom(msToDate(lib_json:get_field(Json, "key")))},
																				{"stream_id", VirtualStreamId},
																				{"value",  lib_json:get_field(Json, binary_to_list(lists:nth(1, Function)))}
																				]),
											erlastic_search:index_doc(?INDEX, "vsdatapoint", lib_json:to_string(FinalDatapoint)),
											datapoints:update_fields_in_stream({"virtual_stream", lib_json:to_string(VirtualStreamId)}, list_to_atom(msToDate(lib_json:get_field(Json, "key"))))
										end, DatapointsList),
			{true, wrq:set_resp_body("\"status\":\"ok\"", ReqData), State}
	end.


%% @doc
%% Function: get_vstream/2
%% Purpose: Used to handle GET requests by giving the document with the given Id
%% or listing the documents that can be found from the restrictions
%% given by the URI.
%% Returns: {Success, ReqData, State}, where Success is true if the PUT request is
%% successful and false otherwise.
%% @end
-spec get_vstream(ReqData::term(),State::term()) -> {boolean(), term(), term()}.
get_vstream(ReqData, State) ->
	case openidc:auth_request(ReqData) of
        {error, Msg} -> {{halt, 498}, wrq:set_resp_body(Msg, ReqData), State};
        {ok, _} ->
			case wrq:get_qs_value("size",ReqData) of
				undefined ->
					Size = 100;
				SizeParam ->
					Size = list_to_integer(SizeParam)
			end,
			case proplists:get_value('user', wrq:path_info(ReqData)) of
				undefined ->
					case id_from_path(ReqData) of
						undefined ->
							Query = "{\"size\" :" ++ integer_to_list(Size) ++",\"query\" : {\"match_all\" : {}},
									\"filter\" : {\"bool\":{\"must_not\":{\"term\":{\"private\":\"true\"}}}}}",
							case erlastic_search:search_json(#erls_params{},?INDEX, "virtual_stream", Query) of
		 					{error, {Code, Body}} ->
									ErrorString = api_help:generate_error(Body, Code),
									{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
							{ok,JsonStruct} ->
									FinalJson = api_help:get_list_and_add_id(JsonStruct, vstreams),
									{FinalJson, ReqData, State}
							end;
						Id ->
							case erlastic_search:get_doc(?INDEX, "virtual_stream", Id) of
								{error, {Code, Body}} ->
									ErrorString = api_help:generate_error(Body, Code),
									{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
								{ok,JsonStruct} ->
									FinalJson = api_help:get_and_add_id(JsonStruct),
									{FinalJson, ReqData, State}
							end
					end;
				UserId ->
					Query = "{\"size\" :" ++ integer_to_list(Size) ++",\"query\" : {\"term\" : { \"user_id\":\"" ++ UserId ++ "\"}}}",
					case erlastic_search:search_json(#erls_params{},?INDEX, "virtual_stream", Query) of
						{error, {Code, Body}} ->
							ErrorString = api_help:generate_error(Body, Code),
							{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
						{ok,JsonStruct} ->
							FinalJson = api_help:get_list_and_add_id(JsonStruct, vstreams),
							{FinalJson, ReqData, State}
				end
			end
	end.

%% @doc
%% Function: put_stream/2
%% Purpose: Used to handle PUT requests by updating the given documents in elastic search
%% Returns: {Success, ReqData, State}, where Success is true if the PUT request is
%% successful and false otherwise.
%% @end
-spec put_stream(ReqData::term(),State::term()) -> {boolean(), term(), term()}.
put_stream(ReqData, State) ->
	case openidc:auth_request(ReqData) of
        {error, Msg} -> {{halt, 498}, wrq:set_resp_body(Msg, ReqData), State};
        {ok, _} ->
			VStreamId = proplists:get_value('id', wrq:path_info(ReqData)),
			{VStream,_,_} = api_help:json_handler(ReqData,State),
			Update = lib_json:set_attr(doc,VStream),
			case api_help:do_only_fields_exist(VStream,?ACCEPTED_FIELDS_VSTREAMS_UPDATE) of
				true->
					case api_help:update_doc(?INDEX, "virtual_stream", VStreamId, Update) of
						{error, {Code, Body}} ->
							ErrorString = api_help:generate_error(Body, Code),
							{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
						{ok,List} ->
							{true,wrq:set_resp_body(lib_json:encode(List),ReqData),State}
					end;
				false ->
					{{halt,403}, wrq:set_resp_body("Unsupported field(s)", ReqData), State}
			end
	end.

%% @doc
%% Function: delete_resource/2
%% Purpose: Used to handle DELETE requests for deleting the virtual stream and its
%% vsdatapoints in elastic search
%% Returns: {Success, ReqData, State}, where Success is true if delete is successful
%% and false otherwise.
%% FIX: This function relies on direct contact with elastic search at localhost:9200
%% @end
-spec delete_resource(ReqData::term(),State::term()) -> {boolean(), term(), term()}.
delete_resource(ReqData, State) ->
	case openidc:auth_request(ReqData) of
        {error, Msg} -> {{halt, 498}, wrq:set_resp_body(Msg, ReqData), State};
        {ok, _} ->
			case {proplists:get_value('id', wrq:path_info(ReqData))} of
				{Id} ->
					case streams:delete_data_points_with_stream_id(Id, "virtual_stream") of
						{error, {Code, Body}} ->
							ErrorString = api_help:generate_error(Body, Code),
							{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
						{ok} ->
							case erlastic_search:delete_doc(?INDEX,"virtual_stream", Id) of
								{error, {Code, Body}} ->
									ErrorString = api_help:generate_error(Body, Code),
									{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
								{ok,List} ->
									virtual_stream_process_supervisor:terminate_child(Id),
									{true,wrq:set_resp_body(lib_json:encode(List),ReqData),State}
							end
					end
			end
	end.


%% @doc
%% Function: create_query/3
%% Purpose: Creates the query for the function specified when creating a vstream
%% Returns: string()
%% @end
-spec create_query(Function::string(), Streams::string(), TimestampFrom::string()) -> {string()}.
create_query(Function, Streams, TimestampFrom) ->
	%size to be passed as a variable?? but it will not work in the terms facets query...
	Query = lib_json:set_attrs(
		  [{size, 100000},
		   {"query", "{}"},
		   {"query.filtered", "{}"},
		   {"query.filtered.query", "{}"},
		   {"query.filtered.query.terms", "{}"},
		   {"query.filtered.query.terms.stream_id", Streams},
		   {"query.filtered.filter", "{}"},
		   {"query.filtered.filter.range", "{}"},
		   {"query.filtered.filter.range.timestamp", "{}"},
		   {"query.filtered.filter.range.timestamp.gte", TimestampFrom}
	]),

	Interval = binary_to_list(lists:nth(2, Function)),
	Facet = [{"facets", "{}"},
			{"facets.statistics", "{}"},
			{"facets.statistics.histogram", "{}"},
			{"facets.statistics.histogram.key_field", 'timestamp'},
			{"facets.statistics.histogram.value_script", '(Long) _source.value'},
			{"facets.statistics.histogram.time_interval", list_to_atom(Interval)}],
	lib_json:add_values(Query, Facet)
.

%% @doc
%% Function: process_search/3
%% Purpose: Used to handle search requests on vstreams that come from POST requests
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%% @end
-spec process_search(ReqData::tuple(), State::string(), term()) ->
		{list(), tuple(), string()}.
process_search(ReqData, State, post) ->
	{Json,_,_} = api_help:json_handler(ReqData,State),
	case erlastic_search:search_json(#erls_params{},?INDEX, "virtual_stream", Json) of
			{error, {Code, Body}} ->
				ErrorString = api_help:generate_error(Body, Code),
				{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
			{ok,JsonStruct} ->
				FinalJson = api_help:get_list_and_add_id(JsonStruct),
				{true,wrq:set_resp_body(lib_json:encode(FinalJson),ReqData),State}
	end.


%% @doc
%% Function: id_from_path/2
%% Purpose: Retrieves the id from the path.
%% Returns: Id
%% @end
-spec id_from_path(string()) -> string().
id_from_path(RD) ->
	case wrq:path_info(id, RD) of
		undefined ->
			case string:tokens(wrq:disp_path(RD), "/") of
				["vstreams", Id] -> Id;
				_ -> undefined
			end;
 		Id -> Id
	end.


%% @doc
%% Function: msToDate/3
%% Purpose: Converts the unix timestamp to date format
%% Returns: date()
%% @end
msToDate(Milliseconds) ->
	BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
	Seconds = BaseDate + (Milliseconds div 1000),
	Date = calendar:gregorian_seconds_to_datetime(Seconds),
	{{Year,Month,Day},{Hour,Minute,Second}} = Date,
	TimeStamp = api_help:generate_timestamp([Year,Month,Day,Hour,Minute,Second],0),
	TimeStamp.
