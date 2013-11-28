%% @author Jacob Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%% This module will contain all functions needed to handle 
%% http requests done to the webmachine regarding virtual streams 
%%
%% @end

-module(virtual_streams).
-export([init/1, allowed_methods/2, content_types_provided/2,
		 content_types_accepted/2, process_post/2, process_search/3, put_stream/2]).

-include_lib("erlastic_search.hrl").
-include("webmachine.hrl").

-define(INDEX, "sensorcloud").

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
			{['POST'], ReqData, State}; 
		[{"vstreams", _Id}] -> %to be removed?
			{['POST', 'PUT'], ReqData, State};
		[{"vstreams", "_search"}] ->
			{['POST', 'GET'], ReqData, State};
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
		{[{"application/json", get_datapoint}], ReqData, State}.


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
%% Side effects: Inserts a new Datapoint in the database
%% @end
-spec process_post(ReqData::tuple(), State::string()) -> {true, tuple(), string()}.
process_post(ReqData, State) ->
	case api_help:is_search(ReqData) of
		false ->
			{VirtualStreamJson,_,_} = api_help:json_handler(ReqData, State),
			{{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
			Date = api_help:generate_date([Year,Month,Day]), % it is crashing if I add Hour, Minute, Second, check it again later
			DateAdded = lib_json:add_values(VirtualStreamJson,[{creation_date, list_to_binary(Date)}]),
erlang:display(DateAdded),
			case erlastic_search:index_doc(?INDEX, "vstream", DateAdded) of	
				{error, Reason} ->
					{{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
				{ok,List} -> 
					VirtualStreamId = lib_json:get_field(List, "_id"),
					StreamsInvolved = lib_json:get_field(VirtualStreamJson, "streams_involved"),
					TimestampFrom = lib_json:get_field(VirtualStreamJson, "timestampfrom"),
					Function = lib_json:get_field(VirtualStreamJson, "function"),
					reduce(VirtualStreamId, StreamsInvolved, TimestampFrom, Function, ReqData, State),
					{true, wrq:set_resp_body(lib_json:encode(List), ReqData), State}
			end;
		true ->
			process_search(ReqData, State, post)
	end
	.


%% @doc
%% Function: reduce/6
%% Purpose: Gets information about which streams will be involved, a timestamp range
%% and the statistical function, it executes a query and posts the datapoints returned
%% to the current virtual stream
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
-spec reduce(VirtualStreamId::string(), Streams::string(), TimestampFrom::string(), Function::string(), ReqData::tuple(), State::string()) -> %%should change to date type instead, also add ReqData, State
		  {true, tuple(), string()}.
reduce(VirtualStreamId, Streams, TimestampFrom, Function, ReqData, State) ->
	Query = create_query(Function, Streams, TimestampFrom),
	case erlastic_search:search_json(#erls_params{},?INDEX, "datapoint", lib_json:to_string(Query)) of
		{error, Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
		{ok,JsonStruct} ->
			%%   	{true,wrq:set_resp_body(lib_json:encode(FinalJson),ReqData),State},
			case string:str(Function, [<<"aggregate">>]) of %this might be removed if not used. It malfunctions if empty input
            	0 ->
					{{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
					TimeStamp = api_help:generate_timestamp([Year,Month,Day,Hour,Minute,Second],0), % to be reconsidered
					Datapoint = lib_json:get_field(JsonStruct, "facets.statistics"),
					FinalDatapoint = lib_json:set_attrs([
														   {"timestamp", list_to_atom(TimeStamp)}, %does timestamp have a meaning here? maybe do a reverse search for the time???
														   {"stream_id", list_to_atom(binary_to_list(VirtualStreamId))},
														   {"value",  lib_json:get_field(Datapoint, binary_to_list(lists:nth(1, Function)))}
														  ]),
					case erlastic_search:index_doc(?INDEX, "vsdatapoint", FinalDatapoint) of 
						{error, Reason2} -> erlang:display("Error");
						{ok,JsonStruct2} ->	erlang:display("Correct")						
					end;
				_->
					DatapointsList = lib_json:get_field(JsonStruct, "facets.statistics.entries"),
					NewDatapoints = lists:map(fun(Json) -> 
													  FinalDatapoint = lib_json:set_attrs([
																						   {"timestamp", list_to_atom(msToDate(lib_json:get_field(Json, "key")))},
																						   {"stream_id", VirtualStreamId},
																						   {"value",  lib_json:get_field(Json, binary_to_list(lists:nth(2, Function)))}
																						  ]),
													  erlastic_search:index_doc(?INDEX, "vsdatapoint", lib_json:to_string(FinalDatapoint)) %to add error check here
											  end, DatapointsList)
			
			%%will post all datapoints one by one, improve with bulk posting should be like the following
			%% 							case erlastic_search:bulk_index_docs(?INDEX, "datapoint", NewDatapoints) of
			%% 									{error, Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
			%% 									{ok,List} -> {true, wrq:set_resp_body(lib_json:encode(List), ReqData), State}
			%% 							end
			end,
			{true, wrq:set_resp_body("\"status\":\"ok\"", ReqData), State} %% need to fix message returned
	end.

 
%% @doc
%% Function: put_stream/2
%% Purpose: Used to handle PUT requests by updating the given documents in elastic search
%% Returns: {Success, ReqData, State}, where Success is true if the PUT request is
%% successful and false otherwise.
%% @end
-spec put_stream(ReqData::term(),State::term()) -> {boolean(), term(), term()}.
put_stream(ReqData, State) ->
	VStreamId = proplists:get_value('id', wrq:path_info(ReqData)),
	{VStream,_,_} = api_help:json_handler(ReqData,State),
	Update = api_help:create_update(VStream),
	case api_help:update_doc(?INDEX, "vstream", VStreamId, Update) of 
		{error, {Code, Body}} -> 
			ErrorString = api_help:generate_error(Body, Code),
			{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
		{ok,List} -> 
			{true,wrq:set_resp_body(lib_json:encode(List),ReqData),State}
	end.
	

%% @doc
%% Function: create_query/3
%% Purpose: Creates the query for the function specified
%% Returns: string()
%% @end
-spec create_query(Function::string(), Streams::string(), TimestampFrom::string()) -> {string()}.
create_query(Function, Streams, TimestampFrom) ->
	%size to be passed as a variable?? but it will not work in the terms facets query...
	Query = lib_json:set_attrs(
		  [{size, 100},
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
	case string:str(Function, [<<"aggregate">>]) of %is aggregate the proper name? maybe groupby?
            0 -> %	   	["min" "max" "mean" "???average???" "sum_of_squares" "variance" "std_deviation" ->
						%min, max etc are not added in the query yet, I will investigate if it is even possible
				Facet =  [{"facets", "{}"},
					  	 {"facets.statistics", "{}"},
					  	 {"facets.statistics.statistical", "{}"},
					  	 {"facets.statistics.statistical.script", '(Long) _source.value'}]; %I have to get the value here in another way
			_ -> %		["aggregate", "min" "max" "total(==sum)" ] ->		can also support custom calculations like *,+,-,/, ^(prob)
				% as above

				% the interval is also included in the function, in the 3rd position otherwise pick 10s??
				Interval = binary_to_list(lists:nth(3, Function)),
				Facet = [{"facets", "{}"},
						{"facets.statistics", "{}"},
						{"facets.statistics.histogram", "{}"},
						{"facets.statistics.histogram.key_field", 'timestamp'},
						{"facets.statistics.histogram.value_script", '(Long) _source.value'},
						{"facets.statistics.histogram.time_interval", list_to_atom(Interval)}]
	end,
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
	case erlastic_search:search_json(#erls_params{},?INDEX, "vstream", Json) of
			{error, {Code, Body}} ->
				ErrorString = api_help:generate_error(Body, Code),
				{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
			{ok,JsonStruct} ->
				FinalJson = lib_json:get_list_and_add_id(JsonStruct),
				{true,wrq:set_resp_body(lib_json:encode(FinalJson),ReqData),State}
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
