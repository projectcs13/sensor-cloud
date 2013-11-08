%% @author Tomas Sävström <tosa7943@student.uu.se>
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == streams ==
%% This module will contain all functions needed to handle 
%% http requests done to the webmachine regarding streams 
%%
%% @end
-module(streams).
-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2,
		 delete_resource/2, process_post/2, put_stream/2, get_stream/2]).



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
%% Purpose: Used to define what methods are allowed one the given URI's.
%% Returns: {List, ReqData, State}, where list is the allowed methods for the given URI. 
%% @end
-spec allowed_methods(ReqData::term(),State::term()) -> {list(), term(), term()}.

allowed_methods(ReqData, State) ->
	case api_help:parse_path(wrq:path(ReqData)) of
		[{"streams", "_search"}] ->
			{['POST', 'GET'], ReqData, State};
		[{"users", _UserID}, {"streams","_search"}] ->
			{['POST', 'GET'], ReqData, State};
		[{"users", _UserID}, {"resources", _ResourceID}, {"streams", "_search"}] ->
		  	{['POST', 'GET'], ReqData, State};
		[{"streams"}] ->
			{['POST', 'GET'], ReqData, State}; 
		[{"users", _UserID}, {"streams"}] ->
			{['POST', 'GET'], ReqData, State};
		[{"users", _UserID}, {"resources", _ResourceID}, {"streams"}] ->
			{['POST', 'GET'], ReqData, State};
		[{"streams", _StreamID}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[{"users", _UserID}, {"streams", _StreamID}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[{"users", _UserID}, {"resources", _ResourceID}, {"streams", _StreamID}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[error] ->
		    {[], ReqData, State} 
end.



%% @doc
%% Function: content_types_provided/2
%% Purpose: based on the Accept header on a 'GET' request, we provide different media types to the client.
%% A code 406 is returned to the client if we cannot return the media-type that the user has requested.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_provided(ReqData::term(),State::term()) -> {list(), term(), term()}.

content_types_provided(ReqData, State) ->
	{[{"application/json", get_stream}], ReqData, State}.


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
%% Function: delete_resource/2
%% Purpose: Used to handle DELETE requests by deleting the stream in elastic search
%% Returns: {Success, ReqData, State}, where Success is true if delete is successful
%% and false otherwise.
%% @end
-spec delete_resource(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

delete_resource(ReqData, State) ->
	Id = proplists:get_value('stream', wrq:path_info(ReqData)),
	case erlastic_search:delete_doc(?INDEX,"stream", Id) of
			{error,Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
			{ok,List} -> httpc:request(delete, {"http://localhost:9200/sensorcloud/datapoint/_query?q=streamid:" 
                  		 ++ Id, []}, [], []),
				 	{true,wrq:set_resp_body(lib_json:encode(List),ReqData),State}
	end.


%% @doc
%% Function: process_post/2
%% Purpose: Used to handle POST requests by creating streams, or search for streams in elastic search
%% Returns: {Success, ReqData, State}, where Success is true if the post request is
%% successful and false otherwise.
%% @end
-spec process_post(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

process_post(ReqData, State) ->
	case api_help:is_search(ReqData) of 
		false ->
			{Stream,_,_} = api_help:json_handler(ReqData, State),
			case proplists:get_value('user', wrq:path_info(ReqData)) of
				undefined ->
					UserAdded = Stream;
				UserId ->
					UserAdded = api_help:add_field(Stream,"user_id",UserId)
			end,
			case proplists:get_value('res', wrq:path_info(ReqData)) of
				undefined ->
					ResAdded = UserAdded;
				ResId ->
					ResAdded = api_help:add_field(UserAdded,"resource_id",ResId)
			end,
			case lib_json:get_field(ResAdded,"resource_id") == undefined of
				true -> {false, wrq:set_resp_body("\"resource_id_missing\"",ReqData), State};
				false ->
					{{Year,Month,Day},_} = calendar:local_time(),
					Date = generate_date([Year,Month,Day]),
					DateAdded = api_help:add_field(ResAdded,"creation_date",Date),
					case erlastic_search:index_doc(?INDEX, "stream", DateAdded) of	
						{error, Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
						{ok,List} -> 
							suggest:update_suggestion(ResAdded),
							{true, wrq:set_resp_body(lib_json:encode(List), ReqData), State}
					end
			end;
		true ->
			process_search_post(ReqData,State)	
	end.


%% @doc
%% Function: process_search_post/2
%% Purpose: Used to handle search requests that come from POST requests
%% Returns: {Success, ReqData, State}, where Success is true if the search request is
%% successful and false otherwise.
%% @end
-spec process_search_post(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

process_search_post(ReqData, State) ->

        erlang:display("search with json request"),
        {Json,_,_} = api_help:json_handler(ReqData,State),
        case proplists:get_value('res', wrq:path_info(ReqData)) of
                undefined ->
                        FilteredJson = filter_json(Json);
                ResId ->
                        ResQuery = "\"resource\":" ++ ResId,
                        FilteredJson = filter_json(Json, ResQuery)
        end,
        erlang:display(FilteredJson),
        case erlastic_search:search_json(#erls_params{},?INDEX, "stream", FilteredJson) of % Maybe wanna take more
                {error,Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
                {ok,List} -> {true,wrq:set_resp_body(lib_json:encode(List),ReqData),State} % May need to convert
        end.


%% @doc
%% Function: process_search_get/2
%% Purpose: Used to handle search requests that come from GET requests
%% Returns: {Success, ReqData, State}, where Success is true if the search request is
%% successful and false otherwise.
%% @end
-spec process_search_get(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

process_search_get(ReqData, State) ->
	URIQuery = wrq:req_qs(ReqData),
	case proplists:get_value('user', wrq:path_info(ReqData)) of
		undefined ->
			UserQuery = [],
			UserDef = false;
		UserId ->
			UserQuery = "user_id:" ++ UserId,
			UserDef = true
		end,
	case proplists:get_value('res', wrq:path_info(ReqData)) of
		undefined ->
			ResQuery = [],
			ResDef = false;
		ResId ->
			ResQuery = "resource_id:" ++ ResId,
			ResDef = true
	end,
	case ResDef and UserDef of
		true -> Query = UserQuery ++ "&" ++ ResQuery; 
		false -> case ResDef or UserDef of
					 true -> Query = UserQuery ++ ResQuery;
					 false -> Query = ""
				 end
	end,
	FullQuery = lists:append(api_help:transform(URIQuery,ResDef or UserDef),Query),
	case erlastic_search:search_limit(?INDEX, "stream", FullQuery,200) of % Maybe wanna take more
		{error,Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
		{ok,List} -> {lib_json:encode(List),ReqData,State} 
	end.


%% @doc
%% Function: put_stream/2
%% Purpose: Used to handle PUT requests by updating the given documents in elastic search
%% Returns: {Success, ReqData, State}, where Success is true if the PUT request is
%% successful and false otherwise.
%% @end
-spec put_stream(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

put_stream(ReqData, State) ->
	StreamId = proplists:get_value('stream', wrq:path_info(ReqData)),
	{Stream,_,_} = api_help:json_handler(ReqData,State),
	Update = api_help:create_update(Stream),
	case api_help:update_doc(?INDEX, "stream", StreamId, Update) of 
		{error,Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
		{ok,List} -> {true,wrq:set_resp_body(lib_json:encode(List),ReqData),State}
	end.




%% @doc
%% Function: get_stream/2
%% Purpose: Used to handle GET requests by giving the document with the given
%% Id or listing the documents that can be found from the restrictions
%% given by the URI.
%% Returns: {Success, ReqData, State}, where Success is true if the PUT request is
%% successful and false otherwise.
%% @end
-spec get_stream(ReqData::term(),State::term()) -> {boolean(), term(), term()}.


get_stream(ReqData, State) ->
	case api_help:is_search(ReqData) of
		true -> process_search_get(ReqData,State);
		false ->
			case proplists:get_value('stream', wrq:path_info(ReqData)) of
				undefined ->
				% List streams based on URI
					case proplists:get_value('user', wrq:path_info(ReqData)) of
						undefined ->
							UserQuery = [],
							UserDef = false;
						UserId ->
							UserQuery = "user_id:" ++ UserId,
							UserDef = true
					end,
					case proplists:get_value('res', wrq:path_info(ReqData)) of
						undefined ->
							ResQuery = [],
							ResDef = false;
						ResId ->
							ResQuery = "resource_id:" ++ ResId,
							ResDef = true
					end,
					case ResDef and UserDef of
						true -> Query = ResQuery;
						false -> case ResDef or UserDef of
							 		true -> Query = UserQuery ++ ResQuery;
							 		false -> Query = "*"
								 end
					end,
					case erlastic_search:search_limit(?INDEX, "stream", Query,200) of % Maybe wanna take more
						{error,Reason} -> 
						      {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
					        {ok,JsonStruct} ->
						       FinalJson = lib_json:get_list_and_add_id(JsonStruct),
						       {FinalJson, ReqData, State} 
					end;
				StreamId ->
				% Get specific stream
					case erlastic_search:get_doc(?INDEX, "stream", StreamId) of 
						{error, Reason} -> 
							{{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
						{ok,JsonStruct} -> 	 
						        FinalJson = lib_json:get_and_add_id(JsonStruct),
						        {FinalJson, ReqData, State}
					end
				end
	end.

%% @doc
%% Function: filter_json/1
%% Purpose: Used to add private filters to the json query
%% Returns: JSON string that is updated with filter
%% @end
filter_json(Json) ->
        NewJson = string:sub_string(Json,1,string:len(Json)-1),
        "{\"query\":{\"filtered\":"++NewJson++",\"filter\":{\"bool\":{\"must\":{\"term\":{\"private\":\"false\"}}}}}}}".

%% @doc
%% Function: filter_json/2
%% Purpose: Used to add private and resource filters to the json query
%% Returns: JSON string that is updated with filter
%% @end
filter_json(Json,ResourceQuery) ->
        NewJson = string:sub_string(Json,1,string:len(Json)-1),
        "{\"query\":{\"filtered\":"++NewJson++",\"filter\":{\"bool\":{\"must\":[{\"term\":{\"private\":\"false\"}},{\"term\":{"++ResourceQuery++"}}]}}}}}".

%% @doc
%% Function: generate_date/2
%% Purpose: Used to create a date valid in ES
%%          from the input which should be the list
%%          [Year,Mounth,Day]
%% Returns: The generated timestamp
%%
%% @end
-spec generate_date(DateList::list()) -> string().

generate_date([First]) ->
	case First < 10 of
		true -> "0" ++ integer_to_list(First);
		false -> "" ++ integer_to_list(First)
	end;
generate_date([First|Rest]) ->
	case First < 10 of
		true -> "0" ++ integer_to_list(First) ++ "-" ++ generate_date(Rest);
		false -> "" ++ integer_to_list(First) ++ "-" ++ generate_date(Rest)
	end.
