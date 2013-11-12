%%%-------------------------------------------------------------------
%%% @author ProjectCS13 <> Andreas, Carl, Tomas
%%% @doc
%%% This module handles the uri requests that are related to resources.
%%% @end
%%% Created : 9 Oct 2013 by ProjectCS13 <>
%%%-------------------------------------------------------------------
-module(resources).
-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2,
		 delete_resource/2, process_post/2, put_resource/2, get_resource/2, delete_streams_with_resource_id/1]).

-include_lib("webmachine.hrl").
-include_lib("erlastic_search.hrl").

-define(INDEX, "sensorcloud").
-define(RESTRCITEDUPDATE, ["user_id","type","accuracy","manufacturer","uri","creation_date"]).
-define(RESTRCITEDCREATE, ["creation_date"]).

%% @doc
%% Function: init/1
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end
-spec init([]) -> {ok, undefined}.
init([]) -> 
	%erlastic_search_app:start(), %% start this in the make file somehow
	{ok, undefined}.

%% @doc
%% Function: allowed_methods/2
%% Purpose: Defines which HTTP methods are allowed
%% Returns: {List of allowed HTTP requests, string , string()}
%% @end
-spec allowed_methods(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
allowed_methods(ReqData, State) ->
	case api_help:parse_path(wrq:path(ReqData)) of
		[ {"resources"}] ->
			{['POST'], ReqData, State};
		[ {"resources", _ResourceID}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[{"users", _UserID}, {"resources"}] ->
			{['GET','POST'], ReqData, State};
		[{"users", _UserID}, {"resources", "_search" ++ _Query}] ->
			{['GET', 'POST'], ReqData, State};
		[{"users", _UserID}, {"resources", _ResourceID}] ->
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
-spec content_types_provided(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
content_types_provided(ReqData, State) ->
	{[{"application/json", get_resource}], ReqData, State}.


%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is allowed to be sent to the server.
%% A code 406 is returned to the client if we don't accept a media type that the client has sent.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_accepted(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
content_types_accepted(ReqData, State) ->
	{[{"application/json", put_resource}], ReqData, State}.


%% @doc
%% Function: delete_resource/2
%% Purpose: Deletes a resource and the streams associated with it
%% Returns:  ERROR = {{error,Errorcode} ReqData, State}
%%			 OK = {ok, ReqData, State}
%% @end
-spec delete_resource(ReqData::tuple(), State::string()) -> {string(), tuple(), string()}.
delete_resource(ReqData, State) ->
	Id = proplists:get_value('resourceid', wrq:path_info(ReqData)),
	%% TODO Authentication
	case delete_streams_with_resource_id(Id) of
		{error, {Code, Body}} -> 
            ErrorString = api_help:generate_error(Body, Code),
            {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
		{ok} ->
			case erlastic_search:delete_doc(?INDEX,"resource", Id) of
				{error, {Code, Body}} -> 
            		ErrorString = api_help:generate_error(Body, Code),
            		{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
				{ok,List} -> 
					{true,wrq:set_resp_body(lib_json:encode(List),ReqData),State}
			end
	end.

%% @doc
%% Function: delete_streams_with_resource_id/1
%% Purpose: Deletes the first 500 streams associated with the given resourceid
%% Returns:  ERROR = {error,Errorcode}
%%			 OK = {ok}
%% @end
-spec delete_streams_with_resource_id(Id::string()) -> term().

delete_streams_with_resource_id(Id) ->
	api_help:refresh(),
	Query = "resource_id:" ++ Id, 
	case erlastic_search:search_limit(?INDEX, "stream", Query,500) of
		{error,Reason} -> 
			{error,Reason};
		{ok,List} -> 
			case get_streams(List) of
				[] -> {ok};
				Streams ->
					case delete_streams(Streams) of
						{error,Reason} -> {error, Reason};
						{ok} -> {ok}
					end
			end
	end.

%% @doc
%% Function: get_streams/1
%% Purpose: get a list of ids of a list of JSON objects
%% Returns:  a list with the ids of the JSON objects given
%% @end
-spec get_streams(JSON::string()) -> list().

get_streams(JSON) when is_tuple(JSON)->
	Result = lib_json:get_field(JSON, "hits.hits"),
	get_streams(Result);
get_streams(undefined) ->
	[];
get_streams([]) ->
	[];
get_streams([JSON | Tl]) ->
	case lib_json:get_field(JSON, "_id") of
		undefined -> [];
		Id -> [Id] ++ get_streams(Tl)
	end.



%% @doc
%% Function: delete_streams/1
%% Purpose: Deletes all streams in the given list, the list elements are streamIds as binaries
%% Returns:  ok, or {{error,_Reason}, StreamId, Rest} where StreamId is the binary Id of the stream for which deletion failed
%% @end

delete_streams([]) -> {ok};
delete_streams([StreamId|Rest]) ->
	case streams:delete_data_points_with_stream_id(StreamId) of
        {error,{Code, Body}} -> 
            {error,{Code, Body}};
        {ok} ->
			case erlastic_search:delete_doc(?INDEX, "stream", StreamId) of 
				{error,Reason} -> {error,Reason};
				{ok,_List} -> delete_streams(Rest)
			end
	end.

%% @doc
%% Function: process_post/2
%% Purpose: Handle POST request, only working for create and not search - AS OF SPRINT 3
%% Returns:  {JSON-object(string), ReqData, State}
%% @end
-spec process_post(ReqData::tuple(), State::string()) -> {atom(), tuple(), string()}.
process_post(ReqData, State) ->
	URIList = string:tokens(wrq:path(ReqData), "/"),
	IsSearch = (string:sub_string(lists:nth(length(URIList),URIList),1,7) == "_search"),
	case IsSearch of 
		false ->
			% Create

			{Resource,_,_} = api_help:json_handler(ReqData,State),
			case do_any_field_exist(Resource,?RESTRCITEDCREATE) of
				true ->
					ResFields1 = lists:foldl(fun(X, Acc) -> X ++ ", " ++ Acc end, "", ?RESTRCITEDCREATE),
					ResFields2 = string:sub_string(ResFields1, 1, length(ResFields1)-2),
					{{halt,409}, wrq:set_resp_body("{\"error\":\"Error caused by restricted field in document, these fields are restricted : " ++ ResFields2 ++"\"}", ReqData), State};
				false ->
					{{Year,Month,Day},_} = calendar:local_time(),
					Date = generate_date([Year,Month,Day]),
					DateAdded = api_help:add_field(Resource,"creation_date",Date),
					case erlastic_search:index_doc(?INDEX,"resource",DateAdded) of 
				{error, {Code, Body}} -> 
            		ErrorString = api_help:generate_error(Body, Code),
            		{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
						{ok, Json} -> 
							ResourceId = lib_json:get_field(Json, "_id"),
							suggest:add_suggestion(Resource, ResourceId),
							{true, wrq:set_resp_body(lib_json:encode(Json), ReqData), State}
					end
			end;
		true ->
			% Search
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
	{Json,_,_} = api_help:json_handler(ReqData,State),
	case proplists:get_value('userid', wrq:path_info(ReqData)) of
		undefined ->
			{{halt,405}, ReqData, State};
		UserId ->
			case wrq:get_qs_value("size",ReqData) of 
	            undefined ->
	                Size = "10";
	            SizeParam ->
	                Size = SizeParam
	        end,
	        case wrq:get_qs_value("from",ReqData) of
	            undefined ->
	                From = "0";
	            FromParam ->
	                From = FromParam
	        end,
			UserQuery = "\"user_id\":" ++ UserId,
			FilteredJson = filter_json(Json, UserQuery, From, Size),
			case erlastic_search:search_json(#erls_params{},?INDEX, "resource", FilteredJson) of % Maybe wanna take more
				{error, {Code, Body}} -> 
            		ErrorString = api_help:generate_error(Body, Code),
            		{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
				{ok,List} -> {true,wrq:set_resp_body(lib_json:encode(List),ReqData),State} % May need to convert
			end
	end.


%% @doc
%% Function: put_resource/2
%% Purpose: Updates the resource in the database
%% It is run automatically for POST and PUT requests
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%% @end
-spec put_resource(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
put_resource(ReqData, State) ->	
	%check if doc already exists
	Id = id_from_path(ReqData),
	case erlastic_search:get_doc(?INDEX, "resource", Id) of
		{error, {Code, Body}} -> 
            ErrorString = api_help:generate_error(Body, Code),
            {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
		{ok, _} ->
			{UserJson,_,_} = api_help:json_handler(ReqData, State),
			case do_any_field_exist(UserJson,?RESTRCITEDUPDATE) of
				true ->
					ResFields1 = lists:foldl(fun(X, Acc) -> X ++ ", " ++ Acc end, "", ?RESTRCITEDUPDATE),
					ResFields2 = string:sub_string(ResFields1, 1, length(ResFields1)-2),
					{{halt,409}, wrq:set_resp_body("{\"error\":\"Error caused by restricted field in document, these fields are restricted : " ++ ResFields2 ++"\"}", ReqData), State};
				false ->
					Update = api_help:create_update(UserJson),
					case api_help:update_doc(?INDEX,"resource", Id, Update) of 
				{error, {Code, Body}} -> 
            		ErrorString = api_help:generate_error(Body, Code),
            		{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
						{ok,List} -> 
							suggest:update_resource(UserJson, Id),
							{true,wrq:set_resp_body(lib_json:encode(List),ReqData),State}
					end
			end
	end.


%% @doc
%% Function: get_resource/2
%% Purpose: Handle GET request
%% Returns:  {JSON-object(string), ReqData, State}
%% @end
-spec get_resource(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
get_resource(ReqData, State) ->
	case api_help:is_search(ReqData) of
		false ->
			case proplists:get_value('resourceid', wrq:path_info(ReqData)) of
				undefined ->
					% List resources based on URI
				    case wrq:get_qs_value("size",ReqData) of 
			            undefined ->
			                Size = 100;
			            SizeParam ->
			                Size = list_to_integer(SizeParam)
			        end,
					case proplists:get_value('userid', wrq:path_info(ReqData)) of
						undefined ->
							Query = [];
						UserId ->
							Query = "user_id:" ++ UserId
					end,
					case erlastic_search:search_limit(?INDEX, "resource", Query, Size) of % Maybe wanna take more
						{error, {Code, Body}} -> 
            				ErrorString = api_help:generate_error(Body, Code),
            				{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
						{ok,JsonStruct} ->
							FinalJson = lib_json:get_list_and_add_id(JsonStruct, resources),
							{FinalJson, ReqData, State} 
					end;
				ResourceId ->
					% Get specific resource
					case erlastic_search:get_doc(?INDEX, "resource", ResourceId) of 
						{error, {Code, Body}} -> 
            				ErrorString = api_help:generate_error(Body, Code),
            				{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};			
						{ok,JsonStruct} ->
							FinalJson = lib_json:get_and_add_id(JsonStruct),
							{FinalJson, ReqData, State} 
					end
			end;
		true ->
			process_search(ReqData,State, get)
	end.


%% @doc
%% Function: process_search/3
%% Purpose: Does search for Users for either search done with POST or GET
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%% @end
-spec process_search(ReqData::tuple(), State::string(), term()) ->
	{list(), tuple(), string()}.
process_search(ReqData, State, post) ->
	{Json,_,_} = api_help:json_handler(ReqData,State),
	{struct, JsonData} = mochijson2:decode(Json),
	Query = api_help:transform(JsonData),
	case erlastic_search:search_limit(?INDEX, "resource", Query, 10) of
		{error, {Code, Body}} -> 
            ErrorString = api_help:generate_error(Body, Code),
            {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
		{ok,List} -> {true, wrq:set_resp_body(lib_json:encode(List),ReqData),State}
	end;
process_search(ReqData, State, get) ->
	TempQuery = wrq:req_qs(ReqData),
	TransformedQuery = api_help:transform(TempQuery),
	case erlastic_search:search_limit(?INDEX, "resource", TransformedQuery, 10) of
		{error, {Code, Body}} -> 
            ErrorString = api_help:generate_error(Body, Code),
            {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
		{ok,List} -> {lib_json:encode(List),ReqData,State} % May need to convert
	end.

%% @doc
%% Function: id_from_path/1
%% Purpose: Retrieves the id from the path.
%% Returns: Id
%% @end
-spec id_from_path(tuple()) -> string().
id_from_path(RD) ->
	case wrq:path_info(resourceid, RD) of
		undefined->
			["resource", Id] = string:tokens(wrq:disp_path(RD), "/"),
			Id;
		Id -> Id
	end.

%% @doc
%% Function: filter_json/2
%% Purpose: Used to add private and resource filters to the json query
%% Returns: JSON string that is updated with filter
%% @end
filter_json(Json,UserQuery) ->
	NewJson = string:sub_string(Json,1,string:len(Json)-1),
	"{\"query\":{\"filtered\":"++NewJson++",\"filter\":{\"bool\":{\"must\":[{\"term\":{"++UserQuery++"}}]}}}}}".


%% @doc
%% Function: filter_json/3
%% Purpose: Used to add private and User filters to the json query with pagination
%% Returns: JSON string that is updated with filter and the from size parameters
%% @end
filter_json(Json, UserQuery, From, Size) ->
        NewJson = string:sub_string(Json,1,string:len(Json)-1),
        "{\"from\" : "++From++", \"size\" : "++Size++", \"query\":{\"filtered\":"++NewJson++",\"filter\":{\"bool\":{\"must\":[{\"term\":{"++UserQuery++"}}]}}}}}".



%% @doc
%% Function: generate_date/2
%% Purpose: Used to create a date valid in ES
%% from the input which should be the list
%% [Year,Mounth,Day]
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

		
%% @doc
%% Function: do_any_field_exist/2
%% Purpose: Used to check if a JSON contains any of the given fields
%% Returns: True if at least one of the given fields exist, false otherwise
%% @end
-spec do_any_field_exist(Json::string(),FieldList::list()) -> boolean().

do_any_field_exist(_Json,[]) ->
		false;
do_any_field_exist(Json,[First|Rest]) ->
		case lib_json:get_field(Json, First) of
			undefined ->
				do_any_field_exist(Json,Rest);
			_ ->
				true
		end.

