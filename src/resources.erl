%%%-------------------------------------------------------------------
%%% @author ProjectCS13 <> Andreas, Carl, Tomas
%%% @doc
%%% This module handles the uri requests that are related to resources.
%%% @end
%%% Created : 9 Oct 2013 by ProjectCS13 <>
%%%-------------------------------------------------------------------
-module(resources).
-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2,
		 delete_resource/2, process_post/2, add_suggested_stream/1, put_resource/2, get_resource/2]).

-include_lib("webmachine.hrl").
-include_lib("erlastic_search.hrl").
-include("field_restrictions.hrl").
-include("json.hrl").

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
		[{"resources"}] ->
			{['POST','GET'], ReqData, State};
		[{"resources", "_search" ++ _Query}] ->
			{['GET', 'POST'], ReqData, State};
		[{"resources", _ResourceID}] ->
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
	case openidc:auth_request(ReqData) of
        {error, Msg} -> {{halt, 498}, wrq:set_resp_body(Msg, ReqData), State};
        {ok, _} ->
			Id = proplists:get_value('resourceid', wrq:path_info(ReqData)),
			case erlastic_search:delete_doc(?INDEX,"resource", Id) of
				{error, {Code, Body}} ->
					ErrorString = api_help:generate_error(Body, Code),
		            {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
				{ok,List} ->
					{true,wrq:set_resp_body(lib_json:encode(List),ReqData),State}
			end
	end.

%% @doc
%% Function: process_post/2
%% Purpose: Handle POST request, only working for create and not search - AS OF SPRINT 3
%% Returns:  {JSON-object(string), ReqData, State}
%% @end
-spec process_post(ReqData::tuple(), State::string()) -> {atom(), tuple(), string()}.
process_post(ReqData, State) ->
	case openidc:auth_request(ReqData) of
        {error, Msg} -> {{halt, 498}, wrq:set_resp_body(Msg, ReqData), State};
        {ok, _} ->
			URIList = string:tokens(wrq:path(ReqData), "/"),
			IsSearch = (string:sub_string(lists:nth(length(URIList),URIList),1,7) == "_search"),
			case IsSearch of
				false ->
					% Create
					{Resource,_,_} = api_help:json_handler(ReqData,State),
					case {api_help:do_any_field_exist(Resource,?RESTRICTED_RESOURCES_CREATE),api_help:do_only_fields_exist(Resource,?ACCEPTED_RESOURCES_FIELDS)} of
						{true,_} ->
							ResFields1 = lists:foldl(fun(X, Acc) -> X ++ ", " ++ Acc end, "", ?RESTRICTED_RESOURCES_CREATE),
							ResFields2 = string:sub_string(ResFields1, 1, length(ResFields1)-2),
							{{halt,409}, wrq:set_resp_body("{\"error\":\"Error caused by restricted field in document, these fields are restricted : " ++ ResFields2 ++"\"}", ReqData), State};
						{false,false} ->
							{{halt,403}, wrq:set_resp_body("Unsupported field(s)", ReqData), State};
						{false,true} ->
							FieldsAdded = add_server_side_fields(Resource),

							%% FinalResource = suggest:add_resource_suggestion_fields(Resource),
							case erlastic_search:index_doc(?INDEX,"resource",FieldsAdded) of
								{error, {Code, Body}} ->
									ErrorString = api_help:generate_error(Body, Code),
									{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
								{ok, Json} ->
									ResourceId = lib_json:get_field(Json, "_id"),
									suggest:add_suggestion(FieldsAdded, ResourceId),
									{true, wrq:set_resp_body(lib_json:encode(Json), ReqData), State}
							end
					end;
				true ->
					% Search
					process_search_post(ReqData,State)
			end
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
	%FilteredJson = filter_json(Json, "*", From, Size),
	case erlastic_search:search_json(#erls_params{},?INDEX, "resource", Json) of % Maybe wanna take more
		{error, {Code, Body}} ->
            ErrorString = api_help:generate_error(Body, Code),
            {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
		{ok,List} ->
			{true,wrq:set_resp_body(lib_json:encode(List),ReqData),State} % May need to convert
	end.


%% @doc
%% Function: put_resource/2
%% Purpose: Updates the resource in the database
%% It is run automatically for POST and PUT requests
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%% @end
-spec put_resource(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
put_resource(ReqData, State) ->
	case openidc:auth_request(ReqData) of
        {error, Msg} -> {{halt, 498}, wrq:set_resp_body(Msg, ReqData), State};
        {ok, _} ->
			%check if doc already exists
			Id = id_from_path(ReqData),
			case erlastic_search:get_doc(?INDEX, "resource", Id) of
				{error, {Code, Body}} ->
		            ErrorString = api_help:generate_error(Body, Code),
		            {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
				{ok, _} ->
					{UserJson,_,_} = api_help:json_handler(ReqData, State),
					case {api_help:do_any_field_exist(UserJson,?RESTRICTED_RESOURCES_UPDATE),api_help:do_only_fields_exist(UserJson,?ACCEPTED_RESOURCES_FIELDS)} of
						{true,_} ->
							ResFields1 = lists:foldl(fun(X, Acc) -> X ++ ", " ++ Acc end, "", ?RESTRICTED_RESOURCES_UPDATE),
							ResFields2 = string:sub_string(ResFields1, 1, length(ResFields1)-2),
							{{halt,409}, wrq:set_resp_body("{\"error\":\"Error caused by restricted field in document, these fields are restricted : " ++ ResFields2 ++"\"}", ReqData), State};
						{false,false} ->
							{{halt,403}, wrq:set_resp_body("Unsupported field(s)", ReqData), State};
						{false,true} ->
							NewJson = suggest:add_resource_suggestion_fields(UserJson),
							Update = lib_json:set_attr(doc, NewJson),
							case api_help:update_doc(?INDEX,"resource", Id, Update) of
								{error, {Code, Body}} ->
									ErrorString = api_help:generate_error(Body, Code),
									{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
								{ok,List} ->
									suggest:update_resource(UserJson, Id),
									{true,wrq:set_resp_body(lib_json:encode(List),ReqData),State}
							end
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
	case openidc:auth_request(ReqData) of
        {error, Msg} -> {{halt, 498}, wrq:set_resp_body(Msg, ReqData), State};
        {ok, _} ->
			case api_help:is_search(ReqData) of
				false ->
					case proplists:get_value('resourceid', wrq:path_info(ReqData)) of
						undefined ->
							% List resources based on URI
						    case wrq:get_qs_value("size",ReqData) of
					            undefined ->
									case erlastic_search:count_type(?INDEX, "resource") of
										{error, {_CountCode, _CountBody}} ->
											Size = 100;
										{ok,CountJsonStruct} ->
											Size = lib_json:get_field(CountJsonStruct,"count")
									end;
					            SizeParam ->
					                Size = list_to_integer(SizeParam)
					        end,
							case erlastic_search:search_limit(?INDEX, "resource", "*", Size) of % Maybe wanna take more
								{error, {Code, Body}} ->
		            				ErrorString = api_help:generate_error(Body, Code),
		            				{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
								{ok,JsonStruct} ->
									FinalJson = api_help:get_list_and_add_id(JsonStruct, resources),
									{FinalJson, ReqData, State}
							end;
						ResourceId ->
							% Get specific resource
							case erlastic_search:get_doc(?INDEX, "resource", ResourceId) of
								{error, {Code, Body}} ->
		            				ErrorString = api_help:generate_error(Body, Code),
		            				{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
								{ok,JsonStruct} ->
									FinalJson = api_help:get_and_add_id(JsonStruct),
									{FinalJson, ReqData, State}
							end
					end;
				true ->
					process_search(ReqData,State, get)
			end
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
%% Function: add_suggested_stream/2
%% Purpose: Add a new suggested stream to the array of suggested streams
%% Returns:  {JSON-object(string), ReqData, State}
%% @end
-spec add_suggested_stream(Stream::json()) -> ok |  {error, no_model}.
add_suggested_stream(Stream) ->
	ResourceId = lib_json:get_field(Stream, "resource.resource_type"),
	case erlastic_search:get_doc(?INDEX, "resource",lib_json:to_string(ResourceId)) of
		{error, {Code, Body}} ->
			ErrorString = api_help:generate_error(Body, Code),
			{error, ErrorString};

		{ok,JsonStruct} ->
			FinalJson = api_help:get_and_add_id(JsonStruct),
			case find_stream_type(lib_json:get_field(Stream, "type"),lib_json:get_field(FinalJson,"streams_suggest")) of
				false ->
			%%		Sugg = lib_json:get_field(FinalJson, "hits[0]._source"),
					FilteredStream = lib_json:rm_field(lib_json:rm_field(lib_json:rm_field(lib_json:rm_field(lib_json:rm_field(
														Stream,"location"),"resource"),"private"),"uri"),"user_id"),
					NewSuggestedStream = lib_json:add_value(FinalJson,"streams_suggest" , FilteredStream ),
					FinalSuggested = lib_json:rm_field(NewSuggestedStream, "id"),
					Final = lib_json:set_attr(doc, FinalSuggested),
					case api_help:update_doc(?INDEX, "resource", lib_json:to_string(ResourceId), Final) of
						{error,{Code,Body}} ->
							ErrorString = api_help:generate_error(Body, Code),
							{error, ErrorString};
						{ok, _Json} -> ok
					end;
				true ->
					{error, "Existing type"}
			end
	end.



%% @doc
%% Function: find_stream_type/1
%% Purpose: Used to find a type value in a list of suggested streams(list of JSON objects)
%% based on a given type
%%
%% Returns: If the type was found in the list
%% @end
find_stream_type(Type, StreamsSuggest) when is_list(Type) ->
	find_stream_type(binary:list_to_bin(Type), StreamsSuggest);
find_stream_type(Type, []) ->
	false;
find_stream_type(Type, [Head|Rest]) ->
	erlang:display(Head),
	case lib_json:get_field(Head, "type") of
		Type ->
			true;
		_ ->
			find_stream_type(Type, Rest)
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
%% Function: add_server_side_fields/1
%% Purpose: Used to add all the fields that should be added server side
%% Returns: The new json with the fields added
%% @end
-spec add_server_side_fields(Json::string()) -> string().

add_server_side_fields(Json) ->
	JSON = lib_json:add_values(Json,[
			{streams_suggest, "[]"}
			]),
	JSON.



