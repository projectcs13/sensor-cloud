%%%-------------------------------------------------------------------
%%% @author ProjectCS13 <> Andreas, Carl
%%% @doc
%%% This module handles the uri requests that are related to resources.
%%% @end
%%% Created : 9 Oct 2013 by ProjectCS13 <>
%%%-------------------------------------------------------------------
-module(resource).
-compile(export_all).

-include_lib("webmachine.hrl").
-include_lib("erlastic_search.hrl").

-define(INDEX, "sensorcloud").

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
		    {['POST','GET'], ReqData, State}
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
	erlang:display("DELETE request - check permission here"),
	%% TODO Authentication
	case erlastic_search:get_doc(?INDEX, "resource", Id,  [{"fields", "_source.streams"}]) of
			{ok,{struct,Json}} ->
				case proplists:get_value(<<"fields">>, Json) of
					undefined -> erlang:display("NO STREAMS");
					{struct,Fields} ->
						Streams = proplists:get_value(<<"_source.streams">>, Fields),
						case delete_streams(Streams) of
							{{error,_Reason}, StreamId, _Rest} -> erlang:display("failed to delete stream " ++ StreamId);
							ok -> erlang:display("streams have been deleted")
						end
				end;
			_ -> erlang:display("Search error")
	end,
	case erlastic_search:delete_doc(?INDEX,"resource", Id) of
			{error,_} -> {{halt,404}, ReqData, State};
			{ok,List} -> {true,wrq:set_resp_body(api_help:json_encode(List),ReqData),State}
	end.

%% @doc
%% Function: delete_streams/1
%% Purpose: Deletes all streams in the given list, the list elements are streamIds as binaries
%% Returns:  ok, or {{error,_Reason}, StreamId, Rest} where StreamId is the binary Id of the stream for which deletion failed
%% @end
delete_streams([]) -> ok;
delete_streams([StreamId|Rest]) ->
	case erlastic_search:delete_doc(?INDEX, "stream", binary_to_list(StreamId)) of 
		{error,Reason} -> {{error,Reason},StreamId, Rest};
		{ok,_List} -> delete_streams(Rest)
	end;
delete_streams(StreamId) ->
	case erlastic_search:delete_doc(?INDEX, "stream", StreamId) of 
		{error,Reason} -> {{error,Reason},StreamId, []};
		{ok,_List} -> ok
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
			case erlastic_search:index_doc(?INDEX,"resource",Resource) of 
				{error, Reason} -> {false, wrq:set_resp_body(api_help:json_encode(Reason), ReqData), State};
				{ok,List} -> 	{true, wrq:set_resp_body(api_help:json_encode(List), ReqData), State}
			end;
		true ->
			% Search
			URIQuery = wrq:req_qs(ReqData),
			case proplists:get_value('userid', wrq:path_info(ReqData)) of
				undefined ->
					Query = [];
				UserId ->
					Query = "owner:" ++ UserId
			end,
			FullQuery = lists:append(api_help:transform(URIQuery,true),Query),
			erlang:display(FullQuery),
			case erlastic_search:search_limit(?INDEX, "resource", FullQuery,10) of % Maybe wanna take more
				{error,Reason} -> {{halt,Reason}, ReqData, State};
				{ok,List} -> Json = api_help:make_to_string(api_help:json_encode(List)),
                             Id = api_help:get_id_value(Json,"_id"),
                             NewJson = "{\"id\" : \"" ++ Id ++ "\"}",
                             Update = api_help:create_update(NewJson),
                             case api_help:update_doc(?INDEX,"resource", Id, Update, []) of
								 {error, Reason} -> {false, wrq:set_resp_body(api_help:json_encode(Reason), ReqData), State};
                                 {ok,_} ->{true, wrq:set_resp_body(api_help:json_encode(List), ReqData), State}
                             end
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
	case id_from_path(ReqData) of
		undefined -> {{halt, 400}, ReqData, State};
		
		Id ->	
			%check if doc already exists
			case erlastic_search:get_doc(?INDEX, "resource", Id) of
				{error, _} ->
					{{halt, 404}, ReqData, State};
				{ok, _} ->
					{UserJson,_,_} = api_help:json_handler(ReqData, State),
					case api_help:update_doc(?INDEX,"resource", Id, UserJson, []) of 
						{error, Reason} -> 
							{{halt,Reason}, ReqData, State};
						{ok,List} ->
							{true,wrq:set_resp_body(api_help:json_encode(List),ReqData),State}
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
	erlang:display("GET request"),
	case api_help:is_search(ReqData) of
		false ->
			case proplists:get_value('resourceid', wrq:path_info(ReqData)) of
				undefined ->
				% List resources based on URI
					case proplists:get_value('userid', wrq:path_info(ReqData)) of
						undefined ->
							Query = [];
						UserId ->
							Query = "owner:" ++ UserId
					end,
					case erlastic_search:search_limit(?INDEX, "resource", Query, 100) of % Maybe wanna take more
						{error,Reason} -> {{halt, Reason}, ReqData, State};
						{ok,List} -> SearchRemoved = api_help:remove_search_part(api_help:make_to_string(api_help:json_encode(List)),false,0),
                                     ExtraRemoved = api_help:remove_extra_info(SearchRemoved,0),
                                     {ExtraRemoved, ReqData, State}
					end;
				ResourceId ->
				% Get specific resource
					case erlastic_search:get_doc(?INDEX, "resource", ResourceId) of 
						{error,_Msg} -> 
								{{halt, 404}, ReqData, State};
						{ok,List} -> 
							     {api_help:json_encode(List), ReqData, State}
					end
		end;
		true ->
			erlang:display("Processing _search"),
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
			{error,Reason} -> {{error,Reason}, ReqData, State};
			{ok,List} -> {true, wrq:set_resp_body(api_help:json_encode(List),ReqData),State}
		end;
process_search(ReqData, State, get) ->
		TempQuery = wrq:req_qs(ReqData),
		TransformedQuery = api_help:transform(TempQuery),
		erlang:display("Query "++TransformedQuery),
		case erlastic_search:search_limit(?INDEX, "resource", TransformedQuery, 10) of
			{error,Reason} -> {{error,Reason}, ReqData, State};
			{ok,List} -> {api_help:json_encode(List),ReqData,State} % May need to convert
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

