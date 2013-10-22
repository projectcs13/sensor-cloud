%% @author Georgios Koutsoumpakis
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]

%% @doc Webmachine_resource for /users

-module(users).
-export([init/1,
                allowed_methods/2,
                content_types_accepted/2,
                content_types_provided/2,
                delete_resource/2,
                put_user/2,
                get_user/2,
                process_post/2]).

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
                [{"users","_search"}] ->
                        {['POST','GET'], ReqData, State};
                [{"users",_Id}] ->
                        {['GET', 'PUT', 'DELETE'], ReqData, State};
                [{"users"}] ->
                        {['POST','GET'], ReqData, State};
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
        {[{"application/json", get_user}], ReqData, State}.


%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is allowed to be sent to the server.
%% A code 406 is returned to the client if we don't accept a media type that the client has sent.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_accepted(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
content_types_accepted(ReqData, State) ->
        {[{"application/json", put_user}], ReqData, State}.


%% @doc
%% Function: delete_resource/2
%% Purpose: Works but need to fix transformation of the return value
%% Returns: {JSON-object(string), ReqData, State}
%%
%% Side effects: Deletes the User for the database
%% @end
-spec delete_resource(ReqData::tuple(), State::string()) -> {string(), tuple(), string()}.
delete_resource(ReqData, State) ->
        Id = id_from_path(ReqData),
        case erlastic_search:delete_doc(?INDEX,"user", Id) of
                {error, not_found} -> {{halt,404}, ReqData, State};
                {error, _} -> {{halt,400}, ReqData, State};                
                {ok, _} -> {true, ReqData, State}
        end.


%% @doc
%% Function: put_user/2
%% Purpose: Replaces a User in the database with the new info.
%% It is run automatically for PUT requests
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%%
%% Side effects: Updates the User in the database
%% @end
-spec put_user(ReqData::tuple(), State::string()) -> {true, tuple(), string()}.
put_user(ReqData, State) ->
        case id_from_path(ReqData) of
                undefined -> {{halt, 400}, ReqData, State};
                Id ->        
                        %check if doc already exists
                        case erlastic_search:get_doc(?INDEX, "user", Id) of
                                {error, _} ->
                                        {{halt, 404}, ReqData, State};
                                {ok, _} ->
                                        {UserJson,_,_} = api_help:json_handler(ReqData, State),
                                        erlastic_search:index_doc_with_id(?INDEX, "user", Id, UserJson),
                                        {true, ReqData, State}
                        end
        end.


%% @doc
%% Function: process_post/2
%% Purpose: decodes a JSON object and either adds the new User in the DB or
%% performs search in the User database.
%% It is run automatically for POST requests
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%%
%% Side effects: Inserts a new User in the database (when for insertion)
%% @end
-spec process_post(ReqData::tuple(), State::string()) -> {true, tuple(), string()}.
process_post(ReqData, State) ->
        case api_help:is_search(ReqData) of
                false ->
                        {UserJson,_,_} = api_help:json_handler(ReqData, State),
                        case erlastic_search:index_doc(?INDEX, "user", UserJson) of
                                {error, Reason} -> {{error, Reason}, ReqData, State};
                                {ok,List} -> Json = api_help:make_to_string(lib_json:encode(List)),
                   							 Id = api_help:get_id_value(Json,"_id"),
                                    		 NewJson = "{\"id\" : \"" ++ Id ++ "\"}",
                                     		 Update = api_help:create_update(NewJson),
                                     		 case api_help:update_doc(?INDEX,"user", Id, Update, []) of
												 {error, Reason} -> {false, wrq:set_resp_body(lib_json:encode(Reason), ReqData), State};
                                        		 {ok,_} ->{true, wrq:set_resp_body(lib_json:encode(List), ReqData), State}
                                     		 end
                        end;
                true ->
                        process_search(ReqData,State, post)                        
        end.


%% @doc
%% Function: get_user/2
%% Purpose: Returns the JSON representation of a json-object or multiple json-objects.
%% Returns: {true, ReqData, State} | {false, ReqData, State}
%% @end
-spec get_user(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
get_user(ReqData, State) ->
        case api_help:is_search(ReqData) of
                false ->
                        case id_from_path(ReqData) of
                                undefined ->
                                        % Get all users
                                        case erlastic_search:search_limit(?INDEX,"user","*:*",2000) of
                                                {ok, Result} ->
                                                        SearchRemoved = api_help:remove_search_part(api_help:make_to_string(lib_json:encode(Result)),false,0),
														ExtraRemoved = api_help:remove_extra_info(SearchRemoved,0),
                                     					{ExtraRemoved, ReqData, State}; 
                                                _ -> {{halt, 404}, ReqData, State}
                                        end;
                                Id ->
                                        % Get specific user
                                        case erlastic_search:get_doc(?INDEX, "user", Id) of
                                                {error, _} ->
                                                        {{halt, 404}, ReqData, State};
                                                {ok,List} ->
                                                        ExtraRemoved = api_help:remove_extra_info(api_help:make_to_string(lib_json:encode(List)),0),
														{ExtraRemoved, ReqData, State}
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
        case erlastic_search:search_limit(?INDEX, "user", Query, 10) of
                {error,Reason} -> {{error,Reason}, ReqData, State};
                {ok,List} -> {true, wrq:set_resp_body(lib_json:encode(List),ReqData),State}
        end;
process_search(ReqData, State, get) ->
        TempQuery = wrq:req_qs(ReqData),
        TransformedQuery = api_help:transform(TempQuery),
        case erlastic_search:search_limit(?INDEX, "user", TransformedQuery, 10) of
                {error,Reason} -> {{error,Reason}, ReqData, State};
                {ok,List} -> {lib_json:encode(List),ReqData,State} % May need to convert
        end.





%% @doc
%% Function: id_from_path/2
%% Purpose: Retrieves the if from the path.
%% Returns: Id
%% @end
-spec id_from_path(string()) -> string().
id_from_path(RD) ->
        case wrq:path_info(id, RD) of
                undefined ->
                        case string:tokens(wrq:disp_path(RD), "/") of
                                ["users", Id] -> Id;
                                _ -> undefined
                        end;
                Id -> Id
        end.



