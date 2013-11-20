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
-include("field_restrictions.hrl").

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
    case delete_streams_with_user_id(Id) of
        {error, {Code, Body}} -> 
            ErrorString = api_help:generate_error(Body, Code),
            {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
        {ok} ->
            case erlastic_search:delete_doc(?INDEX,"user", Id) of
                {error, {Code, Body}} -> 
                    ErrorString = api_help:generate_error(Body, Code),
                    {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
                {ok,List} -> 
                    {true,wrq:set_resp_body(lib_json:encode(List),ReqData),State}
            end
    end.



%% @doc
%% Function: delete_streams_with_user_id/1
%% Purpose: Deletes the streams with the given user id
%% Returns:  ERROR = {error,Errorcode}
%%			 OK = {ok}
%% @end
-spec delete_streams_with_user_id(Id::string()) -> term().

delete_streams_with_user_id(Id) ->
	api_help:refresh(),
	Query = "user_id:" ++ Id, 
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
%% Function: put_user/2
%% Purpose: Replaces a User in the database with the new info.
%% It is run automatically for PUT requests
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%%
%% Side effects: Updates the User in the database
%% @end
-spec put_user(ReqData::tuple(), State::string()) -> {true, tuple(), string()}.
put_user(ReqData, State) ->
    Id = id_from_path(ReqData),
	{UserJson,_,_} = api_help:json_handler(ReqData, State),
    %check if doc already exists
	case api_help:do_only_fields_exist(UserJson,?ACCEPTEDFIELDSUSERS) of
		false ->
			{{halt,403}, wrq:set_resp_body("Unsupported field(s)", ReqData), State};
		true ->
			case erlastic_search:get_doc(?INDEX, "user", Id) of
				{error, {Code, Body}} -> 
					ErrorString = api_help:generate_error(Body, Code),
					{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
				{ok, List} ->
            		case erlastic_search:index_doc_with_id(?INDEX, "user", Id, UserJson) of
                		{error, {Code, Body}} -> 
                    		ErrorString = api_help:generate_error(Body, Code),
                    		{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
                		{ok, Json} ->
		            		{true, wrq:set_resp_body(lib_json:encode(Json), ReqData), State}
            		end
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
			case api_help:do_only_fields_exist(UserJson,?ACCEPTEDFIELDSUSERS) of
				false ->
					{{halt,403}, wrq:set_resp_body("Unsupported field(s)", ReqData), State};
				true ->
					case erlastic_search:index_doc(?INDEX, "user", UserJson) of
						{error, {Code, Body}} -> 
							ErrorString = api_help:generate_error(Body, Code),
                    		{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
                		{ok,List} -> 
                    		{true, wrq:set_resp_body(lib_json:encode(List), ReqData), State}
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
                    case wrq:get_qs_value("size",ReqData) of 
                        undefined ->
                            Size = 100;
                        SizeParam ->
                            Size = list_to_integer(SizeParam)
                    end,
                    case erlastic_search:search_limit(?INDEX,"user","*:*",Size) of
                        {error, {Code, Body}} -> 
                            ErrorString = api_help:generate_error(Body, Code),
                            {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
					    {ok,JsonStruct} ->
						    FinalJson = lib_json:get_list_and_add_id(JsonStruct, users),
						    {FinalJson, ReqData, State}  
                    end;
                Id ->
				    %% Get specific user
                    case erlastic_search:get_doc(?INDEX, "user", Id) of
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
        case erlastic_search:search_limit(?INDEX, "user", Query, 10) of
            {error, {Code, Body}} -> 
                ErrorString = api_help:generate_error(Body, Code),
                {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
            {ok,List} -> 
                {true, wrq:set_resp_body(lib_json:encode(List),ReqData),State}
        end;
process_search(ReqData, State, get) ->
        TempQuery = wrq:req_qs(ReqData),
        TransformedQuery = api_help:transform(TempQuery),
        case erlastic_search:search_limit(?INDEX, "user", TransformedQuery, 10) of
            {error, {Code, Body}} -> 
                ErrorString = api_help:generate_error(Body, Code),
                {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
            {ok,List} -> 
                {lib_json:encode(List),ReqData,State} % May need to convert
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
                ["users", Id] -> Id;
                _ -> undefined
            end;
        Id -> Id
    end.
