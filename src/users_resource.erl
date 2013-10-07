%% @author Georgios Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]

%% @doc Webmachine_resource for /users

-module(users_resource).
-export([init/1, 
		 allowed_methods/2,
		 content_types_accepted/2,
		 content_types_provided/2,
		 delete_resource/2,
		 put_user/2,
		 get_user/2,
		 post_is_create/2,
		 allow_missing_post/2,
		 create_path/2]).

-include("webmachine.hrl").
-include("user.hrl").

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
%% Function: post_is_create/2
%% Purpose: Webmachine on a post request should run create_path first. This
%% means that a POST is handled as a PUT. Ex: POST to /users is a
%% PUT to /users/5
%% Returns: {true, undefined}
%% @end
post_is_create(ReqData, State) -> {true, ReqData, State}.


%% @doc
%% Function: allow_missing_post/2
%% Purpose: If the resource accepts POST requests to nonexistent resources, then this should return true.
%% Returns: {true, ReqData, State}
%% @end
allow_missing_post(ReqData, State) ->
	{true, ReqData, State}.


%% @doc
%% Function: allowed_methods/2
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end

allowed_methods(ReqData, State) ->
	case parse_path(wrq:path(ReqData)) of
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
%%          A code 406 is returned to the client if we cannot return the media-type that the user has requested. 
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
content_types_provided(ReqData, State) ->
	{[{"application/json", get_user}], ReqData, State}.


%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is allowed to be sent to the server.
%%          A code 406 is returned to the client if we don't accept a media type that the client has sent. 
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
content_types_accepted(ReqData, State) ->
	{[{"application/json", put_user}], ReqData, State}.



%% DELETE
delete_resource(ReqData, State) ->
	Id = list_to_integer(id_from_path(ReqData)),
	case erlastic_search:delete_doc(?INDEX,"user", Id) of
		{error, not_found} -> {{halt,404}, ReqData, State};
		{error, _} -> {{halt,400}, ReqData, State};		
		{ok, _} -> {true, ReqData, State}
	end.



%% @doc
%% Function: create_path/2
%% Purpose: Creates an ID for the new user and then inserts an empty one
%% in the database. Run automatically on a POST
%% Returns: {Path, _, _ }
%% @end
create_path(ReqData, State) ->
    Path = "/users/" ++ integer_to_list(Id=db_api:generate_id(user)),
	erlastic_search:index_doc_with_id(?INDEX, "user", Id, "{\"user_name\" : \"temp\"}"),
    {Path, ReqData, State}.



%% @doc
%% Function: put_user/2
%% Purpose: decodes a JSON object and returns a record representation of this.
%% It is run automatically for POST and PUT requests
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%% @end
put_user(ReqData, State) ->
	case id_from_path(ReqData) of
		undefined -> {{halt, 400}, ReqData, State};
		Id ->	
			%check if doc already exists
			case erlastic_search:get_doc(?INDEX, "user", Id) of 
				{error, _} -> 
					{{halt, 404}, ReqData, State};
				{ok, _} -> 
					{UserJson,_,_} = json_handler(ReqData, State),
					erlastic_search:index_doc_with_id(?INDEX, "user", Id, UserJson),
					{true, ReqData, State}
			end
	end.



%% @doc
%% Function: get_user/2
%% Purpose: Returns the JSON representation of a json-object or multiple json-objects. 
%%  		Fault tolerance is handled by resources_exists/2.
%% Returns: {true, ReqData, State} | {false, ReqData, State}
%% @end
get_user(ReqData, State) ->
	case id_from_path(ReqData) of
		undefined -> 
			% Get all users
			case erlastic_search:search(?INDEX,"user","*:*") of
				{ok, Result} -> 
					{json_encode(Result), ReqData, State};
				_ -> {{halt, 404}, ReqData, State}
			end;
		Id -> 
			% Get specific user
			case erlastic_search:get_doc(?INDEX, "user", Id) of 
				{error, _} -> 
					{{halt, 404}, ReqData, State};
				{ok,{struct, JsonData}} -> 
					User = proplists:get_value(<<"_source">>, JsonData),
					{json_encode(User), ReqData, State}
			end
	end.

json_handler(ReqData, State) ->
	[{Value,_ }] = mochiweb_util:parse_qs(wrq:req_body(ReqData)), 
	{Value, ReqData, State}.



%% @doc
%% Function: parse_path/1
%% Purpose: Given a string representation of a search path, the path is split by the '/' token
%%			and the return value is a list of tuples [{dir, id}].
%% Returns: [{"directory_name", "id_value"}] | [{Error, Err}] | []
%% @end
-spec parse_path(string()) -> string().
parse_path(Path) -> 
	[_|T] = filename:split(Path),
	pair(T).

pair([]) -> [];
pair([A]) -> [{A}];
pair([A,B|T]) ->
	case string:to_integer(B) of
		{V, []} -> [{A,V}|pair(T)];
		{error, no_integer} -> [error]
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


json_encode(Data) ->
    (mochijson2:encoder([{utf8, true}]))(Data).
