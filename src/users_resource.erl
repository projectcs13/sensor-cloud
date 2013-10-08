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
		 from_json/2,
		 get_resource/2,
		 post_is_create/2,
		 allow_missing_post/2,
		 create_path/2]).

-include("webmachine.hrl").
-include("user.hrl").

%% @doc
%% Function: init/1
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end
-spec init([]) -> {ok, undefined}.
init([]) -> {ok, undefined}.


%% @doc
%% Function: post_is_create/2
%% Purpose: Webmachine on a post request should run create_path first. This
%% means that a POST is handled as a PUT. Ex: POST to /users is a
%% PUT to /users/5
%% Returns: {true, undefined}
%% @end
post_is_create(ReqData, State) -> {true, ReqData, State}.


%% @doc
%% Function: create_path/2
%% Purpose: Creates an ID for the new user and then inserts an empty one
%% in the database
%% Returns: {Path, _, _ }
%% @end
create_path(RD, Ctx) ->
    Path = "/users/" ++ integer_to_list(Id=db_api:generate_id(user)),
	db_api:create_user(#user{id= Id}),
    {Path, RD, Ctx}.


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
		[{"users",_}] ->
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
	{[{"application/json", get_resource}], ReqData, State}.


%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is allowed to be sent to the server.
%%          A code 406 is returned to the client if we don't accept a media type that the client has sent. 
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
content_types_accepted(ReqData, State) ->
	{[{"application/json", from_json}], ReqData, State}.



%% DELETE
delete_resource(ReqData, State) ->
	Id = list_to_integer(id_from_path(ReqData)),
	case db_api:delete_user_with_id(Id) of
		ok -> {true, ReqData, State};
		{error,_} -> {{halt, 404}, ReqData, State};
		{abort,_} -> {{halt, 404}, ReqData, State}
	end.


%% @doc
%% Function: from_json/2
%% Purpose: decodes a JSON object and returns a record representation of this.
%% It is run automatically for POST and PUT requests
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%% @end
from_json(ReqData, State) ->
	Id = id_from_path(ReqData),
	case get_user_from_request(ReqData) of
		{error, _} -> {{halt, 400}, ReqData, State};
		{ok, User} ->
			case db_api:get_user_by_id(list_to_integer(Id)) of
				{aborted, Reason} -> {{error, Reason}, ReqData, State};
				{error, Reason} -> {{error, Reason}, ReqData, State};
				_ -> db_api:update_user(list_to_integer(Id), User),					 
					{true, ReqData, State}
			end
	end.


%% @doc
%% Function: get_user_from_request/2
%% Purpose: Creates a User record in the request
%% Returns: {ok, User} || {error, "empty body"}
%% @end
get_user_from_request(ReqData) ->
	[{Value,_ }] = mochiweb_util:parse_qs(wrq:req_body(ReqData)), 
	case Value of
		[] -> 
	erlang:display("empty"),
			{error, "empty body"};
		_ ->
			{struct, JsonData} = mochijson2:decode(Value),
			User = json_to_user(JsonData),
			{ok, User}
	end.


%% @doc
%% Function: get_resource/2
%% Purpose: Returns the JSON representation of a json-object or multiple json-objects. 
%%  		Fault tolerance is handled by resources_exists/2.
%% Returns: {true, ReqData, State} | {false, ReqData, State}
%% @end

get_resource(ReqData, State) ->
	case proplists:get_value('id', wrq:path_info(ReqData)) of
		undefined -> 
			% Get all users
			Users = lists:map(fun(X) -> user_to_json(X) end, db_api:get_all_users()),
			{Users, ReqData, State};
		X -> 
			% Get specific user
			case User = db_api:get_user_by_id(list_to_integer(X)) of
				{error, "unknown_user"} -> {{halt, 404}, ReqData, State};
		 		_ -> {user_to_json(User), ReqData, State}
			end
	end.


%% @doc
%% Function: json_to_user/1
%% Purpose: Given a proplist, the return value will be a 'user' record with the values taken from the proplist.
%% Returns: user::record()
%% @end
-spec json_to_user(string()) -> Record :: #user{}.
json_to_user(JsonData) ->
	#user{id = proplists:get_value(<<"id">>, JsonData),
		email = proplists:get_value(<<"email">>, JsonData), 
		user_name = proplists:get_value(<<"user_name">>, JsonData),
		password = proplists:get_value(<<"password">>, JsonData), 
		first_name = proplists:get_value(<<"first_name">>, JsonData), 
		last_name = proplists:get_value(<<"last_name">>, JsonData), 
		description = proplists:get_value(<<"description">>, JsonData),
		latitude = proplists:get_value(<<"latitude">>, JsonData), 
		longitude = proplists:get_value(<<"longitude">>, JsonData), 
		creation_date = proplists:get_value(<<"creation_date">>, JsonData),
		last_login = proplists:get_value(<<"last_login">>, JsonData)
	}.


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
%% Function: user_to_json/1
%% Purpose: decodes a record 'user' to a JSON object and returns it.
%% Returns: obj :: JSON()
%% @end

-spec user_to_json( Record :: #user{}) -> string().
user_to_json(Record) ->
  [_ | Values] = tuple_to_list(Record),
  Keys = [<<"id">>, <<"email">>, <<"user_name">>, <<"password">>, 
		  <<"first_name">>, <<"last_name">>, <<"description">>,
		  <<"latitude">>, <<"longitude">>, <<"creation_date">>,
		  <<"last_login">>],
  P_list = merge_lists(Keys, Values),
  mochijson2:encode({struct, P_list}).

%% @doc
%% Function: merge_lists/2
%% Purpose: helper function to user_to_json/1, given a list of keys and a list of values, this function
%%			will create a list [{Key, Value}], if a value is undefined, it will remove the value and the key 
%% 			that it corresponds, both lists are assumed to be of equal length.
%% Returns: [{Key, Value}] | []
%% @end

%% PRE-COND: Assumes that both lists are of equal size.
merge_lists([], []) -> [];
merge_lists([H|T], [A|B]) ->
	case A of
		undefined -> merge_lists(T,B);
		_ -> [{H,A}]++merge_lists(T,B)
	end.


%% @doc
%% Function: id_from_path/2
%% Purpose: Retrieves the if from the path.
%% Returns: Id
%% @end
-spec id_from_path(string()) -> string().
id_from_path(RD) ->
    case wrq:path_info(id, RD) of
        undefined->
            ["users", Id] = string:tokens(wrq:disp_path(RD), "/"),
            Id;
        Id -> Id
    end.


%% To-do : HTTP Caching support w etags / header expiration.


	
