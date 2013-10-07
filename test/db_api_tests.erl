%% @author Tholsgård Gabriel
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == db_api_tests ==
%% This module contains several tests to test the functionallity
%% in the module dp_api.
%%
%% @end

-module(db_api_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("user.hrl").
-include("resource.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% ====================================================================
%% Internal functions
%% ====================================================================


%% @doc
%% Function: start_test/0
%% Purpose: Test to start a db_api process
%% Returns: ok | {error, term()}
%%
%% Side effects: Starts a db_api process
%% @end
-spec start_test() -> ok | {error, term()}.
start_test() ->
	?assertEqual({atomic, ok}, db_api:start()).


%% @doc
%% Function: connect_test/0
%% Purpose: Test to establish a database connection
%% Returns: ok | {error, term()}
%%
%% Side effects: Establish a connection to a 
%% @end
-spec connect_test() -> ok | {error, term()}.
connect_test() -> ok.


%% @doc
%% Function: write_resource/0
%% Purpose: Test to add and update resources
%% Returns: ok | {error, term()}
%%
%% Side effects: Writes data to the database
%% @end
-spec write_resource_test() -> ok | {error, term()}.

write_resource_test() ->
% Createing resoruces, all undefined values will have the value undefined
    Resource1 = #resource{id = 1,
						  owner_id = 1},
    Resource2 = #resource{id = 2,
						  owner_id = 2},
    Resource3 = #resource{id = 3,
						  owner_id = 3,
						  version = 1.0},
	% Resource updated with new owner
	Resource4 = #resource{id = 3,
						  owner_id = 2},
	db_api:write_resource(Resource1),
	db_api:write_resource(Resource2),
	db_api:write_resource(Resource3),
	
	{_,Result1} = find_owner(1),
    {_,Result2} = find_owner(2),
    {_,Result3} = find_owner(3),
	{_,Version1} = find_version(3),
	db_api:write_resource(Resource4),
	{_,Version2} = find_version(3),
    {_,Result4} = find_owner(3),
	

    ?assert(Result1 == [1]),
    ?assert(Result2 == [2]),
    ?assert(Result3 == [3]),
    ?assert(Result4 == [2]),
	?assert(Version1 == [1.0]),
	?assert(Version2 == [1.0]).

%% @doc
%% Function: find_owner/1
%% Purpose: Query the database for the owner of resource with given id
%% Returns: {atomic,list()} | {aborted, Reason}
%%
%% Side effects: Query the mnesia database
%% @end
-spec find_owner(ResourceID::integer()) -> {atomic,list()} | {aborted, atom()}.

find_owner(ResourceID) ->
    F = fun() ->
		Resource = #resource{id = ResourceID,owner_id = '$1', _ = '_'},
		mnesia:select(resource, [{Resource, [], ['$1']}])
        end,
    rpc:call(get_local_db_node(),mnesia,transaction,[F]).

%% @doc
%% Function: find_version/1
%% Purpose: Query the database for the version of resource with given id
%% Returns: {atomic,list()} | {aborted, Reason}
%%
%% Side effects: Query the mnesia database
%% @end
-spec find_version(ResourceID::integer()) -> {atomic,list()} | {aborted, atom()}.

find_version(ResourceID) ->
	F = fun() ->
		Resource = #resource{id = ResourceID,version = '$1', _ = '_'},
		mnesia:select(resource, [{Resource, [], ['$1']}])
    end,
    rpc:call(get_local_db_node(),mnesia,transaction,[F]).

%% @doc
%% Function: get_local_db_node/0
%% Purpose: returns the node name of the mnesia database
%% Returns: atom()
%%
%% @end
-spec get_local_db_node() -> atom().

get_local_db_node() ->
	{ok,Host} = inet:gethostname(),
    FullHost = string:concat("database@",Host), 
    HostAtom = list_to_atom(FullHost),
	HostAtom.
%% @doc
%% Function: create_user_test/0
%% Purpose: Test if user creation works
%% Returns: ok | {error, term()}
%%
%% Side effects: Inserts new user to the database
%% @end
-spec create_user_test() -> ok | {error, term()}.
create_user_test() -> 
	db_api:create_user("user1", "pass1"),
	db_api:create_user("user2", "pass2"),	
	?assertEqual(find_pass_by_username("user1"),["pass1"]),
	?assertEqual(find_pass_by_username("user2"),["pass2"]),
	?assertEqual(db_api:create_user("user2", "pass2"), {error, username_exists}).

%% @doc
%% Function: get_user_by_id_test/0
%% Purpose: Test if get_user_by_id works
%% Returns: ok | {error, term()}
%%
%% Side effects: Inserts new user to the database
%% @end
-spec get_user_by_id_test() -> ok | {error, term()}.
get_user_by_id_test() -> 
	db_api:create_user("user3", "pass3"),
	db_api:create_user("user4", "pass4"),	
	?assertEqual([db_api:get_user_by_id(3)], db_api:get_user_by_username("user3")),
	?assertEqual(db_api:get_user_by_id(4), db_api:get_user_by_username("user4")),
	?assertEqual(db_api:get_user_by_id(5), {error, "unknown_user"}).

%% @doc
%% Function: get_user_by_username_test/0
%% Purpose: Test if get_user_by_username works
%% Returns: ok | {error, term()}
%%
%% Side effects: Inserts new user to the database
%% @end
-spec get_user_by_username_test() -> ok | {error, term()}.
get_user_by_username_test() -> 
	db_api:create_user("user5", "pass5"),
	db_api:create_user("user6", "pass6"),	
	?assertEqual([db_api:get_user_by_id(5)], db_api:get_user_by_username("user5")),
	?assertEqual([db_api:get_user_by_id(6)], db_api:get_user_by_username("user6")),
	?assertEqual(db_api:get_user_by_id(7), {error, "unknown_user"}).


%% @doc
%% Function: authenticate_test/0
%% Purpose: Test if authentication works
%% Returns: ok | {error, term()}
%%
%% Side effects: Inserts new user to the database
%% @end
-spec authenticate_test() -> ok | {error, term()}.
authenticate_test() -> 
	db_api:create_user("user7", "pass7"),
	?assertEqual(db_api:authenticate("user7", "pass7"), ok),
	?assertEqual(db_api:authenticate("user7", "pass8"),
				 {error, authentication_error}).
	
%% @doc
%% Function: exists_username_test/0
%% Purpose: Test if exists_username works
%% Returns: ok | {error, term()}
%%
%% Side effects: Inserts new user to the database
%% @end
-spec exists_username_test() -> ok | {error, term()}.
exists_username_test() -> 
	db_api:create_user("user8", "pass8"),
	?assertEqual(db_api:exists_username("user8"), true),
	?assertEqual(db_api:exists_username("use8"), false).
	

%% @doc
%% Function: change_password_test/0
%% Purpose: Test if changing password works
%% Returns: ok | {error, term()}
%%
%% Side effects: Inserts new user to the database
%% @end
-spec change_password_test() -> ok | {error, term()}.
change_password_test() -> 
	db_api:create_user("user9", "pass9"),
	db_api:change_password("user9", "pass9", "pa9"),
	?assertEqual(find_pass_by_username("user9"), ["pa9"]),
	?assertEqual(db_api:change_password("user9", "pass9", "pa9"), 
				 {error, username_password_wrong}),
	?assertEqual(db_api:change_password("us9", "pa9", "pa9"), 
				 {error, username_password_wrong}).


%% @doc
%% Function: find_pass_by_username/1
%% Purpose: returns the password of the user with the given username
%% Args:   string()
%% Returns: tuple()
%% Side effects: Querys the local mnesia database
%% @end

-spec find_pass_by_username(Username::string()) -> string().
find_pass_by_username(Username) ->
    Pattern = #user{ _ = '_',
					 user_name = Username},
	F = fun() ->
		Res = mnesia:match_object(Pattern),
		[Password || #user{user_name=Username,
			password=Password} <- Res]
	end,
	mnesia:activity(transaction, F).
