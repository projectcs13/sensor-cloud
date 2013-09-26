%% @author Tholsgård Gabriel
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == db_api ==
%% This module contains all the necessary functionality to connect
%% to a db and query it.
%%
%% @end

-module(db_api).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, connect/2, traverse/1]).
-export([create_user/2, get_user_by_id/1, get_user_by_username/1, 
		authenticate/2, change_password/3, exists_username/1]).
-include_lib("stdlib/include/qlc.hrl").
-include("include/database.hrl").


%% ====================================================================
%% Internal functions
%% ====================================================================


%% @doc
%% Function: start/0
%% Purpose: Starts process node for database connection.
%% Returns: ok | or {error, term()}
%%
%% Side effects: Starts a process for handling a database connection.
%% @end
-spec start() -> ok | {error, term()}.
start() ->
	ok.


%% @doc
%% Function: connect/2
%% Purpose: Establish a database connection
%% Returns: ok | or {error, term()}
%%
%% Side effects: Establishes a database connection
%% @end
-spec connect(string(), term()) -> {ok, term()} | {error, term()}.
connect(ConnectStr, Options) ->
	ok.

%% @doc
%% Function: createUser/2
%% Purpose: Inserts a new User to the system
%% Args:   string(), string()
%% Returns: ok | or {error, username_exists} | or {error, term()}
%%
%% Side effects: Creates a new User in the database
%% @end
-spec create_user(string(), string()) -> ok | {error, term()}.
create_user(Username, Password) ->	
	case exists_username(Username) of
		true -> {error, username_exists};
		false ->
			Id = mnesia:dirty_update_counter(unique_ids, user, 1),
			F = fun() ->
				mnesia:write(#user{id=Id, user_name=Username, password=Password})							
			end,
			mnesia:activity(transaction, F)
	end.


%% @doc
%% Function: exists_username/1
%% Purpose: Checks if username exists
%% Args:   string()
%% Returns: boolean()
%%
%% @end
-spec exists_username(string()) -> boolean().
exists_username(Username) ->	
	Pattern = #user{_ = '_',
					user_name = Username},
	F = fun() -> 
		mnesia:match_object(Pattern)
	end,
	case mnesia:activity(transaction, F) of
		[] -> false;
		_ -> true
	end.



%% @doc
%% Function: get_user_by_id/1
%% Purpose: Retrieves a User from the system, given the ID
%% Args: integer()
%% Returns: user | {error, unknown_user}
%%
%% @end
-spec get_user_by_id(integer()) -> Record :: #user{}.
get_user_by_id(ID) ->
	F = fun() -> 
		mnesia:read({user, ID}) 
	end,
	case User = mnesia:activity(transaction, F) of
		[] -> {error, unknown_user};
		_ -> User
	end.

%% @doc
%% Function: getUserbyUsername/1
%% Purpose: Retrieves a User from the system, given the username
%% Args: string()
%% Returns: user | {error, unknown_user}
%%
%% @end
-spec get_user_by_username(string()) ->  Record :: #user{}.
get_user_by_username(Username) ->
	Pattern = #user{ _ = '_',
					 user_name = Username},
	F = fun() -> 
		mnesia:match_object(Pattern)
	end,
	case User = mnesia:activity(transaction, F) of
		[] -> {error, unknown_user};
		_ -> User
	end.


%% @doc
%% Function: authenticate/2
%% Purpose: Authenticates a user
%% Args: string(), string()
%% Returns: ok | {error, authentication_error}
%%
%% @end
-spec authenticate(string(), string()) ->  ok | {error, term()}.
authenticate(Username,Password) ->
	Pattern = #user{_ = '_',
					user_name = Username,
					password = Password},
	F = fun() -> 
		mnesia:match_object(Pattern)
	end,
	case mnesia:activity(transaction, F) of
		[] -> {error, authentication_error};
		_ -> ok
	end.


%% @doc
%% Function: change_password/3
%% Purpose: Changes the user's password
%% Args: string(), string(), string()
%% Returns: ok | {error, username_password_wrong}
%%
%% @end
-spec change_password(string(), string(), string()) ->  ok | {error, term()}.
change_password(Username,OldPassword,NewPassword) ->
	Pattern = #user{_ = '_',
					user_name = Username,
					password = OldPassword},
	F = fun() -> 
		case mnesia:match_object(Pattern) of
			[User] -> mnesia:write(User#user{password = NewPassword});
			[] -> {error, username_password_wrong}
		end
	end,
	mnesia:activity(transaction, F).



%% @doc
%% Function: traverse/2
%% Purpose: Prints a table, used for debug
%% Returns: ok | or {error, term()}
%%
%% Side effects: It prints the table on the screen
%% @end
traverse(Table_name)->
    Iterator =  fun(Rec,_)->
                    io:format("~p~n",[Rec]),
                    []
                end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.







