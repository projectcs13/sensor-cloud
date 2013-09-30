
%% @author Koutsoumpakis Iakovos
%% [www.csproj13.student.it.uu.se]
%% @copyright [Copyright information]
%% @doc == [Module name] ==
%% This module works as an intermediate betwwen the restful_api and the db_api
%% and performs user account tasks
%% @end

-module(userManagement).
-vsn(1.0).

%% ====================================================================
%% API functions
%% ====================================================================
-export([add_user/2,remove_user/1,change_password/3,authenticate/2]).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Function: add_user/2
%% Purpose: Adds user to the database, if the username doesn't exist already
%% Args: username, password
%% Returns: Success or error message(username exists or database error)
%%
%% @end
-spec add_user(string(), string()) -> ok | {error, term()}.
add_user(Username,Password) ->
    db_api:create_user(Username,Password).


%% @doc
%% Function: remove_user/1
%% Purpose: Removes the current user from the database
%% Args: username
%% Returns: Success or database error message
%%
%% @end
%% not implemented in db_api yet, should not be used
-spec remove_user(string()) -> ok | {error, term()}.
remove_user(Username) ->
	db_api:remove_user(Username).


%% @doc
%% Function: change_password/3
%% Purpose: Changes the password of the current user, given the old and the new one
%% Args: username, password, new_password
%% Returns: Success or error message(database or authentication error)
%%
%% @end
-spec change_password(string(), string(), string()) ->  ok | {error, term()}.
change_password(Username,Old_Password,New_password) ->
	db_api:change_password(Username,Old_Password,New_password).


%% @doc
%% Function: authenticate_user/2
%% Purpose: Checks if the login information is correct
%% Args: username, password
%% Returns: Success or error message(database error or authentication error)
%%
%% @end
-spec authenticate(string(), string()) ->  ok | {error, term()}.
authenticate(Username,Password) ->
	db_api:authenticate(Username,Password).
