
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
add_user(username,password) ->
    db_api:create_user(username,password).


%% @doc
%% Function: remove_user/1
%% Purpose: Removes the current user from the database
%% Args: username
%% Returns: (???Success or error message)
%%
%% @end
remove_user(username) ->
	db_api:remove_user(username).


%% @doc
%% Function: change_password/3
%% Purpose: Changes the password of the current user, given the old and the new one
%% Args: username, password, new_password
%% Returns: Success or error message(database or authentication error)
%%
%% @end
change_password(username,password,new_password) ->
	db_api:change_password(username,old_password,new_password).


%% @doc
%% Function: authenticate_user/2
%% Purpose: Checks if the login information is correct
%% Args: username, password
%% Returns: Success or error message(database error or authentication error)
%%
%% @end
authenticate(username,password) ->
	db_api:authenticate(username,password).
