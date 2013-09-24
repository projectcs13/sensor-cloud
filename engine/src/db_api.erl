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
-export([start/0, connect/2]).



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














