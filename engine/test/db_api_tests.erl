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
	?assertEqual(ok, db_api:start()).


%% @doc
%% Function: start_test/0
%% Purpose: Test to establish a database connection
%% Returns: ok | {error, term()}
%%
%% Side effects: Establish a connection to a 
%% @end
-spec connect_test() -> ok | {error, term()}.
connect_test() -> ok.
