%% @author Gabriel Tholsgård
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == poll_help_test ==
%% This module contains tests of the helper functions
%% needed for the polling system. 
%%
%% @end

-module(poll_help_test).

-include_lib("eunit/include/eunit.hrl").

-export([]).


%% ====================================================================
%% API functions
%% ====================================================================

%% @doc
%% Function: inti_test/0
%% Purpose: Used to start the inets to be able to do HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: Start inets
%% @end
-spec init_test() -> ok | {error, term()}.
init_test() ->
	inets:start().


%% @doc
%% Function: get_resources_using_polling_test/0
%% Purpose: Retrieves all resources from Elastic Search that are using polling.
%% Returns: ok | {error, term()}.
%% @end
-spec get_resources_using_polling_test() -> ok | {error, term()}.
get_resources_using_polling_test() ->
	httpc:request(delete, {"http://localhost:9200/sensorcloud/resource", []},
				  [], []),
	%% Test that should return the empty list, since no resources exists.
	?assertMatch([], poll_help:get_resources_using_polling()),
	
	%% Test that should return a list of length one.
	httpc:request(post, {"http://localhost:9200/sensorcloud/resource", [],
						 "application/json",
						 "{\"model\" : \"test-model\", \"uri\" : \"127.0.0.1\"}"
						}, [], []),
	httpc:request(post, {"http://localhost:9200/sensorcloud/resource", [],
						 "application/json",
						 "{\"model\" : \"test-model2\"}"
						}, [], []),
	timer:sleep(1000),
	?assertMatch(1, length(poll_help:get_resources_using_polling())),
	
	%% Test that should return a list of length two.
	httpc:request(post, {"http://localhost:9200/sensorcloud/resource", [],
						 "application/json",
						 "{\"model\" : \"test-model\", \"uri\" : \"127.0.0.2\"}"
						}, [], []),
	timer:sleep(1000),
	?assertMatch(2, length(poll_help:get_resources_using_polling())).


%% ====================================================================
%% Internal functions
%% ====================================================================


