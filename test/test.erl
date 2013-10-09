%% @author Tommy Mattsson
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]

%% @doc Test wrapper module
-module(test).
-author('Tommy Mattsson').
-export([run/0]).

%% @doc
%% Function: run/0
%% Purpose: Wrapper function for testing in order to be able to return a 
%%          non-zero exit code on failure of one or more test cases fails.
%%          This is for getting tests to work with Travis CI.
%% Returns: ok | no_return()
%% @end
run() ->
	timer:sleep(10000),
    Result = eunit:test("ebin",
			[verbose, 
			 {cover_enabled, true},
			 {report, {eunit_surefire, [{dir, "test-results"}]}}
			]),
    case Result of
	ok ->
	    init:stop();
	error ->
	    halt(1)
    end.
