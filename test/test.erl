%% @author Tommy Mattsson
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]

%% @doc Test wrapper module
-module(test).
-author('Tommy Mattsson').
-export([run/0, run/1]).

-define(WEBMACHINE_URL, api_help:get_webmachine_url()).
-define(RESOURCE_URL, ?WEBMACHINE_URL++"/resources/").
%% @doc
%% Function: run/0
%% Purpose: Wrapper function for testing in order to be able to return a 
%%          non-zero exit code on failure of one or more test cases fails.
%%          This is for getting tests to work with Travis CI.
%% Returns: ok | no_return()
%% @end
run() ->
	run("ebin").


post_request(URL, ContentType, Body) -> request(post, {URL, [], ContentType, Body}).
request(Method, Request) ->
	httpc:request(Method, Request, [], []).

run(Suite) ->    
	Result = eunit:test(Suite,
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
