%% @author Iakovos Koutsoumpakis
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == datapoints_tests ==
%% This module contains several tests to test 
%% the virtual streams functionality.
%%
%% @end

-module(vstreams_tests).
-include_lib("eunit/include/eunit.hrl").

-define(URL, api_help:get_webmachine_url()).
-define(RESOURCES_URL, ?URL ++ "/resources").
-define(STREAMS_URL, ?URL ++ "/streams").
-define(VSTREAMS_URL, ?URL++"/vstreams/").
-define(VSDATAPOINTS_URL, ?URL++"/data/").
-define(TEST_VALUE, "1").
-define(INDEX, "sensorcloud").


%% @doc
%% Function: init_test/0
%% Purpose: Used to start the inets to be able to do HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: Start inets
%% @end
-spec init_test() -> ok | {error, term()}.

init_test() ->
	inets:start().


%% @doc
%% Function: post_test/0
%% Purpose: Test a post request
%% Returns: ok | {error, term()}
%%
%% @end
-spec post_test() -> ok | {error, term()}.
post_test() ->
	Response1 = post_request(?RESOURCES_URL, "application/json", "{	\"name\" : \"test\",	\"tags\" : \"testtag\"	}"),
	check_returned_code(Response1, 200),
	api_help:refresh(),
	Response2 = post_request(?RESOURCES_URL, "application/json", "{	\"name\" : \"test2\",	\"tags\" : \"testtag\"	}"),
	check_returned_code(Response2, 200),
	api_help:refresh(),
	
	Response1 = post_request(?STREAMS_URL, "application/json", "{	\"name\" : \"test\",	\"tags\" : \"testtag\"	}"),
	check_returned_code(Response1, 200),
	api_help:refresh(),
	Response2 = post_request(?STREAMS_URL, "application/json", "{	\"name\" : \"test2\",	\"tags\" : \"testtag\"	}"),
	check_returned_code(Response2, 200),
	api_help:refresh()
	
	.%?assertNotMatch({error, "no match"}, get_index_id(?TEST_NAME)).


%% @doc
%% Checks if the Response has the correct http return code
%% @end
-spec check_returned_code(string(), integer()) -> ok.
check_returned_code(Response, Code) ->
	{ok, Rest} = Response,
	{Header,_,_} = Rest,
	?assertMatch({_, Code, _}, Header).


post_request(URL, ContentType, Body) -> request(post, {URL, [], ContentType, Body}).
get_request(URL) -> request(get, {URL, []}).
request(Method, Request) ->
    httpc:request(Method, Request, [], []).