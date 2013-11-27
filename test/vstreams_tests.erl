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
-define(STREAMS_URL, ?URL ++ "/streams/").
-define(VSTREAMS_URL, ?URL ++ "/vstreams/").
-define(VSDATAPOINTS_URL, ?URL ++ "/data/").
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
	
	Response1 = post_request(?STREAMS_URL, "application/json", "{	\"name\" : \"teststream1\",	\"user_id\" : \"testuser\"	}"),
	check_returned_code(Response1, 200),
	{ok, {_, _ ,Body}} = Response1,
	Streamid1 = lib_json:get_field(Body, "_id"),
	api_help:refresh(),
	Response2 = post_request(?STREAMS_URL, "application/json", "{	\"name\" : \"teststream2\",	\"user_id\" : \"testuser\"	}"),
	check_returned_code(Response2, 200),
	{ok, {_, _ ,Body2}} = Response2,
	erlang:display(Body2),
	Streamid2 = lib_json:get_field(Body2, "_id"),
	api_help:refresh(),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid1) ++ "/data", "application/json", "{	\"value\":\"1\"}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid1) ++ "/data", "application/json", "{	\"value\":\"1\"}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid1) ++ "/data", "application/json", "{	\"value\":\"1\"}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid1) ++ "/data", "application/json", "{	\"value\":\"1\"}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid1) ++ "/data", "application/json", "{	\"value\":\"1\"}"),

	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid2) ++ "/data", "application/json", "{	\"value\":\"2\"}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid2) ++ "/data", "application/json", "{	\"value\":\"2\"}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid2) ++ "/data", "application/json", "{	\"value\":\"2\"}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid2) ++ "/data", "application/json", "{	\"value\":\"2\"}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid2) ++ "/data", "application/json", "{	\"value\":\"2\"}"),
	api_help:refresh(),
	Response3 = post_request(?VSTREAMS_URL, "application/json", "{\"name\" : \"myvstream\", \"description\" : \"test\",
			 \"streams_involved\" : [\"" ++ lib_json:to_string(Streamid1) ++ "\", \"" ++ lib_json:to_string(Streamid2) ++ "\"], 
			 \"timestampfrom\" : \"now-1h\", \"function\" : [\"aggregate\", \"mean\", \"1s\"]}"),
	check_returned_code(Response3, 200),
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