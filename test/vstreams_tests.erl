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

-define(WEBMACHINE_URL, api_help:get_webmachine_url()).
-define(STREAMS_URL, ?WEBMACHINE_URL ++ "/streams/").
-define(VSTREAMS_URL, ?WEBMACHINE_URL ++ "/vstreams/").
-define(VSDATAPOINTS_URL, ?WEBMACHINE_URL ++ "/data/").
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
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \"vstreamuser\"}"}, [], []),
	UserId = lib_json:get_field(Body1,"_id"),
	api_help:refresh(),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = post_request(?STREAMS_URL, "application/json", "{	\"name\" : \"teststream1\",	\"user_id\" : \"" ++ lib_json:to_string(UserId) ++ "\"	}"),
	Streamid1 = lib_json:get_field(Body2, "_id"),
	api_help:refresh(),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = post_request(?STREAMS_URL, "application/json", "{	\"name\" : \"teststream2\",	\"user_id\" : \"" ++ lib_json:to_string(UserId) ++ "\"	}"),
	Streamid2 = lib_json:get_field(Body3, "_id"),
	api_help:refresh(),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid1) ++ "/data", "application/json", "{	\"value\":1}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid1) ++ "/data", "application/json", "{	\"value\":1}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid1) ++ "/data", "application/json", "{	\"value\":1}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid1) ++ "/data", "application/json", "{	\"value\":1}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid1) ++ "/data", "application/json", "{	\"value\":1}"),

	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid2) ++ "/data", "application/json", "{	\"value\":2}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid2) ++ "/data", "application/json", "{	\"value\":2}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid2) ++ "/data", "application/json", "{	\"value\":2}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid2) ++ "/data", "application/json", "{	\"value\":2}"),
	post_request(?STREAMS_URL ++ lib_json:to_string(Streamid2) ++ "/data", "application/json", "{	\"value\":2}"),
	api_help:refresh(),
		Response3 = post_request(?VSTREAMS_URL, "application/json", "{\"user_id\" : \"" ++ lib_json:to_string(UserId) ++ "\", \"name\" : \"post_testvstream1\", \"description\" : \"test\",
			 \"streams_involved\" : [\"" ++ lib_json:to_string(Streamid1) ++ "\", \"" ++ lib_json:to_string(Streamid2) ++ "\"], 
			 \"timestampfrom\" : \"now-1h\", \"function\" : [\"mean\", \"1s\"]}"),
	check_returned_code(Response3, 200),
	api_help:refresh().



%% @doc
%% Function: get_vstream_test/0
%% Purpose: Test the get_stream function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates a document in elasticsearch
%% @end
-spec get_vstream_test() -> ok | {error, term()}.

get_vstream_test() ->
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/vstreams/_search",[],"application/json", "{\"query\":{\"term\" : { \"name\" : \"post_testvstream1\"}}}"}, [], []),
	StreamId = lib_json:get_field(Body1,"hits[0].id"),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(get, {?WEBMACHINE_URL++"/vstreams/" ++ lib_json:to_string(StreamId), []}, [], [])
	.


%% @doc
%% Function: put_vstream_test/0
%% Purpose: Test the put_vstream function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 2 document in elasticsearch and updates them
%% @end
-spec put_vstream_test() -> ok | {error, term()}.
put_vstream_test() ->
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/vstreams/_search",[],"application/json", "{\"query\":{\"term\" : { \"name\" : \"post_testvstream1\"}}}"}, [], []),
	StreamId = lib_json:get_field(Body1,"hits[0].id"),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(put, {?WEBMACHINE_URL++"/vstreams/" ++ lib_json:to_string(StreamId), [], "application/json", "{\n\"name\" : \"updated_testvstream1\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {?WEBMACHINE_URL++"/vstreams/_search",[],"application/json", "{\"query\":{\"term\" : { \"name\" : \"post_testvstream1\"}}}"}, [], []),
	?assertEqual(length(lib_json:get_field(lib_json:to_string(Body3), "hits")), 0),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(post, {?WEBMACHINE_URL++"/vstreams/_search",[],"application/json", "{\"query\":{\"term\" : { \"name\" : \"updated_testvstream1\"}}}"}, [], []),
	?assertEqual(length(lib_json:get_field(lib_json:to_string(Body4), "hits")), 1)
.


%% @doc
%% Function: delete_vstream_test/0
%% Purpose: Test the delete_vstream function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 2 document in elasticsearch and deletes them
%% @end
-spec delete_vstream_test() -> ok | {error, term()}.

delete_vstream_test() ->
	%delete vstream, make sure there are no vsdatapoints left
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/vstreams/_search",[],"application/json", "{\"query\":{\"term\" : { \"name\" : \"updated_testvstream1\"}}}"}, [], []),
	StreamId = lib_json:get_field(Body1,"hits[0].id"),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(delete, {?WEBMACHINE_URL++"/vstreams/" ++ lib_json:to_string(StreamId), []}, [], []),
	api_help:refresh(),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {?WEBMACHINE_URL++"/vstreams/_search",[],"application/json", "{\"query\":{\"term\" : { \"name\" : \"updated_testvstream1\"}}}"}, [], []),
	?assertEqual(length(lib_json:get_field(lib_json:to_string(Body3), "hits")), 0),
	{ok, {{_Version3, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {?WEBMACHINE_URL++"/vstreams/" ++ lib_json:to_string(StreamId) ++ "/data/_search", []}, [], []),
	?assertEqual(length(lib_json:get_field(lib_json:to_string(Body4), "data")), 0),
	api_help:refresh(),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/vstreamuser", []}, [], [])
	.


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