%% @author Jose Arias, Andreas Moregård Haubenwaller
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == streams_tests ==
%% This module contains several tests to test the functionallity
%% in the module streams which is done by calling the webbmachine.
%%
%% @end

-module(search_tests).
-include_lib("eunit/include/eunit.hrl").
-export([]).

-define(WEBMACHINE_URL, api_help:get_webmachine_url()).
-define(ELASTIC_SEARCH_URL, api_help:get_webmachine_url()).

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
%% Function: get_search_test/0
%% Purpose: Test the get_search function by doing some HTTP requests
%% Returns: ok | {error, term()}
%% @end
get_search_test() ->
    {ok, {{_Version1, 405, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(get, {?WEBMACHINE_URL++"/_history", []}, [], []),
    {ok, {{_Version2, 501, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(get, {?WEBMACHINE_URL++"/_search", []}, [], []),
    {ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {?WEBMACHINE_URL++"/_history?stream_id=id_that_doesnt_exist", []}, [], []),
    ?assertEqual([],lib_json:get_field(Body3,"history[0].data")).



%% @doc
%% Function: process_search_post_test/0
%% Purpose: Test the process_post_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%% @end
process_search_post_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {"http://localhost:8000/users", [],"application/json", "{\"username\" : \"search\"}"}, [], []),
	UserId = lib_json:get_field(Body,"_id"),
	refresh(),
    {ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"name\" : \"search\",\"user_id\" : \"" ++ lib_json:to_string(UserId) ++ "\", \"private\" : \"false\"}"}, [], []),
    {ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"name\" : \"search\",\"user_id\" : \"" ++ lib_json:to_string(UserId) ++ "\", \"private\" : \"true\"}"}, [], []),
    DocId1 = lib_json:get_field(Body1,"_id"),
    DocId2 = lib_json:get_field(Body2,"_id"),
    refresh(),
	timer:sleep(1000), %% still needed for the test to pass
    {ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {?WEBMACHINE_URL++"/_search", [],"application/json", "{\"query\":{\"match_all\":{}}}"}, [], []),
    {ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, _Body8}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
    {ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, _Body9}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	{ok, {{_Version10, 200, _ReasonPhrase10}, _Headers10, _Body10}} = httpc:request(delete, {"http://localhost:8000/users/" ++ lib_json:to_string(UserId), []}, [], []),
	erlang:display(Body3),
    ?assertEqual(true,lib_json:get_field(Body3,"streams.hits.total") >= 1).

%% @doc
%% Function: refresh/0
%% Purpose: Help function to find refresh the sensorcloud index
%% Returns: {ok/error, {{Version, Code, Reason}, Headers, Body}}
%% @end
refresh() ->
	httpc:request(post, {?ELASTIC_SEARCH_URL++"/sensorcloud/_refresh", [],"", ""}, [], []).


