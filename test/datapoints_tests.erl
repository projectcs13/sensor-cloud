%% @author Iakovos Koutsoumpakis
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == datapoints_tests ==
%% This module contains several tests to test the functionallity
%% in the restful API in users.
%%
%% @end

-module(datapoints_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% ====================================================================
%% Internal functions
%% ====================================================================

-define(DATAPOINTS_URL, "http://localhost:8000/streams/4/data/").
-define(TEST_VALUE, "3").
-define(TEST_TIMESTAMP, "2").

%% @doc
%% Function: post_test/0
%% Purpose: Test a post request
%% Returns: ok | {error, term()}
%%
%% @end
-spec post_test() -> ok | {error, term()}.
post_test() ->
        Response1 = post_request(?DATAPOINTS_URL, "application/json",
                                         "{\"value\":\"" ++ ?TEST_VALUE ++ "\", \"timestamp\": \"" ++ ?TEST_TIMESTAMP ++ "\"}"),
        check_returned_code(Response1, 200),
        refresh(),
        ?assertNotMatch({error, "no match"}, get_index_id(?TEST_VALUE, ?TEST_TIMESTAMP)).


%% @doc
%% Function: get_existing_datapoint_test/0
%% Purpose: Test a get request for a datapoint that exists, using its Id
%% Returns: ok | {error, term()}
%%
%% @end
-spec get_existing_datapoint_test() -> ok | {error, term()}.
get_existing_datapoint_test() ->
        Id = get_index_id(?TEST_VALUE, ?TEST_TIMESTAMP),
        ?assertNotMatch({error, "no match"}, Id),
        Response1 = get_request(?DATAPOINTS_URL ++ "_search?_id=" ++ Id),
        check_returned_code(Response1, 200).


%% @doc
%% Function: no_timestamp_test/0
%% Purpose: Test a post request without a timestamp
%% Returns: ok | {error, term()}
%%
%% @end
-spec no_timestamp_test() -> ok | {error, term()}.
no_timestamp_test() ->
        Response1 = post_request("http://localhost:8000/streams/5/data/", "application/json",
                                         "{\"value\":\"55\"}"),
        check_returned_code(Response1, 200),
		refresh(),
		{ok,{_,_,Body}} = httpc:request(get, {"http://localhost:8000/streams/5/data/", []}, [], []),
		ObjectList = lib_json:get_field(Body,"hits"),
        ?assertEqual(true, lib_json:get_field(lists:nth(1,ObjectList),"timestamp") =/= undefined).

%% @doc
%% Function: get_index_id/0
%% Purpose: Searches the ES and returns the _id of a datapoint
%% Returns: string() | {error, string()}
%%
%% @end
-spec get_index_id(string(), string()) -> string() | {error, string()}.
get_index_id(Uvalue, Uvalue2) ->
        Response1 = get_request(?DATAPOINTS_URL ++ "_search?value=" ++ Uvalue ++ "&timestamp=" ++ Uvalue2),
        check_returned_code(Response1, 200),
        {ok, {_,_,A}} = Response1,
        case re:run(A, "id\":\"[^\"]*", [{capture, first, list}]) of
                {match, ["id\":\"" ++ Id]} -> Id;
                nomatch -> {error, "no match"}
        end.


%% @doc
%% Function: check_returned_code/0
%% Purpose: Checks if the Response has the correct http return code
%%
%% @end
-spec check_returned_code(string(), integer()) -> ok.
check_returned_code(Response, Code) ->
        {ok, Rest} = Response,
        {Header,_,_} = Rest,
        ?assertMatch({_, Code, _}, Header).


%% @doc
%% Function: get_non_existent_user_datapoint/0
%% Purpose: Tests a get request for a datapoint that doesn't exist
%% Returns: ok | {error, term()}
%%
%% @end
-spec get_non_existent_datapoint_test() -> ok | {error, term()}.
get_non_existent_datapoint_test() ->
        Response1 = get_request(?DATAPOINTS_URL ++ "_search?_id=" ++ "nonexistent"),
		{ok, Rest} = Response1,
		{_,_,Result} = Rest,
	    ?assertNotEqual(0, string:str(Result, "hits\":[]")).

post_request(URL, ContentType, Body) -> request(post, {URL, [], ContentType, Body}).
get_request(URL) -> request(get, {URL, []}).
request(Method, Request) ->
    httpc:request(Method, Request, [], []).

%% @doc
%% Function: refresh/0
%% Purpose: Help function to find refresh the sensorcloud index
%% Returns: {ok/error, {{Version, Code, Reason}, Headers, Body}}
%% @end
refresh() ->
    httpc:request(post, {"http://localhost:9200/sensorcloud/_refresh", [],"", ""}, [], []).

