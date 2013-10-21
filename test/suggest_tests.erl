%% @author Georgios Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == suggest_tests ==
%% This module contains several tests to test the functionallity
%% in the restful API for the autocomplete feature.
%%
%% @end

-module(suggest_tests).
-include_lib("eunit/include/eunit.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% ====================================================================
%% Internal functions
%% ====================================================================

-define(SUGGEST_URL, "http://localhost:8000/suggest/").
-define(RESOURCE_URL, "http://localhost:8000/resources/").


%% @doc
%% Function: post_test/0
%% Purpose: Test a post request
%% Returns: ok | {error, term()}
%%
%% @end
-spec post_test() -> ok | {error, term()}.
post_test() ->
	Response1 = post_request(?RESOURCE_URL, "application/json", 
							"{
								\"model\" : \"test-smartphone1\",
								\"tags\" : \"test-tag\"
							}"),
	check_returned_code(Response1, 200),
 	Response2 = get_request(?SUGGEST_URL++"test-smartphone1"),     
 	check_returned_code(Response2, 200),
 	{ok, {_, _ ,Body}} = Response2,
	erlang:display(Body),
	?assertEqual("test-tag",lib_json:get_value_field(Body, "testsuggest[0].options[0].payload.tags")).


%% @doc
%% Function: get_suggestion_test/0
%% Purpose: Test a get request for a suggestion
%% Returns: ok | {error, term()}
%%
%% @end
-spec get_suggestion_test() -> ok | {error, term()}.
get_suggestion_test() ->
	Response1 = post_request(?RESOURCE_URL, "application/json", 
							"{
								\"model\" : \"test-smartphone2\",
								\"suggest\" : {
									\"input\": [ \"test-smartphone2\" ],
									\"output\": \"ericsson\",
									\"payload\" : { \"brand\" : \"ericsson\" },
									\"weight\" : 2
								}
							}"),
	check_returned_code(Response1, 200),
	Response2 = get_request(?SUGGEST_URL ++ "/test-smartphone2"),
	check_returned_code(Response2, 200),
	{ok, {_, _ ,Body}} = Response2,
	erlang:display(Body),
	?assertEqual("ericsson",lib_json:get_value_field(Body, "testsuggest[0].options[0].payload.brand")).


%% @doc
%% Function: get_non_existing_term_test/0
%% Purpose: Test a get request for a model that doesn't exist
%% Returns: ok | {error, term()}
%%
%% @end
-spec get_non_existing_term_test() -> ok | {error, term()}.
 get_non_existing_term_test() ->
 	Response1 = get_request(?SUGGEST_URL ++ "non-existing-term"),
 	check_returned_code(Response1, 404).




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


post_request(URL, ContentType, Body) -> request(post, {URL, [], ContentType, Body}).
put_request(URL, ContentType, Body) -> request(put, {URL, [], ContentType, Body}).
get_request(URL)                     -> request(get,  {URL, []}).
delete_request(URL)                     -> request(delete,  {URL, []}).

request(Method, Request) ->
    httpc:request(Method, Request, [], []).
