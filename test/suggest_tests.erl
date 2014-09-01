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

-define(URL, api_help:get_webmachine_url()).
-define(STREAMS_URL, ?URL ++ "/streams").
-define(SUGGEST_URL, ?URL ++ "/suggest").
-define(RESOURCES_URL, ?URL ++ "/resources").

%% @doc
%% Test a post request
%% @end
%-spec post_test() -> ok | {error, term()}.
%post_test() ->
	%Response1 = post_request(?RESOURCES_URL, "application/json", 
	%						"{
	%							\"model\" : \"testsmartphone2\",
	%							\"tags\" : \"testtag\"
	%		}"),
	%check_returned_code(Response1, 200),
	%api_help:refresh(),
	%Response2 = get_request(?SUGGEST_URL++"/testsmartphone2"),     
	%check_returned_code(Response2, 200),
	%{ok, {_, _ ,Body}} = Response2,
	%?assertEqual(<<"testtag">>,lib_json:get_field(Body, "suggestions[0].payload.tags")).


%% @doc
%% Test creating a suggestion which contains both resource and streams
%% @end
%-spec post_and_stream_test() -> ok | {error, term()}.
%post_and_stream_test() ->
	%Response1 = post_request(?RESOURCES_URL, "application/json", 
	%						"{
	%							\"model\" : \"testwithstream\",
	%							\"tags\" : \"testtag\"
	%		}"),
	%check_returned_code(Response1, 200),
	%{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = Response1,
	%Id = lib_json:get_field(Body, "_id"),
	%api_help:refresh(),
	%{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, _Body1}} = httpc:request(post, {?STREAMS_URL, [],"application/json", "{\"name\" : \"search\",\"resource_id\" : \""++lib_json:to_string(Id)++"\", \"private\" : \"false\", \"tags\":\"test_tag\"}"}, [], []),
	%api_help:refresh(),
	%{ok, {{_Version11, 200, _ReasonPhrase11}, _Headers11, _Body11}} = httpc:request(post, {?STREAMS_URL, [],"application/json", "{\"name\" : \"search2\",\"resource_id\" : \""++lib_json:to_string(Id)++"\", \"private\" : \"false\", \"tags\":\"test2\"}"}, [], []),
	%api_help:refresh(),
	%Response2 = get_request(?SUGGEST_URL++"/testwithstream"),
	%check_returned_code(Response2, 200),
	%{ok, {_, _ ,Body2}} = Response2,
	%?assertEqual(<<"testtag">>,lib_json:get_field(Body2, "suggestions[0].payload.tags")),
	%?assertEqual(true, lib_json:field_value_exists(Body2, "suggestions[0].payload.streams[*].tags",<<"test_tag">>)),
	%?assertEqual(<<"test2">>, lib_json:get_field_value(Body2, "suggestions[0].payload.streams[*].tags",<<"test2">>)).


%% @doc
%% Test a get request for a suggestion
%% @end
%-spec get_suggestion_test() -> ok | {error, term()}.
%get_suggestion_test() ->
	%Response1 = post_request(?RESOURCES_URL, "application/json", 
	%						"{
	%							\"model\" : \"testgetsuggestion\",
	%							\"manufacturer\" : \"ericsson\"
	%		}"),
	%check_returned_code(Response1, 200),
	%api_help:refresh(),
	%Response2 = get_request(?SUGGEST_URL ++ "/testgetsuggestion"),
	%check_returned_code(Response2, 200),
	%{ok, {_, _ ,Body}} = Response2,
	%?assertEqual(<<"ericsson">>,lib_json:get_field(Body, "suggestions[0].payload.manufacturer")).


%% @doc
%% Test a get request for a model that doesn't exist
%% @end
%-spec get_non_existing_term_test() -> ok | {error, term()}.
%get_non_existing_term_test() ->
	%Response1 = get_request(?SUGGEST_URL ++ "/non-existing-term"),
	%check_returned_code(Response1, 404).



%% @doc
%% Updates a resource and makes sure that the suggestion was updated properly
%% @end
%-spec update_resource_test() -> ok | {error, term()}.
%update_resource_test() ->
	%Response1 = post_request(?RESOURCES_URL, "application/json", 
	%						"{
	%							\"model\" : \"testupdate\",
	%							\"tags\" : \"testtag\",
	%							\"manufacturer\" : \"testmanu\"
	%		}"),
	%check_returned_code(Response1, 200),
	%{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = Response1,
	%Id = lib_json:get_field(Body, "_id"),
	%api_help:refresh(),
	%{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, _Body1}} = httpc:request(post, {?STREAMS_URL, [],"application/json", "{\"name\" : \"search\",\"resource_id\" : \""++lib_json:to_string(Id)++"\", \"private\" : \"false\", \"tags\":\"test_tag\"}"}, [], []),
	%api_help:refresh(),
	%{ok, {{_Version11, 200, _ReasonPhrase11}, _Headers11, _Body11}} = httpc:request(post, {?STREAMS_URL, [],"application/json", "{\"name\" : \"search2\",\"resource_id\" : \""++lib_json:to_string(Id)++"\", \"private\" : \"false\", \"tags\":\"test2\"}"}, [], []),
	%api_help:refresh(),
	%Response2 = get_request(?SUGGEST_URL++"/testupdate"),
	%check_returned_code(Response2, 200),
	%{ok, {_, _ ,Body2}} = Response2,
	%?assertEqual(<<"testtag">>,lib_json:get_field(Body2, "suggestions[0].payload.tags")),
	%?assertEqual(true, lib_json:field_value_exists(Body2, "suggestions[0].payload.streams[*].tags",<<"test_tag">>)),
	%?assertEqual(<<"test2">>, lib_json:get_field_value(Body2, "suggestions[0].payload.streams[*].tags",<<"test2">>)),
	%{ok, {{_Version21, 200, _ReasonPhrase21}, _Headers21, _Body21}} = httpc:request(put, {?RESOURCES_URL++"/"++lib_json:to_string(Id), [],"application/json", "{\"model\" : \"testupdate\",\"tags\" : \"newtag\"}"}, [], []),
	%api_help:refresh(),
	%{ok, {_, _ ,Body3}} = get_request(?SUGGEST_URL++"/testupdate"),
	%?assertEqual(<<"newtag">>,lib_json:get_field(Body3, "suggestions[0].payload.tags")),
	%?assertEqual(true, lib_json:field_value_exists(Body3, "suggestions[0].payload.streams[*].tags",<<"test_tag">>)),
	%?assertEqual(<<"test2">>, lib_json:get_field_value(Body3, "suggestions[0].payload.streams[*].tags",<<"test2">>)).



%% @doc
%% Updates a stream and makes sure that the suggestion was updated properly
%% @end
%-spec update_stream_test() -> ok | {error, term()}.
%update_stream_test() ->
	%Response1 = post_request(?RESOURCES_URL, "application/json", 
	%						"{
	%							\"model\" : \"teststreamupdate\",
	%							\"tags\" : \"testtag\",
	%							\"manufacturer\" : \"testmanu\"
	%		}"),
	%check_returned_code(Response1, 200),
	%{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = Response1,
	%Id = lib_json:get_field(Body, "_id"),
	%api_help:refresh(),
	%{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, _Body1}} = httpc:request(post, {?STREAMS_URL, [],"application/json", "{\"name\" : \"search\",\"resource_id\" : \""++lib_json:to_string(Id)++"\", \"private\" : \"false\", \"tags\":\"test_tag\"}"}, [], []),
	%api_help:refresh(),
	%{ok, {{_Version11, 200, _ReasonPhrase11}, _Headers11, Body11}} = httpc:request(post, {?STREAMS_URL, [],"application/json", "{\"name\" : \"search2\",\"resource_id\" : \""++lib_json:to_string(Id)++"\", \"private\" : \"false\", \"tags\":\"test2\"}"}, [], []),
	%StreamId = lib_json:get_field(Body11, "_id"),
	%api_help:refresh(),
	%Response2 = get_request(?SUGGEST_URL++"/teststreamupdate"),
	%check_returned_code(Response2, 200),
	%{ok, {_, _ ,Body2}} = Response2,
	%?assertEqual(<<"testtag">>,lib_json:get_field(Body2, "suggestions[0].payload.tags")),
	%?assertEqual(true, lib_json:field_value_exists(Body2, "suggestions[0].payload.streams[*].tags",<<"test_tag">>)),
	%?assertEqual(<<"test2">>, lib_json:get_field_value(Body2, "suggestions[0].payload.streams[*].tags",<<"test2">>)),
	%{ok, {{_Version21, 200, _ReasonPhrase21}, _Headers21, _Body21}} = httpc:request(put, {?STREAMS_URL++"/"++lib_json:to_string(StreamId), [],"application/json", "{\"name\" : \"search2\", \"private\" : \"false\", \"tags\":\"newtest2\"}"}, [], []),
	%api_help:refresh(),
	%{ok, {_, _ ,Body3}} = get_request(?SUGGEST_URL++"/teststreamupdate"),
	%?assertEqual(<<"testtag">>,lib_json:get_field(Body3, "suggestions[0].payload.tags")),
	%?assertEqual(true, lib_json:field_value_exists(Body3, "suggestions[0].payload.streams[*].tags",<<"test_tag">>)),
	%?assertEqual(<<"newtest2">>, lib_json:get_field_value(Body3, "suggestions[0].payload.streams[*].tags",<<"newtest2">>)).


%% @doc
%% Checks if API for text autocompletion works properly
%% @end
%-spec text_autocompletion_test() -> ok | {error, term()}.
%text_autocompletion_test() ->
	%Response1 = post_request(?RESOURCES_URL, "application/json", 
	%						"{
	%							\"model\" : \"testauto\",
	%							\"tags\" : \"testautotag\"
	%		}"),
	%check_returned_code(Response1, 200),
	%{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = Response1,
	%Id = lib_json:get_field(Body, "_id"),
	%api_help:refresh(),
	%{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?STREAMS_URL, [],"application/json", "{\"name\" : \"search\",\"resource_id\" : \""++lib_json:to_string(Id)++"\", \"private\" : \"false\", \"tags\":\"test_auto_tag\"}"}, [], []),
	%api_help:refresh(),
	%Response2 = get_request(?SUGGEST_URL++"/model/"++"testauto"),
	%Response3 = get_request(?SUGGEST_URL++"/tags/"++"testautotag"),
	%Response4 = get_request(?SUGGEST_URL++"/tags/"++"test_auto_tag"),
	%check_returned_code(Response2, 200),
	%check_returned_code(Response3, 200),
	%check_returned_code(Response4, 200),
	%{ok, {_, _ ,Body2}} = Response2,
	%{ok, {_, _ ,Body3}} = Response3,
	%{ok, {_, _ ,Body4}} = Response4,
	%?assertEqual(<<"testauto">>,lib_json:get_field(Body2, "suggestions[0].text")),
	%?assertEqual(<<"testautotag">>, lib_json:get_field(Body3, "suggestions[0].text")),
	%?assertEqual(<<"test_auto_tag">>, lib_json:get_field(Body4, "suggestions[0].text")),
	%make changes to resource/stream and check if it got updated
	%StreamId = lib_json:get_field(Body1, "_id"),
	%{ok, {{_Version21, 200, _ReasonPhrase21}, _Headers21, _Body21}} = httpc:request(put, {?STREAMS_URL++"/"++lib_json:to_string(StreamId), [],"application/json", "{\"name\" : \"newsearch\", \"private\" : \"false\", \"tags\":\"newtag\"}"}, [], []),
	%api_help:refresh(),
	%Response5 = get_request(?SUGGEST_URL++"/name/"++"newsearch"),
	%check_returned_code(Response5, 200),
	%{ok, {_, _ ,Body5}} = Response5,
	%?assertEqual(<<"newsearch">>, lib_json:get_field(Body5, "suggestions[0].text")).

%% @doc
%% Checks if API for search autocompletion works 
%% @end
-spec search_autocompletion_test() -> ok | {error, term()}.
search_autocompletion_test() ->
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, _Body1}} = httpc:request(post, {"http://localhost:8000/_search", [],"application/json", "{\"sort\":\"nr_subscribers\",\"query\":{\"query_string\":{\"query\":\"test\"}}}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(get, {"http://localhost:8000/suggest/_search?query=test", []}, [], []),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, _Body3}} = httpc:request(get, {"http://localhost:8000/suggest/_search?query=öäå", []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, _Body4}} = httpc:request(get, {"http://localhost:8000/suggest/_search?query=test%20or", []}, [], []), 
	?assertNotEqual(0,string:str(lib_json:to_string(lib_json:get_field(Body2, "suggestions[0].text")), "test")).


%% @doc
%% Checks if the Response has the correct http return code
%% @end
-spec check_returned_code(string(), integer()) -> ok.
check_returned_code(Response, Code) ->
	{ok, Rest} = Response,
	{Header,_,_} = Rest,
	?assertMatch({_, Code, _}, Header).


post_request(URL, ContentType, Body) -> request(post, {URL, [], ContentType, Body}).

put_request(URL, ContentType, Body) -> request(put, {URL, [], ContentType, Body}).
get_request(URL)                     -> request(get,  {URL, []}).

request(Method, Request) ->
	httpc:request(Method, Request, [], []).

