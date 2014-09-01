%% @author Georgios Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == users_tests ==
%% This module contains several tests to test the functionallity
%% in the restful API in users.
%%
%% @end

-module(users_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% ====================================================================
%% Internal functions
%% ====================================================================

-define(URL, api_help:get_webmachine_url()).
-define(USERS_URL, ?URL ++ "/users/").
-define(STREAMS_URL, ?URL ++ "/streams/").
-define(RESOURCES_URL, ?URL ++ "/resources/").
-define(TEST_NAME, "weird_test_name").
-define(TEST_EMAIL, "weird_test_email").

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
	Response1 = post_request(?USERS_URL, "application/json", 
					 "{\"username\":\""++?TEST_NAME++"\"}"),
	check_returned_code(Response1, 200),
	api_help:refresh(),
	?assertNotMatch({error, "no match"}, get_index_id(?TEST_NAME)).


%% @doc
%% Function: get_existing_user_test/0
%% Purpose: Test a get request for a user that exists
%% Returns: ok | {error, term()}
%%
%% @end
-spec get_existing_user_test() -> ok | {error, term()}.
get_existing_user_test() ->
	Response1 = get_request(?USERS_URL ++ lib_json:to_string(?TEST_NAME)),
	check_returned_code(Response1, 200).


%% @doc
%% Function: get_non_existing_user_test/0
%% Purpose: Test a get request for a user that doesn't exist
%% Returns: ok | {error, term()}
%%
%% @end
-spec get_non_existing_user_test() -> ok | {error, term()}.
get_non_existing_user_test() ->
	Response1 = get_request(?USERS_URL ++ "non-existing-key"),
	check_returned_code(Response1, 404).


%% @doc
%% Function: get_user_search_test/0
%% Purpose: Perform a GET search query
%% Returns: ok | {error, term()}
%%
%% @end
-spec get_user_search_test() -> ok | {error, term()}.
get_user_search_test() ->	
	Response1 = get_request(?USERS_URL ++ "_search?username="++?TEST_NAME),
	check_returned_code(Response1, 200),
	{ok, Rest} = Response1,
	{_,_,A} = Rest,
	?assertEqual(true, lib_json:field_value_exists(A, "hits.hits[*]._source.username", <<?TEST_NAME>>)).


%% @doc
%% Function: post_user_search_test/0
%% Purpose: Perform a POST search query
%% Returns: ok | {error, term()}
%%
%% @end
-spec post_user_search_test() -> ok | {error, term()}.
post_user_search_test() ->	
	Response1 = post_request(?USERS_URL ++ "_search", "application/json", 
					 "{\"username\":\""++?TEST_NAME++"\"}"),
	check_returned_code(Response1, 200),
	{ok, Rest} = Response1,
	{_,_,A} = Rest,
	?assertEqual(true, lib_json:field_value_exists(A, "hits.hits[*]._source.username", <<?TEST_NAME>>)).



%% @doc
%% Function: put_user_search_test/0
%% Purpose: Checks if PUT requests work
%% Returns: ok | {error, term()}
%%
%% @end
-spec put_user_search_test() -> ok | {error, term()}.
put_user_search_test() ->	
	Response1 = put_request(?USERS_URL++?TEST_NAME, "application/json", "{\"private\":\"true\"}"),
	check_returned_code(Response1, 200),
	Response2 = get_request(?USERS_URL ++?TEST_NAME),
	{ok, Rest} = Response2,
	{_,_,A} = Rest,
	?assertEqual(true, lib_json:field_value_exists(A, "private", <<"true">>)).



%% @doc
%% Function: dont_list_private_users_test/0
%% Purpose: Test that users with private set to true will not be shown on listing
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and deletes documents in elasticsearch
%% @end
-spec dont_list_private_users_test() -> ok | {error, term()}.
dont_list_private_users_test() ->
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?USERS_URL, [],"application/json", "{\"username\" : \"test11\",\"private\":\"true\"}"}, [], []),
	DocId = lib_json:get_field(Body1,"_id"),
	api_help:refresh(),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(get, {?USERS_URL, []}, [], []),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(delete, {?USERS_URL++"/test11", []}, [], []),
	?assertEqual(false, lib_json:field_value_exists(Body2, "suggestions[*].private",<<"true">>)).

%% @doc
%% Function: dont_list_private_users_test/0
%% Purpose: Test that users with private set to true will not be shown on listing
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and deletes documents in elasticsearch
%% @end
-spec dont_search_private_users_test() -> ok | {error, term()}.
dont_search_private_users_test() ->
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?USERS_URL, [],"application/json", "{\"username\" : \"test_search1\",\"private\":\"true\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {?USERS_URL, [],"application/json", "{\"username\" : \"test_search2\",\"private\":\"false\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {?USERS_URL ++ "/_search", [],"application/json", "{\"username\" : \"test_search1\"}"}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {?USERS_URL ++ "/_search?username=test_search2", []}, [], []),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(delete, {?USERS_URL ++ "/test_search1", []}, [], []),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(delete, {?USERS_URL ++ "/test_search2", []}, [], []),
	
	?assertEqual(0, lib_json:get_field(Body3, "hits.total")),
	?assertEqual(false, lib_json:field_value_exists(Body4, "hits.hits[*]._source.private",<<"true">>)).

%% @doc
%% Function: delete_user_test/0
%% Purpose: Test the delete_resource function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and deletes documents in elasticsearch
%% @end
-spec delete_user_test() -> ok | {error, term()}.
delete_user_test() ->
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {?USERS_URL, [],"application/json", "{\"username\" : \"test22\"}"}, [], []),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {?USERS_URL, [],"application/json", "{\"username\" : \"test33\"}"}, [], []),
	DocId = lib_json:get_field(Body2,"_id"),
	DocId2 = lib_json:get_field(Body3,"_id"),
	api_help:refresh(),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(post, {?STREAMS_URL, [],"application/json", "{\"name\" : \"delete\",\"user_id\" : \"test33\"}"}, [], []),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(post, {?STREAMS_URL, [],"application/json", "{\"name\" : \"delete\",\"user_id\" : \"test33\"}"}, [], []),
	{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(post, {?STREAMS_URL, [],"application/json", "{\"name\" : \"delete\",\"user_id\" : \"test22\"}"}, [], []),
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(post, {?STREAMS_URL, [],"application/json", "{\"name\" : \"delete\",\"user_id\" : \"test22\"}"}, [], []),
	DocId4 = lib_json:get_field(Body5,"_id"),
	DocId5 = lib_json:get_field(Body6,"_id"),
	DocId6 = lib_json:get_field(Body7,"_id"),
	DocId7 = lib_json:get_field(Body8,"_id"),
	api_help:refresh(),
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, _Body9}} = httpc:request(post, {?STREAMS_URL ++ lib_json:to_string(DocId4) ++ "/data", [],"application/json", "{\"value\" : 2.0}"}, [], []),
	{ok, {{_Version10, 200, _ReasonPhrase10}, _Headers10, _Body10}} = httpc:request(post, {?STREAMS_URL ++ lib_json:to_string(DocId5) ++ "/data", [],"application/json", "{\"value\" : 2.0}"}, [], []),
	{ok, {{_Version11, 200, _ReasonPhrase11}, _Headers11, _Body11}} = httpc:request(post, {?STREAMS_URL ++ lib_json:to_string(DocId6) ++ "/data", [],"application/json", "{\"value\" : 2.0}"}, [], []),
	{ok, {{_Version12, 200, _ReasonPhrase12}, _Headers12, _Body12}} = httpc:request(post, {?STREAMS_URL ++ lib_json:to_string(DocId7) ++ "/data", [],"application/json", "{\"value\" : 2.0}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version13, 200, _ReasonPhrase13}, _Headers13, _Body13}} = httpc:request(delete, {?USERS_URL ++ "test22", []}, [], []),
	{ok, {{_Version22, 200, _ReasonPhrase22}, _Headers22, _Body22}} = httpc:request(delete, {?USERS_URL ++ "test33", []}, [], []),
	api_help:refresh(),
        timer:sleep(1000),
	{ok, {{_Version15, 200, _ReasonPhrase15}, _Headers15, Body15}} = httpc:request(get, {?USERS_URL++ "test22/streams", []}, [], []),
	{ok, {{_Version16, 200, _ReasonPhrase16}, _Headers16, Body16}} = httpc:request(get, {?USERS_URL++ "test33/streams", []}, [], []),
	{ok, {{_Version17, 200, _ReasonPhrase17}, _Headers17, Body17}} = httpc:request(get, {?STREAMS_URL ++ lib_json:to_string(DocId4) ++ "/data", []}, [], []),
	{ok, {{_Version18, 200, _ReasonPhrase18}, _Headers18, Body18}} = httpc:request(get, {?STREAMS_URL ++ lib_json:to_string(DocId5) ++ "/data", []}, [], []),
	{ok, {{_Version19, 200, _ReasonPhrase19}, _Headers19, Body19}} = httpc:request(get, {?STREAMS_URL ++ lib_json:to_string(DocId6) ++ "/data", []}, [], []),
	{ok, {{_Version20, 200, _ReasonPhrase20}, _Headers20, Body20}} = httpc:request(get, {?STREAMS_URL ++ lib_json:to_string(DocId7) ++ "/data", []}, [], []),
	% Delete a resource that doesn't exist
	{ok, {{_Version21, 404, _ReasonPhrase21}, _Headers21, _Body21}} = httpc:request(delete, {?USERS_URL++"idthatdoesntexist", []}, [], []),
	% Test that all children to the user is removed when it is deleted
	?assertEqual("{\"streams\":[]}",Body15),
	?assertEqual("{\"streams\":[]}",Body16),
	?assertEqual("{\"data\":[]}",Body17),
	?assertEqual("{\"data\":[]}",Body18),
	?assertEqual("{\"data\":[]}",Body19),
	?assertEqual("{\"data\":[]}",Body20).
	
%% @doc
%% Function: add_unsupported_field_test/0
%% Purpose: Test that unsuported fields are not allowed to be added 
%%          on create or update
%% Returns: ok | {error, term()}
%% @end
-spec add_unsupported_field_test() -> ok | {error, term()}.
add_unsupported_field_test() ->
	{ok, {{_Version1, 403, _ReasonPhrase1}, _Headers1, _Body1}} = httpc:request(post, {?USERS_URL, [],"application/json", "{\"test\":\"asdas\"}"}, [], []),
	{ok, {{_Version2, 403, _ReasonPhrase2}, _Headers2, _Body2}} = httpc:request(put, {?USERS_URL++"1", [],"application/json", "{\"test\":\"asdas\"}"}, [], []).


%% @doc
%% Function: delete_non_existing_user_test/0
%% Purpose: Checks user deletion when deleting non existing id
%% Returns: ok | {error, term()}
%%
%% @end
-spec delete_non_existing_user_test() -> ok | {error, term()}.
delete_non_existing_user_test() ->	
	Response1 = delete_request(?USERS_URL++"non-existing-key"),
	check_returned_code(Response1, 404).


%% @doc
%% Function: unique_username_test/0
%% Purpose: Test that a user can't have the same username as
%%          one already in the system
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and deletes documents in elasticsearch
%% @end
-spec unique_username_test() -> ok | {error, term()}.
unique_username_test() ->
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/users", [],"application/json", "{\"username\" : \"unique\",\"private\":\"true\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version2, 409, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users", [],"application/json", "{\"username\" : \"unique\",\"private\":\"true\"}"}, [], []),
	
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, _Body3}} = httpc:request(delete, {"http://localhost:8000/users/unique", []}, [], []),
	?assertEqual(Body2, "Non unique username given").

%% @doc
%% Function: username_exist_test/0
%% Purpose: Test that a user needs to have a username
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and deletes documents in elasticsearch
%% @end
-spec username_exist_test() -> ok | {error, term()}.
username_exist_test() ->
	{ok, {{_Version1, 403, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/users", [],"application/json", "{\"private\":\"true\"}"}, [], []),
	?assertEqual(Body1, "Username missing").

%% @doc
%% Function: get_index_id/0
%% Purpose: Searches the ES and finds the _id for a user
%% Returns: string() | {error, string()}
%%
%% @end
-spec get_index_id(string()) -> string() | {error, string()}.
get_index_id(Uname) ->
	Response1 = get_request(?USERS_URL ++ "_search?username="++Uname),
	check_returned_code(Response1, 200),
	{ok, {_,_,A}} = Response1,
	Response = lib_json:get_field(A, "hits.hits[0]._id"),
	case Response of
		undefined ->
			{error, "no match"};
		_ ->
			Response
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


post_request(URL, ContentType, Body) -> request(post, {URL, [], ContentType, Body}).
put_request(URL, ContentType, Body) -> request(put, {URL, [], ContentType, Body}).
get_request(URL)                     -> request(get,  {URL, []}).
delete_request(URL)                     -> request(delete,  {URL, []}).

request(Method, Request) ->
    httpc:request(Method, Request, [], []).

