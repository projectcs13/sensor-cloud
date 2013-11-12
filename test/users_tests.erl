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

-define(USERS_URL, "http://localhost:8000/users/").
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
					 "{\"user_name\":\""++?TEST_NAME++"\"}"),
	check_returned_code(Response1, 200),
	refresh(),
	?assertNotMatch({error, "no match"}, get_index_id(?TEST_NAME)).


%% @doc
%% Function: get_existing_user_test/0
%% Purpose: Test a get request for a user that exists
%% Returns: ok | {error, term()}
%%
%% @end
-spec get_existing_user_test() -> ok | {error, term()}.
get_existing_user_test() ->
	Id = get_index_id(?TEST_NAME),
	?assertNotMatch({error, "no match"}, Id),
	Response1 = get_request(?USERS_URL ++ lib_json:to_string(Id)),
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
	Response1 = get_request(?USERS_URL ++ "_search?user_name="++?TEST_NAME),
	check_returned_code(Response1, 200),
	{ok, Rest} = Response1,
	{_,_,A} = Rest,
	?assertEqual(true, lib_json:field_value_exists(A, "hits.hits[*]._source.user_name", <<?TEST_NAME>>)).


%% @doc
%% Function: post_user_search_test/0
%% Purpose: Perform a POST search query
%% Returns: ok | {error, term()}
%%
%% @end
-spec post_user_search_test() -> ok | {error, term()}.
post_user_search_test() ->	
	Response1 = post_request(?USERS_URL ++ "_search", "application/json", 
					 "{\"user_name\":\""++?TEST_NAME++"\"}"),
	check_returned_code(Response1, 200),
	{ok, Rest} = Response1,
	{_,_,A} = Rest,
	?assertEqual(true, lib_json:field_value_exists(A, "hits.hits[*]._source.user_name", <<?TEST_NAME>>)).



%% @doc
%% Function: put_user_search_test/0
%% Purpose: Checks if PUT requests work
%% Returns: ok | {error, term()}
%%
%% @end
-spec put_user_search_test() -> ok | {error, term()}.
put_user_search_test() ->	
	Id = get_index_id(?TEST_NAME),
	?assertNotMatch({error, "no match"}, Id),
	Response1 = put_request(?USERS_URL++lib_json:to_string(Id), "application/json", "{\"user_name\":\""++?TEST_NAME++"\","++
						"\"email\":\""++ ?TEST_EMAIL++"\"}"),
	check_returned_code(Response1, 200),
	Response2 = get_request(?USERS_URL ++ lib_json:to_string(Id)),
	{ok, Rest} = Response2,
	{_,_,A} = Rest,
	?assertEqual(true, lib_json:field_value_exists(A, "email", <<?TEST_EMAIL>>)).




%% @doc
%% Function: delete_user_test/0
%% Purpose: Test the delete_resource function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and deletes documents in elasticsearch
%% @end
-spec delete_user_test() -> ok | {error, term()}.
delete_user_test() ->
	% Create a resource and two streams, then delete the resource and check if streams are automatically deleted
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users", [],"application/json", "{\"name\" : \"test\"}"}, [], []),
	DocId = lib_json:get_field(Body2,"_id"),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"test\" : \"delete\",\"user_id\" : \"" ++ lib_json:to_string(DocId) ++ "\"}"}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"test\" : \"delete\",\"user_id\" : \"" ++ lib_json:to_string(DocId) ++ "\"}"}, [], []),
	DocId2 = lib_json:get_field(Body3,"_id"),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"delete\",\"resource_id\" : \"" ++ lib_json:to_string(DocId2) ++ "\"}"}, [], []),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"delete\",\"resource_id\" : \"" ++ lib_json:to_string(DocId2) ++ "\"}"}, [], []),
	DocId3 = lib_json:get_field(Body4,"_id"),
	{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"delete\",\"resource_id\" : \"" ++ lib_json:to_string(DocId3) ++ "\"}"}, [], []),
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"delete\",\"resource_id\" : \"" ++ lib_json:to_string(DocId3) ++ "\"}"}, [], []),
	refresh(),
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(delete, {"http://localhost:8000/users/" ++ lib_json:to_string(DocId), []}, [], []),
	refresh(),
	{ok, {{_Version10, 200, _ReasonPhrase10}, _Headers10, Body10}} = httpc:request(get, {"http://localhost:8000/users/"++ lib_json:to_string(DocId) ++"/resources", []}, [], []),
	{ok, {{_Version11, 200, _ReasonPhrase11}, _Headers11, Body11}} = httpc:request(get, {"http://localhost:8000/users/"++ lib_json:to_string(DocId) ++"/resources/"++ lib_json:to_string(DocId2) ++ "/streams", []}, [], []),
	{ok, {{_Version12, 200, _ReasonPhrase12}, _Headers12, Body12}} = httpc:request(get, {"http://localhost:8000/users/"++ lib_json:to_string(DocId) ++"/resources/"++ lib_json:to_string(DocId3) ++ "/streams", []}, [], []),
	% Delete a resource that doesn't exist
	{ok, {{_Version13, 404, _ReasonPhrase13}, _Headers13, Body13}} = httpc:request(delete, {"http://localhost:8000/users/idthatdoesntexist", []}, [], []),
	?assertEqual("{\"resources\":[]}",Body10),
	?assertEqual("{\"streams\":[]}",Body11),
	?assertEqual("{\"streams\":[]}",Body12).

	



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
%% Function: get_index_id/0
%% Purpose: Searches the ES and finds the _id for a user
%% Returns: string() | {error, string()}
%%
%% @end
-spec get_index_id(string()) -> string() | {error, string()}.
get_index_id(Uname) ->
	Response1 = get_request(?USERS_URL ++ "_search?user_name="++Uname),
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

%% @doc
%% Function: refresh/0
%% Purpose: Help function to find refresh the sensorcloud index
%% Returns: {ok/error, {{Version, Code, Reason}, Headers, Body}}
%% @end
refresh() ->
	httpc:request(post, {"http://localhost:9200/sensorcloud/_refresh", [],"", ""}, [], []).
