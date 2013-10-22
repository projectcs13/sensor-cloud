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
%% Function: post_test/0
%% Purpose: Test a post request
%% Returns: ok | {error, term()}
%%
%% @end
-spec post_test() -> ok | {error, term()}.
post_test() ->
	Response1 = post_request(?USERS_URL, "application/json", 
					 "{\"user_name\":\""++?TEST_NAME++"\"}"),
	check_returned_code(Response1, 204),
	timer:sleep(100),
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
	Response1 = get_request(?USERS_URL ++ Id),
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
	?assertMatch({match, _}, re:run(A, "\"user_name\":\""++?TEST_NAME++"\"", [{capture, first, list}])).


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
	?assertMatch({match, _}, re:run(A, "\"user_name\":\""++?TEST_NAME++"\"", [{capture, first, list}])).


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
	Response1 = put_request(?USERS_URL++Id, "application/json", "{\"user_name\":\""++?TEST_NAME++"\","++
						"\"email\":\""++ ?TEST_EMAIL++"\"}"),
	check_returned_code(Response1, 204),
	Response2 = get_request(?USERS_URL ++ Id),
	{ok, Rest} = Response2,
	{_,_,A} = Rest,
	?assertMatch({match, _}, re:run(A, "\"email\":\""++?TEST_EMAIL++"\"", [{capture, first, list}])).


%% @doc
%% Function: delete_user_test/0
%% Purpose: Checks user deletion
%% Returns: ok | {error, term()}
%%
%% @end
-spec delete_user_test() -> ok | {error, term()}.
delete_user_test() ->	
	Id = get_index_id(?TEST_NAME),
	?assertNotMatch({error, "no match"}, Id),
	Response1 = delete_request(?USERS_URL++Id),
	check_returned_code(Response1, 204),
	
	Response2 = get_request(?USERS_URL ++ Id),
	check_returned_code(Response2, 404).


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
	{ok, Rest} = Response1,
	{_,_,A} = Rest,
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


post_request(URL, ContentType, Body) -> request(post, {URL, [], ContentType, Body}).
put_request(URL, ContentType, Body) -> request(put, {URL, [], ContentType, Body}).
get_request(URL)                     -> request(get,  {URL, []}).
delete_request(URL)                     -> request(delete,  {URL, []}).

request(Method, Request) ->
    httpc:request(Method, Request, [], []).
