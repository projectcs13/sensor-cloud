%% @author Georgios Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == users_resource_tests ==
%% This module contains several tests to test the functionallity
%% in the restful API in users_resource.
%%
%% @end

-module(users_resource_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% ====================================================================
%% Internal functions
%% ====================================================================

-define(Url, "http://localhost:8000/users/").

%% @doc
%% Function: get_user_test/0
%% Purpose: 
%% Returns: ok | {error, term()}
%%
%% @end
-spec get_user_test() -> ok | {error, term()}.
get_user_test() ->
	A = post("http://localhost:8000/users", "application/json", "{\"user_name\":\"weird_name\"}"),
	erlang:display(A).
	
	
	?assertEqual(ok, db_api:start()).

post(URL, ContentType, Body) -> request(post, {URL, [], ContentType, Body}).
put(URL, ContentType, Body) -> request(put, {URL, [], ContentType, Body}).
get(URL)                     -> request(get,  {URL, []}).
head(URL)                    -> request(head, {URL, []}).

request(Method, Request) ->
    httpc:request(Method, Request, [], []).

