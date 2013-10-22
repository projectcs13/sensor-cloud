%% @author Andreas Moregård <andreas.moregard@gmail.com>
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%%
%% @doc Tests for the resource module
%%  This module contains tests for the resource module
%%
%% @end

-module(resource_tests).
-include_lib("eunit/include/eunit.hrl").

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
%% Function: process_post_test/0
%% Purpose: Test the process_post_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates documents in elasticsearch
%% @end
process_post_test() ->
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"test\" : \"post\",\"owner\" : \"0\",\"streams\" : \"1\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users/0/resources/", [],"application/json", "{\"test\" : \"post\",\"owner\" : \"0\",\"streams\" : \"1\"}"}, [], []),
	timer:sleep(100),
	?assertEqual(true,lib_json:get_field(Body1,"ok")),	
	?assertEqual(true,lib_json:get_field(Body2,"ok")).

%% @doc
%% Function: delete_resource_test/0
%% Purpose: Test the delete_resource_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and deletes documents in elasticsearch
%% @end
delete_resource_test() ->
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} =
	httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"test\" : \"delete\",\"owner\" : \"1\"}"}, [], []),
	timer:sleep(100),
	DocId = get_id_value(Body2,"_id"),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} =
	httpc:request(delete, {"http://localhost:8000/resources/" ++ DocId, []}, [], []),
	?assertEqual(true,lib_json:get_field(Body2,"ok")),
	?assertEqual(true,lib_json:get_field(Body3,"ok")).
	
%% @doc
%% Function: put_resource_test/0
%% Purpose: Test the put_resource_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and updatess a document in elasticsearch
%% @end
put_resource_test() ->
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/resources/", [],"application/json", "{\"test\" : \"put1\",\"owner\" : \"0\",\"streams\" : \"1\"}"}, [], []),
	timer:sleep(100),
	DocId = get_id_value(Body1,"_id"),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(put, {"http://localhost:8000/resources/" ++ DocId , [],"application/json", "{\"doc\" :{\"test\" : \"put2\",\"owner\" : \"0\",\"streams\" : \"1\"}}"}, [], []),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {"http://localhost:8000/resources/" ++ DocId, []}, [], []),
	?assertEqual(true,lib_json:get_field(Body1,"ok")),
	?assertEqual(true,lib_json:get_field(Body2,"ok")),
	?assertEqual("put2",lib_json:get_field(Body3,"_source.test")).
	
%% @doc
%% Function: get_resource_test/0
%% Purpose: Test the get_resource_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and returns documents in elasticsearch
%% @end
get_resource_test() ->
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/resources/", [],"application/json", "{\"test\" : \"get\",\"owner\" : \"0\"}"}, [], []),
	timer:sleep(800),
	DocId = get_id_value(Body1,"_id"),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(get, {"http://localhost:8000/resources/" ++ DocId, []}, [], []),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {"http://localhost:8000/users/0/resources/" ++ DocId, []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {"http://localhost:8000/users/0/resources/_search?test=get", []}, [], []),
	?assertEqual("get",lib_json:get_field(Body2,"_source.test")),
	?assertEqual("get",lib_json:get_field(Body3,"_source.test")),
	?assertEqual(true,lib_json:field_value_exists(Body4,"hits.hits[*]._source.test","get")).


%% @doc
%% Function: get_id_value/2
%% Purpose: Help function to find value of a field in the string
%% Returns: String with value of the field
%% @end
-spec get_id_value(String::string(),Field::string()) -> string().

get_id_value(String,Field) ->
	Location = string:str(String,Field),
	Start = Location + 3 + length(Field),
	RestOfString = string:substr(String, Start),
	NextComma = string:str(RestOfString,","),
	NextBracket = string:str(RestOfString,"}"),
	case (NextComma < NextBracket) and (NextComma =/= 0) of
		true ->
			string:substr(RestOfString, 1,NextComma-2);
		false ->
			string:substr(RestOfString, 1,NextBracket-2)
	end.

