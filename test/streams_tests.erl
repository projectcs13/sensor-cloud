%% @author Tomas Sävström <tosa7943@student.uu.se>
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == streams_tests ==
%% This module contains several tests to test the functionallity
%% in the module streams which is done by calling the webbmachine.
%%
%% @end

-module(streams_tests).
-include_lib("eunit/include/eunit.hrl").
-export([]).

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
%% Function: get_stream_test/0
%% Purpose: Test the get_stream function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates a document in elasticsearch
%% @end
-spec get_stream_test() -> ok | {error, term()}.

get_stream_test() ->
	{ok, {{Version1, 200, ReasonPhrase1}, Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"get\",\"owner_id\" : 0, \"resource_id\" : 0}"}, [], []),
	DocId = get_field_value(Body1,"_id"),
	{ok, {{Version2, 200, ReasonPhrase1}, Headers2, Body2}} = httpc:request(get, {"http://localhost:8000/streams/" ++ DocId, []}, [], []),
	{ok, {{Version3, 200, ReasonPhrase2}, Headers3, Body3}} = httpc:request(get, {"http://localhost:8000/users/0/resources/0/streams", []}, [], []),
	{ok, {{Version4, 200, ReasonPhrase3}, Headers4, Body4}} = httpc:request(get, {"http://localhost:8000/streams/_search?owner_id=0", []}, [], []),
	{ok, {{Version5, 200, ReasonPhrase5}, Headers5, Body5}} = httpc:request(post, {"http://localhost:8000/streams/_search?test=get",[],"",""}, [], []),
	?assertEqual(true,string:str(Body2,"get") - string:str(Body2,"test") == 7),
	?assertEqual(true,string:str(Body3,"get") - string:str(Body3,"test") == 11),
	?assertEqual(true,string:str(Body4,"get") - string:str(Body4,"test") == 11),
	?assertEqual(true,string:str(Body5,"get") - string:str(Body5,"test") == 11).
	
%% @doc
%% Function: get_stream_test/0
%% Purpose: Test the put_stream function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 2 document in elasticsearch and updates them
%% @end
-spec put_stream_test() -> ok | {error, term()}.

put_stream_test() ->
	{ok, {{Version1, 200, ReasonPhrase1}, Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"test\" : \"get\"\n}"}, [], []),
	{ok, {{Version2, 200, ReasonPhrase2}, Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users/0/resources/0/streams", [], "application/json", "{\n\"test\" : \"get\"\n}"}, [], []),
	DocId1 = get_field_value(Body1,"_id"),
	DocId2 = get_field_value(Body2,"_id"),
	{ok, {{Version3, 200, ReasonPhrase3}, Headers3, Body3}} = httpc:request(put, {"http://localhost:8000/streams/" ++ DocId1, [], "application/json", "{\n\"test\" : \"put\"\n}"}, [], []),
	{ok, {{Version4, 200, ReasonPhrase4}, Headers4, Body4}} = httpc:request(put, {"http://localhost:8000/streams/" ++ DocId2, [], "application/json", "{\n\"test\" : \"put\"\n}"}, [], []),
	{ok, {{Version5, 200, ReasonPhrase5}, Headers5, Body5}} = httpc:request(get, {"http://localhost:8000/streams/" ++ DocId1, []}, [], []),
	{ok, {{Version6, 200, ReasonPhrase6}, Headers6, Body6}} = httpc:request(get, {"http://localhost:8000/streams/" ++ DocId2, []}, [], []),
	?assertEqual(true,string:str(Body5,"put") - string:str(Body5,"test") == 7),
	?assertEqual(true,string:str(Body6,"put") - string:str(Body6,"test") == 7),
	?assertEqual(true,string:str(Body5,"get") == 0),
	?assertEqual(true,string:str(Body6,"get") == 0).

%% @doc
%% Function: delete_stream_test/0
%% Purpose: Test the delete_stream function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 2 document in elasticsearch and deletes them
%% @end
-spec delete_stream_test() -> ok | {error, term()}.

delete_stream_test() ->
	{ok, {{Version1, 200, ReasonPhrase1}, Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"test\" : \"get\"\n}"}, [], []),
	{ok, {{Version2, 200, ReasonPhrase2}, Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users/0/resources/0/streams", [], "application/json", "{\n\"test\" : \"get\"\n}"}, [], []),
	DocId1 = get_field_value(Body1,"_id"),
	DocId2 = get_field_value(Body2,"_id"),
	{ok, {{Version3, 200, ReasonPhrase3}, Headers3, Body3}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId1, []}, [], []),
	{ok, {{Version4, 200, ReasonPhrase4}, Headers4, Body4}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId2, []}, [], []),
	{ok, {{Version5, 500, ReasonPhrase5}, Headers5, Body5}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId1, []}, [], []),
	{ok, {{Version6, 500, ReasonPhrase6}, Headers6, Body6}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId2, []}, [], []),
	?assertEqual(true,string:str(Body3,DocId1) =/= 0),
	?assertEqual(true,string:str(Body4,DocId2) =/= 0),
	?assertEqual(true,string:str(Body5,"not_found") =/= 0),
	?assertEqual(true,string:str(Body5,"not_found") =/= 0).


%% @doc
%% Function: delete_stream_and_datapoints_test/0
%% Purpose: Test if delete_stream function correctly deletes the streams datapoints
%% Returns: ok | {error, term()}
%% @end
-spec delete_stream_and_datapoints_test() -> ok | {error, term()}.
delete_stream_and_datapoints_test() ->
	?assertMatch({ok, {{Version1, 200, ReasonPhrase1}, Headers1, Body1}}, 
		httpc:request(post, {"http://localhost:8000/streams", 
							 [], 
							 "application/json", 
							 "{\n\"test\" : \"get\"\n}"}, [], [])),
	DocId1 = get_field_value(Body1,"_id"),
	?assertMatch({ok, {{Version2, 200, ReasonPhrase12, Headers2, Body2}} ,
		 httpc:request(post, {"http://localhost:8000/streams/"++DocId1++"/data", 
							  [], 
							  "application/json", 
							  "{\n\"streamid\" : "++ DocId1 ++"\n}"}, [], [])),
	?assertMatch({ok, {{Version3, 200, ReasonPhrase3}, Headers3, Body3}} , 
		 httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId1, []}, [], [])),
	?assertMatch({ok, {{_, 400, _}, _, _}} , 
		 httpc:request(get, {"http://localhost:8000/streams/" ++ DocId1++"/data", []}, [], [])).


	
%% @doc
%% Function: get_field_value/2
%% Purpose: Help function to find value of a field in the string
%% Returns: String with value of the field
%%
%% Side effects: creates 2 document in elasticsearch and deletes them
%% @end
-spec get_field_value(String::string(),Field::string()) -> string().
	
get_field_value(String,Field) ->
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