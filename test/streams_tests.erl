%% @author Tomas Sävström <tosa7943@student.uu.se>
%% [www.csproj13.student.it.uu.se]
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
-include_lib("misc.hrl").
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
%% Function: process_search_post_test/0
%% Purpose: Test the process_post_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%% @end
process_search_post_test() ->
        {ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"search\",\"resource_id\" : \"0\", \"private\" : \"false\"}"}, [], []),
        {ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"search\",\"resource_id\" : \"0\", \"private\" : \"true\"}"}, [], []),
        DocId1 = ?TO_STRING(lib_json:get_field(Body1,"_id")),
        DocId2 = lib_json:to_string(lib_json:get_field(Body2,"_id")),
        timer:sleep(1000),
        {ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {"http://localhost:8000/streams/_search", [],"application/json", "{\"query\":{\"match_all\":{}}}"}, [], []),
        {ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId1, []}, [], []),
        {ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId2, []}, [], []),
        ?assertEqual(true,lib_json:get_field(Body3,"hits.total") >= 1).

%% @doc
%% Function: get_stream_test/0
%% Purpose: Test the get_stream function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates a document in elasticsearch
%% @end
-spec get_stream_test() -> ok | {error, term()}.

get_stream_test() ->
	% Test create

	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"get\",\"user_id\" : \"0\", \"resource_id\" : \"asdascvsr213sda\", \"private\" : \"false\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"get\",\"user_id\" : \"0\", \"resource_id\" : \"asdascvsr213sda\", \"private\" : \"false\"}"}, [], []),
	DocId1 = lib_json:get_field(Body1,"_id"),
	DocId2 = lib_json:get_field(Body2,"_id"),
	refresh(),
	% Test get and search
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {"http://localhost:8000/users/0/resources/asdascvsr213sda/streams", []}, [], []),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(get, {"http://localhost:8000/streams/_search?user_id=0", []}, [], []),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(post, {"http://localhost:8000/streams/_search",[],"application/json", "{\"query\":{\"term\" : { \"test\" : \"get\" }}}"}, [], []),


	% Test get for missing index
	{ok, {{_Version7, 500, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(get, {"http://localhost:8000/streams/1", []}, [], []),
	% Test delete
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	?assertEqual(<<"get">>,lib_json:get_field(Body3,"test")),

	?assertEqual(true,lib_json:get_field(Body3,"private") == <<"false">>),
	?assertEqual(true,lib_json:field_value_exists(Body4,"hits[*].test", <<"get">>)),
	?assertEqual(true,lib_json:field_value_exists(Body5,"hits.hits[*]._source.test", <<"get">>)),
	?assertEqual(true,lib_json:get_field(Body5,"hits.total") >= 2), % Needed in case unempty elasticsearch
	?assertEqual(true,lib_json:field_value_exists(Body5,"hits.hits[*]._source.test", <<"get">>)),
	?assertEqual(true,lib_json:get_field(Body6,"hits.total") >= 2), % Needed in case unempty elasticsearch
	?assertEqual(true,string:str(Body7,"not_found") =/= 0),
	?assertEqual(true,lib_json:get_field(Body8,"_id") == DocId1),
	?assertEqual(true,lib_json:get_field(Body9,"_id") == DocId2).

%% @doc
%% Function: get_stream_test/0
%% Purpose: Test the put_stream function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 2 document in elasticsearch and updates them
%% @end
-spec put_stream_test() -> ok | {error, term()}.

put_stream_test() ->
	% Test create
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"test\" : \"get\",\n\"private\" : \"true\"\n, \"resource_id\" : \"asdascvsr213sda\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users/0/resources/asdascvsr213sda/streams", [], "application/json", "{\n\"test\" : \"get\",\n\"private\" : \"true\"\n}"}, [], []),
	DocId1 = lib_json:get_field(Body1,"_id"),
	DocId2 = lib_json:get_field(Body2,"_id"),
	refresh(),
	% Test update
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, _Body3}} = httpc:request(put, {"http://localhost:8000/streams/" ++ ?TO_STRING(DocId1), [], "application/json", "{\n\"test\" : \"put\",\n\"private\" : \"false\"\n}"}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, _Body4}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId2), [], "application/json", "{\n\"test\" : \"put\"\n}"}, [], []),
	% Test get

	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(get, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(get, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	% Test delete
	{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	% Test update on missing doc
	{ok, {{_Version9, 500, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(put, {"http://localhost:8000/streams/1", [], "application/json", "{\n\"test\" : \"put\"\n}"}, [], []),
	?assertEqual(<<"false">>,lib_json:get_field(Body5,"private")),
	?assertEqual(true,lib_json:get_field(Body5,"private") =/= <<"true">>),
	?assertEqual(true,lib_json:get_field(Body6,"private") =/= <<"false">>),
	?assertEqual(true,lib_json:get_field(Body6,"private") == <<"true">>),
	?assertEqual(true,lib_json:get_field(Body5,"test") == <<"put">>),
	?assertEqual(true,lib_json:get_field(Body5,"test") =/= <<"get">>),
	?assertEqual(true,lib_json:get_field(Body6,"test") == <<"put">>),
	?assertEqual(true,lib_json:get_field(Body6,"test") =/= <<"get">>),
	?assertEqual(true,lib_json:get_field(Body7,"_id") == DocId1),
	?assertEqual(true,lib_json:get_field(Body8,"_id") == DocId2),
	?assertEqual(true,string:str(Body9,"not_found") =/= 0).

%% @doc
%% Function: delete_stream_test/0
%% Purpose: Test the delete_stream function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 2 document in elasticsearch and deletes them
%% @end
-spec delete_stream_test() -> ok | {error, term()}.

delete_stream_test() ->
	% Test create
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"test\" : \"get\"\n, \"resource_id\" : \"asdascvsr213sda\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users/0/resources/asdascvsr213sda/streams", [], "application/json", "{\n\"test\" : \"get\"\n}"}, [], []),
	DocId1 = lib_json:get_field(Body1,"_id"),
	DocId2 = lib_json:get_field(Body2,"_id"),
        erlang:display("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"),
        erlang:display(DocId2),
	refresh(),
	% Test delete
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ ?TO_STRING(DocId1), []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	% Test delete on missing index
	{ok, {{_Version5, 500, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version6, 500, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	?assertEqual(true,lib_json:get_field(Body3,"_id") == DocId1),
	?assertEqual(true,lib_json:get_field(Body4,"_id") == DocId2),
	?assertEqual(true,string:str(Body5,"not_found") =/= 0),
	?assertEqual(true,string:str(Body6,"not_found") =/= 0).

%% @doc
%% Function: create_doc_without_resource_test/0
%% Purpose: Test that error is given when creating stream without resource
%% Returns: ok | {error, term()}
%%
%% @end
-spec create_doc_without_resource_test() -> ok | {error, term()}.

create_doc_without_resource_test() ->
	{ok, {{_Version1, 500, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"test\" : \"get\"\n}"}, [], []),
 	{ok, {{_Version2, 500, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users/0/streams", [], "application/json", "{\n\"test\" : \"get\"\n}"}, [], []),
	?assertEqual(true,string:str(Body1,"resource_id_missing") =/= 0),
	?assertEqual(true,string:str(Body2,"resource_id_missing") =/= 0).
		
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

	%% @doc
%% Function: refresh/0
%% Purpose: Help function to find refresh the sensorcloud index
%% Returns: {ok/error, {{Version, Code, Reason}, Headers, Body}}
%% @end
refresh() ->
	httpc:request(post, {"http://localhost:9200/sensorcloud/_refresh", [],"", ""}, [], []).
