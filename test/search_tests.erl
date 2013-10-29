%% @author Jose Arias
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == streams_tests ==
%% This module contains several tests to test the functionallity
%% in the module streams which is done by calling the webbmachine.
%%
%% @end

-module(search_tests).
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
%% Function: process_search_post_test/0
%% Purpose: Test the process_post_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%% @end
process_search_post_test() ->
        {ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"search\",\"resource_id\" : \"0\", \"private\" : \"false\"}"}, [], []),
        {ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"search\",\"resource_id\" : \"0\", \"private\" : \"true\"}"}, [], []),
        DocId1 = lib_json:get_field(Body1,"_id"),
        DocId2 = lib_json:get_field(Body2,"_id"),
        timer:sleep(1000),
        {ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {"http://localhost:8000/_search", [],"application/json", "{\"query\":{\"match_all\":{}}}"}, [], []),
        {ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId1, []}, [], []),
        {ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId2, []}, [], []),
        ?assertEqual(true,lib_json:get_field(Body3,"streams.hits.total") >= 1).


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
