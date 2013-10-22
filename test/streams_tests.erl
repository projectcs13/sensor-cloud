%% @author Tomas S�vstr�m <tosa7943@student.uu.se>
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
	% Test create
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"get\",\"user_id\" : 0, \"resource_id\" : \"asdascvsr213sda\", \"private\" : \"true\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"get\",\"user_id\" : 0, \"resource_id\" : \"asdascvsr213sda\", \"private\" : \"true\"}"}, [], []),
	DocId1 = get_id_value(Body1,"_id"),
	DocId2 = get_id_value(Body2,"_id"),
	refresh(),
	% Test get and search
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {"http://localhost:8000/streams/" ++ DocId1, []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {"http://localhost:8000/users/0/resources/asdascvsr213sda/streams", []}, [], []),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(get, {"http://localhost:8000/streams/_search?user_id=0", []}, [], []),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(post, {"http://localhost:8000/streams/_search?test=get",[],"",""}, [], []),
	% Test get for missing index
	{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(get, {"http://localhost:8000/streams/1", []}, [], []),
	% Test delete
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId1, []}, [], []),
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId2, []}, [], []),

	?assertEqual(true,get_value_field(Body3,"test") == "get"),
	?assertEqual(true,get_value_field(Body3,"private") == "true"),
	?assertEqual(true,get_value_field(Body4,"test") == "get"),
	?assertEqual(true,get_value_field(Body5,"test") == "get"),
	?assertEqual(true,list_to_integer(get_value_field(Body5,"total")) >= 2), % Needed in case unempty elasticsearch
	?assertEqual(true,get_value_field(Body6,"test") == "get"),
	?assertEqual(true,list_to_integer(get_value_field(Body6,"total")) >= 2), % Needed in case unempty elasticsearch
	?assertEqual(true,string:str(Body7,"not_found") =/= 0),
	?assertEqual(true,get_id_value(Body8,"_id") == DocId1),
	?assertEqual(true,get_id_value(Body9,"_id") == DocId2).

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
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users/0/resources/asdascvsr213sda/streams", [], "application/json", "{\n\"test\" : \"get\",\n\"private\" : \"true\"\n}}"}, [], []),
	DocId1 = get_id_value(Body1,"_id"),
	DocId2 = get_id_value(Body2,"_id"),
	refresh(),
	% Test update
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, _Body3}} = httpc:request(put, {"http://localhost:8000/streams/" ++ DocId1, [], "application/json", "{\n\"test\" : \"put\",\n\"private\" : \"false\"\n}"}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, _Body4}} = httpc:request(put, {"http://localhost:8000/streams/" ++ DocId2, [], "application/json", "{\n\"test\" : \"put\"\n}"}, [], []),
	% Test get

	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(get, {"http://localhost:8000/streams/" ++ DocId1, []}, [], []),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(get, {"http://localhost:8000/streams/" ++ DocId2, []}, [], []),
	% Test delete
	{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId1, []}, [], []),
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId2, []}, [], []),
	% Test update on missing doc
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(put, {"http://localhost:8000/streams/1", [], "application/json", "{\n\"test\" : \"put\"\n}"}, [], []),
	?assertEqual(true,get_value_field(Body5,"private") == "false"),
	?assertEqual(true,get_value_field(Body5,"private") =/= "true"),
	?assertEqual(true,get_value_field(Body6,"private") =/= "false"),
	?assertEqual(true,get_value_field(Body6,"private") == "true"),
	?assertEqual(true,get_value_field(Body5,"test") == "put"),
	?assertEqual(true,get_value_field(Body5,"test") =/= "get"),
	?assertEqual(true,get_value_field(Body6,"test") == "put"),
	?assertEqual(true,get_value_field(Body6,"test") =/= "get"),
	?assertEqual(true,get_id_value(Body7,"_id") == DocId1),
	?assertEqual(true,get_id_value(Body8,"_id") == DocId2),
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
	DocId1 = get_id_value(Body1,"_id"),
	DocId2 = get_id_value(Body2,"_id"),
	refresh(),
	% Test delete
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId1, []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId2, []}, [], []),
	% Test delete on missing index
	{ok, {{_Version5, 500, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId1, []}, [], []),
	{ok, {{_Version6, 500, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ DocId2, []}, [], []),
	?assertEqual(true,get_id_value(Body3,"_id") == DocId1),
	?assertEqual(true,get_id_value(Body4,"_id") == DocId2),
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
%% Function: get_value_field/2
%% Purpose: Return the value of a certain field in the given JSON string.
%% Returns: Return the value of the specified field, if it exists, 
%%          otherwise returns the empty string.
%% @end
-spec get_value_field(String::string(),Field::string()) -> string().

get_value_field(JSONString,Field) ->
	Tokens = string:tokens(JSONString, ","),
	ResourceField = find_field(Tokens,Field),
	case ResourceField of 
		[] -> "";
		_ -> FieldValue = string:tokens(ResourceField,":"),
			 remove_special_characters(lists:nth(length(FieldValue),FieldValue),false)
	end.

%% @doc
%% Function: find_field/2
%% Purpose: Help function to find the first string containing the given string
%%          in the list.
%% Returns: The first string containing the given string
%%          in the list, the empty string if non exists.
%% @end
-spec find_field(List::list(),Field::string()) -> string().

find_field([],_) ->
	[];

find_field([First|Rest],Field) ->
	case string:str(First,Field) of
		0 -> find_field(Rest,Field);
		_ -> First
	end.


%% @doc
%% Function: remove_special_characters/2
%% Purpose: Help function to remove non alphanumerical characters
%% Returns: First string of alphanumerical characters that can be found,
%%          empty string if non exists
%% @end
-spec remove_special_characters(String::string(),CharactersFound::boolean()) -> string().

remove_special_characters([],_) ->
	[];

remove_special_characters([First|Rest],false) ->
	Character = (First < 91) and (First > 64) or (First < 123) and (First > 96) or (First > 47) and (First < 58),
	case Character of
		true ->
			[First|remove_special_characters(Rest,true)];
		false ->
			remove_special_characters(Rest,false)
	end;
remove_special_characters([First|Rest],true) ->
	Character = (First < 91) and (First > 64) or (First < 123) and (First > 96) or (First > 47) and (First < 58),
	case Character of
		true ->
			[First|remove_special_characters(Rest,true)];
		false ->
			[]
	end.


		
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