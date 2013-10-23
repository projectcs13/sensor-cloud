%% @author Andreas Moregård <andreas.moregard@gmail.com>
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%%
%% @doc Tests for the resource module
%%  This module contains tests for the resource module
%%
%% @end

-module(resources_tests).
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
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"test\" : \"post\",\"user_id\" : \"abc\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users/abc/resources/", [],"application/json", "{\"test\" : \"post\",\"user_id\" : \"abc\"}"}, [], []),
	refresh(),
	?assertEqual("true",get_field_value(Body1,"ok")),	
	?assertEqual("true",get_field_value(Body2,"ok")),
	%Clean up after the test
	httpc:request(delete, {"http://localhost:8000/resources/" ++ get_id_value(Body1,"_id"), []}, [], []),
	httpc:request(delete, {"http://localhost:8000/resources/" ++ get_id_value(Body2,"_id"), []}, [], []).

%% @doc
%% Function: delete_resource_test/0
%% Purpose: Test the delete_resource_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and deletes documents in elasticsearch
%% @end
delete_resource_test() ->
	% Create a resource and two streams, then delete the resource and check if streams are automatically deleted
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"test\" : \"delete\",\"user_id\" : 1}"}, [], []),
	DocId = get_id_value(Body2,"_id"),
	refresh(),
	httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"delete\",\"user_id\" : \"1\", \"resource_id\" : \"" ++ DocId ++ "\"}"}, [], []),
	httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"delete\",\"user_id\" : \"1\", \"resource_id\" : \"" ++ DocId ++ "\"}"}, [], []),
	refresh(),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(delete, {"http://localhost:8000/resources/" ++ DocId, []}, [], []),
	refresh(),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {"http://localhost:8000/users/1/resources/"++DocId++"/streams", []}, [], []),
	% Delete a resource that doesn't exist
	{ok, {{_Version5, 500, _ReasonPhrase5}, _Headers5, _Body5}} = httpc:request(delete, {"http://localhost:8000/resources/1", []}, [], []),
	?assertEqual("true",get_field_value(Body2,"ok")),
	?assertEqual("true",get_field_value(Body3,"ok")),
	?assertEqual("{\"hits\":[]}",Body4).

	
%% @doc
%% Function: put_resource_test/0
%% Purpose: Test the put_resource_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and updatess a document in elasticsearch
%% @end
put_resource_test() ->
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/resources/", [],"application/json", "{\"test\" : \"put1\",\"user_id\" : \"0\"}"}, [], []),
	refresh(),
	DocId = get_id_value(Body1,"_id"),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(put, {"http://localhost:8000/resources/" ++ DocId , [],"application/json", "{\"test\" : \"put2\",\"user_id\" : \"0\"}"}, [], []),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {"http://localhost:8000/resources/" ++ DocId, []}, [], []),
	%Try to put to a resource that doesn't exist
	{ok, {{_Version4, 500, _ReasonPhrase4}, _Headers4, _Body4}} = httpc:request(put, {"http://localhost:8000/resources/derp", [],"application/json", "{\"test\" : \"put3\",\"user_id\" : \"0\"}"}, [], []),
	?assertEqual("true",get_field_value(Body1,"ok")),
	?assertEqual("true",get_field_value(Body2,"ok")),
	?assertEqual("put2",get_field_value(Body3,"test")),
	%Clean up
	httpc:request(delete, {"http://localhost:8000/resources/" ++ get_id_value(Body1,"_id"), []}, [], []).
	
%% @doc
%% Function: get_resource_test/0
%% Purpose: Test the get_resource_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and returns documents in elasticsearch
%% @end
get_resource_test() ->
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/resources/", [],"application/json", "{\"test\" : \"get\",\"user_id\" : 0}"}, [], []),
	refresh(),
	DocId = get_id_value(Body1,"_id"),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(get, {"http://localhost:8000/resources/" ++ DocId, []}, [], []),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {"http://localhost:8000/users/0/resources/" ++ DocId, []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {"http://localhost:8000/users/0/resources/_search?test=get", []}, [], []),
	%Get resource that doesn't exist
	{ok, {{_Version5, 500, _ReasonPhrase5}, _Headers5, _Body5}} = httpc:request(get, {"http://localhost:8000/resources/1" ++ DocId, []}, [], []),
	?assertEqual("get",get_field_value(Body2,"test")),
	?assertEqual("get",get_field_value(Body3,"test")),
	?assertEqual("get",get_field_value(Body4,"test")),
	%Clean up
	httpc:request(delete, {"http://localhost:8000/resources/" ++ get_id_value(Body1,"_id"), []}, [], []).

%% @doc
%% Function: get_value_field/2
%% Purpose: Return the value of a certain field in the given JSON string.
%% Returns: Return the value of the specified field, if it exists, 
%%          otherwise returns the empty string.
%% @end
-spec get_field_value(String::string(),Field::string()) -> string().

get_field_value(JSONString,Field) ->
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