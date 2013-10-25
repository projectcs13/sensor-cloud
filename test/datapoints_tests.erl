%% @author Iakovos Koutsoumpakis
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == datapoints_tests ==
%% This module contains several tests to test the functionallity
%% in the restful API in users.
%%
%% @end

-module(datapoints_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% ====================================================================
%% Internal functions
%% ====================================================================

-define(DATAPOINTS_URL, "http://localhost:8000/streams/1/data/").
-define(TEST_VALUE, "test_value").
-define(TEST_TIMESTAMP, "test_timestamp").

%% @doc
%% Function: post_test/0
%% Purpose: Test a post request
%% Returns: ok | {error, term()}
%%
%% @end
-spec post_test() -> ok | {error, term()}.
post_test() ->
        Response1 = post_request(?DATAPOINTS_URL, "application/json",
                                         "{\"datapoint_value\":\""++?TEST_VALUE++"\"}"),
        check_returned_code(Response1, 204),
        timer:sleep(2000),
        ?assertNotMatch({error, "no match"}, get_index_id(?TEST_VALUE)).


%% @doc
%% Function: get_existing_datapoint_test/0
%% Purpose: Test a get request for a datapoint that exists, using its Id
%% Returns: ok | {error, term()}
%%
%% @end
-spec get_existing_datapoint_test() -> ok | {error, term()}.
get_existing_datapoint_test() ->
        Id = get_index_id(?TEST_VALUE),
        ?assertNotMatch({error, "no match"}, Id),
        Response1 = get_request(?DATAPOINTS_URL ++ "_search?_id="++ Id),
        check_returned_code(Response1, 200).


%% @doc
%% Function: get_index_id/0
%% Purpose: Searches the ES and returns the _id of a datapoint
%% Returns: string() | {error, string()}
%%
%% @end
-spec get_index_id(string()) -> string() | {error, string()}.
get_index_id(Uvalue) ->
        Response1 = get_request(?DATAPOINTS_URL ++ "_search?datapoint_value="++Uvalue),
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


%% @doc
%% Function: get_non_existent_user_datapoint/0
%% Purpose: Test a get request for a datapoint that doesn't exist
%% Returns: ok | {error, term()}
%%
%% @end
-spec get_non_existent_datapoint_test() -> ok | {error, term()}.
get_non_existent_datapoint_test() ->
        Response1 = get_request(?DATAPOINTS_URL ++ "_search?_id=" ++ "nonexistent"),
		{ok, Rest} = Response1,
		{_,_,Result} = Rest,
		%JSONString=json_encode(Result),
	    ?assertNotEqual(0, string:str(Result, "max_score\":null")).


%% @doc
%% Function: get_value_field/2
%% Purpose: Return the value of a certain field in the given JSON string.
%% Returns: Return the value of the specified field, if it exists,
%% otherwise returns the empty string.
%% @end
-spec get_value_field(String::string(),Field::string()) -> string().

get_value_field(JSONString2,Field) ->
	JSONString=json_encode(JSONString2),
	case re:run(JSONString, "\"max_score\":null", [{capture, first, list}]) of
							{match, _} -> erlang:display("not fuond!!!!!##############");
							nomatch -> erlang:display("Found :( ##############)")
						end.

   %  end.



%% @doc
%% Function: json_encode/2
%% Purpose: To encode utf8-json WITHOUT converting multi-byte utf8-chars into ASCII '\uXXXX'.
%% Returns: A string with fields and values formatted in a correct way
%% @end
-spec json_encode(string()) -> string().
json_encode(Data) ->
    (mochijson2:encoder([{utf8, true}]))(Data).



%% @doc
%% Function: remove_special_characters/2
%% Purpose: Help function to remove non alphanumerical characters
%% Returns: First string of alphanumerical characters that can be found,
%% empty string if non exists
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
%% Function: find_field/2
%% Purpose: Help function to find the second string containing the given string
%% in the list.
%% Returns: The second string containing the given string
%% in the list, the empty string if non exists.
%% @end
-spec find_field(List::list(),Field::string()) -> string().

find_field([],_) ->
        [];

find_field([First|Rest],Field) ->
        case string:str(First,Field) of
                0 -> find_second_field(Rest,Field);
                _ -> First
        end.

-spec find_second_field(List::list(),Field::string()) -> string().

find_second_field([First|Rest],Field) ->
        case string:str(First,Field) of
                0 -> find_field(Rest,Field);
                _ -> First
        end.




post_request(URL, ContentType, Body) -> request(post, {URL, [], ContentType, Body}).
get_request(URL) -> request(get, {URL, []}).
request(Method, Request) ->
    httpc:request(Method, Request, [], []).