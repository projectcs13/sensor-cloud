%% @author Tommy Mattsson <toma10293@student.uu.se>
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == json library_tests ==
%% This module contains several tests to test the functionallity
%% in the module lib_json for decoding json objects.
%%
%% @end
-module(lib_json_tests).
-include_lib("eunit/include/eunit.hrl").
-include("json.hrl").
-export([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% = = Test input = =
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(JSON1, 
	"{"
	"\"friend\":["
	"{\"name\":\"FriendName1\",\"nickname\":\"NickName1\"},"
	"{\"name\":\"FriendName2\",\"nickname\":[\"NickName2\",\"NickName3\"]},"
	"{\"name\":\"FriendName3\",\"nickname\":[\"NickName4\",\"NickName5\"]}"
	"],"
	"\"name\":\"Name1\""
	"}").

-define(JSON2, 
	"{"
	"\"name\":\"Name1\","
	"\"friend\":{\"name\":\"FriendName2\",\"nickname\":[\"NickName2\",\"NickName3\"]}"
	"}").

-define(JSON3, 
	"{\"took\":1,\"timed_out\":false,\"_shards\":{\"total\":5,\"successful\":5,\"failed\":0},\"hits\":{\"total\":0,\"max_score\":null,\"hits\":[]}}").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% = = Test desired input = =
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(JSON_RESULT1, 
	"{"
	"\"friend\":["
	"{\"name\":\"FriendName0\",\"nickname\":\"NickName0\"},"
	"{\"name\":\"FriendName1\",\"nickname\":\"NickName1\"},"
	"{\"name\":\"FriendName2\",\"nickname\":[\"NickName2\",\"NickName3\"]},"
	"{\"name\":\"FriendName3\",\"nickname\":[\"NickName4\",\"NickName5\"]}"
	"],"
	"\"name\":\"Name1\""
	"}").

-define(JSON_RESULT2, 
	"{"
	"\"friend\":["
	"{\"name\":\"FriendName1\",\"nickname\":\"NickName1\"},"
	"{\"name\":\"FriendName2\",\"nickname\":[\"NickName2\",\"NickName3\",\"NickName6\"]},"
	"{\"name\":\"FriendName3\",\"nickname\":[\"NickName4\",\"NickName5\"]}"
	"],"
	"\"name\":\"Name1\""
	"}").

-define(JSON_RESULT3, 
	"{"
	"\"friend\":["
	"{\"height\":180,\"name\":\"FriendName1\",\"nickname\":\"NickName1\"},"
	"{\"name\":\"FriendName2\",\"nickname\":[\"NickName2\",\"NickName3\"]},"
	"{\"name\":\"FriendName3\",\"nickname\":[\"NickName4\",\"NickName5\"]}"
	"],"
	"\"name\":\"Name1\""
	"}").
-define(JSON_RESULT4, 
	"{"
	"\"friend\":["
	"{\"height\":[180,182],\"name\":\"FriendName1\",\"nickname\":\"NickName1\"},"
	"{\"name\":\"FriendName2\",\"nickname\":[\"NickName2\",\"NickName3\"]},"
	"{\"name\":\"FriendName3\",\"nickname\":[\"NickName4\",\"NickName5\"]}"
	"],"
	"\"name\":\"Name1\""
	"}").


-define(JSON_RESULT5, 
	["{\"name\":\"FriendName1\",\"nickname\":\"NickName1\"}",
	"{\"name\":\"FriendName2\",\"nickname\":[\"NickName2\",\"NickName3\"]}",
	"{\"name\":\"FriendName3\",\"nickname\":[\"NickName4\",\"NickName5\"]}"
	]).

-define(JSON_RESULT6, 
	"{\"name\":\"FriendName1\",\"nickname\":\"NickName1\"}"
       ).

-define(JSON_RESULT7, 
	"{\"name\":\"poff\",\"nickname\":\"NickName1\"}"
       ).

-define(ENCODE_RESULT1, 
	[$\{,[$\",<<"friend">>,$\"],$:,[$[,
[${,[$\",<<"name">>,$\"],$:,[$\",<<"FriendName1">>,$\"],$,,[$\",<<"nickname">>,$\"],$:,[$\",<<"NickName1">>,$\"],$}],$,,
[${,[$\",<<"name">>,$\"],$:,[$\",<<"FriendName2">>,$\"],$,,[$\",<<"nickname">>,$\"],$:,[$[,[$\",<<"NickName2">>,$\"],$,,[$\",<<"NickName3">>,$\"],$]],$}],$,,
[${,[$\",<<"name">>,$\"],$:,[$\",<<"FriendName3">>,$\"],$,,[$\",<<"nickname">>,$\"],$:,[$[,[$\",<<"NickName4">>,$\"],$,,[$\",<<"NickName5">>,$\"],$]],$}]
,$]],
$,,[$\", <<"name">>,$\"],$:,[$\",<<"Name1">>,$\"],
$}]).

-define(DECODE_RESULT1, 
	{struct,[{<<"friend">>,
		  [{struct,[{<<"name">>,<<"FriendName1">>},
			    {<<"nickname">>,<<"NickName1">>}]},
		   {struct,[{<<"name">>,<<"FriendName2">>},
			    {<<"nickname">>,[<<"NickName2">>,<<"NickName3">>]}]},
		   {struct,[{<<"name">>,<<"FriendName3">>},
			    {<<"nickname">>,[<<"NickName4">>,<<"NickName5">>]}]}]},
		 {<<"name">>,<<"Name1">>}]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% = = Test functions = =
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% Purpose: Tests lib_json:add_value3
%% @doc
add_value_test() ->
    ?assertEqual("{\"attr1\":\"value1\"}", lib_json:add_value("{}", attr1, <<"value1">>)),

    %% For a regular string value the function does not recognize it, so 
    %% it needs to be defined as a binary like above
    ?assertEqual("{\"attr1\":\"value1\"}", lib_json:add_value("{}", attr1, <<"value1">>)),
    ?assertEqual(?JSON_RESULT1, lib_json:add_value(?JSON1, friend, "{\"name\":\"FriendName0\", \"nickname\":\"NickName0\"}")),
    ?assertEqual(?JSON_RESULT2, lib_json:add_value(?JSON1, "friend[1].nickname", <<"NickName6">>)),
    ?assertEqual(?JSON_RESULT3, lib_json:add_value(?JSON1, "friend[0].height", 180)),
    ?assertEqual(?JSON_RESULT4, lib_json:add_value(?JSON1, "friend[0].height", "[180,182]")),
    ?assertEqual(?JSON_RESULT4, lib_json:add_value(?JSON1, "friend[0].height", [180, 182])),

    %% If the field already exist and is not a list then no action is taken
    ?assertEqual(?JSON1, lib_json:add_value(?JSON1, name, <<"poff">>)),

    ?assertEqual(?JSON1, lib_json:add_value(?JSON1, "name.poff", <<"poff">>)).


add_values_test() ->
    true.

%% @doc
%% Purpose: Test the json_lib:decode/1
%% @end
decode_test() ->
    ?assertEqual(?DECODE_RESULT1, lib_json:decode(?JSON1)).

%% @doc
%% Purpose: Test the json_lib:encode/1
%% @end
encode_test() ->
    ?assertEqual(?ENCODE_RESULT1, lib_json:encode(?JSON1)).

%% @doc
%% Purpose: Test the json_lib:encode/1 and lib_json:decode/1 in combination
%% @end
encode_decode_test() ->
    ?assertEqual(?DECODE_RESULT1, lib_json:decode(lib_json:encode(?JSON1))),
    ?assertEqual(?ENCODE_RESULT1, lib_json:encode(lib_json:decode(lib_json:encode(?JSON1)))).

%% @doc
%% Purpose: Test the json_lib:field_value_exists/3
%% @end
field_value_exists_test() ->
    ?assertEqual(true, lib_json:field_value_exists(?JSON1, "friend[1].name", <<"FriendName2">>)),
    ?assertEqual(false, lib_json:field_value_exists(?JSON1, "friend[0].name", <<"FriendName2">>)),
    ?assertEqual(true, lib_json:field_value_exists(?JSON1, "friend[*].nickname", <<"NickName1">>)),
    ?assertEqual(true, lib_json:field_value_exists(?JSON1, "friend[*].nickname", <<"NickName3">>)),
    ?assertEqual(false, lib_json:field_value_exists(?JSON1, "friend[*].name", <<"NickName3">>)),
    ?assertEqual(false, lib_json:field_value_exists(?JSON2, "friend[*].name", <<"NickName3">>)),
    ?assertEqual(true, lib_json:field_value_exists(?JSON2, "friend.nickname", <<"NickName3">>)),
    ?assertEqual(true, lib_json:field_value_exists(?JSON1, "friend[0]", ?JSON_RESULT6)),
    ?assertEqual(true, lib_json:field_value_exists(?JSON1, "friend", ?JSON_RESULT5)).

%% @doc
%% Purpose: Tests lib_json:get_field/2
%% @end
get_field_test() ->
    ?assertEqual(<<"Name1">>, lib_json:get_field(?JSON1, "name")),
    ?assertEqual(?JSON_RESULT5, lib_json:get_field(?JSON1, "friend")),
    ?assertEqual(?JSON_RESULT6, lib_json:get_field(?JSON1, "friend[0]")),
    ?assertEqual(<<"FriendName1">>, lib_json:get_field(?JSON1, "friend[0].name")),
    ?assertEqual(<<"NickName1">>, lib_json:get_field(?JSON1, "friend[0].nickname")),
    ?assertEqual([<<"NickName2">>, <<"NickName3">>], lib_json:get_field(?JSON1, "friend[1].nickname")),
    ?assertEqual(<<"NickName2">>, lib_json:get_field(?JSON1, "friend[1].nickname[0]")),
    ?assertEqual(undefined, lib_json:get_field(?JSON1, "friend[0].nick")),
    ?assertEqual("{\"name\":\"FriendName2\",\"nickname\":[\"NickName2\",\"NickName3\"]}",
		 lib_json:get_field(?JSON2, "friend")),

    AddedField1 = lib_json:add_value(?JSON1, "friend[0].height", [1,2]),
    ?assertEqual([1,2], lib_json:get_field(AddedField1, "friend[0].height")),
    AddedField2 = lib_json:add_value(?JSON1, "friend[0].height", ["value1","value2"]),
    ?assertEqual([<<"value1">>,<<"value2">>], lib_json:get_field(AddedField2, "friend[0].height")).

%% @doc
%% Purpose: Tests json_lib:get_field_value/2 
%% @end
get_field_value_test() ->
    ?assertEqual(<<"FriendName2">>, lib_json:get_field_value(?JSON1, "friend[1].name", <<"FriendName2">>)),
    ?assertEqual(undefined, lib_json:get_field_value(?JSON1, "friend[0].name", <<"FriendName2">>)),
    ?assertEqual(<<"NickName1">>, lib_json:get_field_value(?JSON1, "friend[*].nickname", <<"NickName1">>)),
    ?assertEqual(<<"NickName3">>, lib_json:get_field_value(?JSON1, "friend[*].nickname", <<"NickName3">>)),
    ?assertEqual(undefined, lib_json:get_field_value(?JSON1, "friend[*].name", <<"NickName3">>)),
    ?assertEqual(undefined, lib_json:get_field_value(?JSON2, "friend[*].name", <<"NickName3">>)),
    ?assertEqual(<<"NickName3">>, lib_json:get_field_value(?JSON2, "friend.nickname", <<"NickName3">>)),
    ?assertEqual(?JSON_RESULT6, lib_json:get_field_value(?JSON1, "friend[0]", ?JSON_RESULT6)),
    ?assertEqual(?JSON_RESULT5, lib_json:get_field_value(?JSON1, "friend", ?JSON_RESULT5)),
    ?assertEqual(null, lib_json:get_field_value(?JSON3, "hits.max_score", null)),

    %% This call will produce an error. Added here as an example of how 
    %% lib_json:get_field_value/3 NOT should be used. The second argument is not
    %% allowed to end with [*]
    %% See lib_json:get_field_value/3 for details of how to use the function
    Try = try lib_json:get_field_value(?JSON1, "friend[*]", "") of
	      _ -> will_not_happen
	  catch
	      _:_ -> error
	  end,    
    ?assertEqual(error, Try).

replace_field_test() ->
    true.

rm_field_test() ->
    true.

set_attr_test() ->
    true.

set_attrs_test() ->
    true.

to_string_test() ->
    true.
