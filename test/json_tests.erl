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
-module(json_tests).
-include_lib("eunit/include/eunit.hrl").
-include("json.hrl").
-export([]).

-define(JSON1, 
	"{"
	"\"name\":\"Name1\","
	"\"friend\": ["
	"{\"name\":\"FriendName1\", \"nickname\":\"NickName1\"},"
	"{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]},"
	"{\"name\":\"FriendName3\", \"nickname\":[\"NickName4\", \"NickName5\"]}"
	"]"
	"}").

-define(JSON2, 
	"{"
	"\"name\":\"Name1\","
	"\"friend\": {\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
	"}").

-define(GET_FIELD_RESULT1, 
	[[{"name","FriendName1"},{"nickname","NickName1"}],
	 [{"name","FriendName2"},{"nickname",["NickName2","NickName3"]}],
	 [{"name","FriendName3"},{"nickname",["NickName4","NickName5"]}]
	]).

-define(ENCODE_RESULT1, 
	[$\{,[$\", <<"name">>,$\"],$:,[$\",<<"Name1">>,$\"],$,,[$\",<<"friend">>,$\"],$:,[$[,
[${,[$\",<<"name">>,$\"],$:,[$\",<<"FriendName1">>,$\"],$,,[$\",<<"nickname">>,$\"],$:,[$\",<<"NickName1">>,$\"],$}],$,,
[${,[$\",<<"name">>,$\"],$:,[$\",<<"FriendName2">>,$\"],$,,[$\",<<"nickname">>,$\"],$:,[$[,[$\",<<"NickName2">>,$\"],$,,[$\",<<"NickName3">>,$\"],$]],$}],$,,
[${,[$\",<<"name">>,$\"],$:,[$\",<<"FriendName3">>,$\"],$,,[$\",<<"nickname">>,$\"],$:,[$[,[$\",<<"NickName4">>,$\"],$,,[$\",<<"NickName5">>,$\"],$]],$}]
,$]],$}]).

-define(DECODE_RESULT1,
	[{"name","Name1"},
	 {"friend",
	  [
	   [{"name","FriendName1"},{"nickname","NickName1"}],
	   [{"name","FriendName2"},{"nickname",["NickName2","NickName3"]}],
	   [{"name","FriendName3"},{"nickname",["NickName4","NickName5"]}]
	  ]
	 }
	]).

%% @doc
%% Function: get_field_test/0
%% Purpose: Test the json_lib:get_field/2 by attempting to get various fields 
%%          from string json objects
%% Returns: ok | {error, term()}
%%
%% @end
get_field_test() ->
    ?assertEqual("Name1", lib_json:get_field(?JSON1, "name")),
    ?assertEqual(?GET_FIELD_RESULT1, lib_json:get_field(?JSON1, "friend")),
    ?assertEqual([{"name", "FriendName1"}, {"nickname", "NickName1"}], lib_json:get_field(?JSON1, "friend[0]")),
    ?assertEqual("FriendName1", lib_json:get_field(?JSON1, "friend[0].name")),
    ?assertEqual("NickName1", lib_json:get_field(?JSON1, "friend[0].nickname")),
    ?assertEqual(["NickName2", "NickName3"], lib_json:get_field(?JSON1, "friend[1].nickname")),
    ?assertEqual("NickName2", lib_json:get_field(?JSON1, "friend[1].nickname[0]")),
    ?assertEqual(undefined, lib_json:get_field(?JSON1, "friend[0].nick")),
    ?assertEqual([{"name","FriendName2"}, {"nickname",["NickName2","NickName3"]}] 
		 lib_json:get_field(?JSON2, "friend")).

%% @doc
%% Function: get_field_value_test/0
%% Purpose: Test the json_lib:get_field_value/2 by attempting to get various fields 
%%          from string json objects
%% Returns: ok | {error, term()}
%%
%% @end
get_field_value_test() ->
    ?assertEqual("FriendName2", lib_json:get_field_value(?JSON1, "friend[1].name", "FriendName2")),
    ?assertEqual(false, lib_json:get_field_value(?JSON1, "friend[0].name", "FriendName2")),
    ?assertEqual("NickName1", lib_json:get_field_value(?JSON1, "friend[*].nickname", "NickName1")),
    ?assertEqual("NickName3", lib_json:get_field_value(?JSON1, "friend[*].nickname", "NickName3")),
    ?assertEqual(false, lib_json:get_field_value(?JSON1, "friend[*].name", "NickName3")),
    ?assertEqual(false, lib_json:get_field_value(?JSON2, "friend[*].name", "NickName3")),
    ?assertEqual("NickName3", lib_json:get_field_value(?JSON2, "friend.nickname", "NickName3")),
    ?assertEqual([{"name", "FriendName1"}, {"nickname", "NickName1"}], 
		 lib_json:get_field_value(?JSON1, "friend[0]", Result8)),
    ?assertEqual(?GET_FIELD_RESULT1, lib_json:get_field_value(?JSON1, "friend", ?GET_FIELD_RESULT1)),

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

%% @doc
%% Function: field_value_exists_test/0
%% Purpose: Test the json_lib:field_value_exists/3 by attempting to get various fields 
%%          from string json objects
%% Returns: ok | {error, term()}
%%
%% @end
field_value_exists_test() ->
    ?assertEqual(true, lib_json:field_value_exist(?JSON1, "friend[1].name", "FriendName2")),
    ?assertEqual(false, lib_json:field_value_exist(?JSON1, "friend[0].name", "FriendName2")),
    ?assertEqual(true, lib_json:field_value_exist(?JSON1, "friend[*].nickname", "NickName1")),
    ?assertEqual(true, lib_json:field_value_exist(?JSON1, "friend[*].nickname", "NickName3")),
    ?assertEqual(false, lib_json:field_value_exist(?JSON1, "friend[*].name", "NickName3")),
    ?assertEqual(false, lib_json:field_value_exist(?JSON2, "friend[*].name", "NickName3")),
    ?assertEqual(true, lib_json:field_value_exist(?JSON2, "friend.nickname", "NickName3")),
    ?assertEqual(true, lib_json:field_value_exist(
			 ?JSON1, "friend[0]", 
			 [{"name", "FriendName1"}, {"nickname", "NickName1"}]
			)),
    ?assertEqual(true, lib_json:field_value_exist(?JSON1, "friend", ?GET_FIELD_RESULT1)).

%% @doc
%% Function: encode_test/0
%% Purpose: Test the json_lib:encode/1 by attempting to encode various json objects
%% Returns: ok | {error, term()}
%%
%% @end
encode_test() ->
    Json2 = 
	{{name, "Name1"},
	 {friend, [{{name, "FriendName1"}, {nickname, "NickName1"}},
		   {{name, "FriendName2"}, {nickname, ["NickName2", "NickName3"]}}
		  ]
	 }
	},
    
    Json3 = {{name, "Name1"},
	     {friend, {{name, "FriendName2"}, {nickname, ["NickName2", "NickName3"]}}}
	    },

    ?assertEqual(?ENCODE_RESULT1, lib_json:encode(?JSON1))%% ,


    %% Result2 = 
    %% 	"{"
    %% 	"\"name\":\"Name1\","
    %% 	  "\"friend\": ["
    %% 	  "{\"name\":\"FriendName1\", \"nickname\":\"NickName1\"},"
    %% 	  "{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
    %% 	  "]"
    %% 	  "}",
    %% 	  ?assertEqual(Result2, lib_json:encode({non_mochi, Json2})),
	  
    %% Result3 = 
    %% 	"{"
    %% 	  "\"name\":\"Name1\","
    %% 	  "\"friends\": ["
    %% 	  "{\"name\":\"FriendName1\", \"nickname\":\"NickName1\"},"
    %% 	  "{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
    %% 	  "]"
    %% 	  "}",
    %% 	  ?assertEqual(Result3, lib_json:encode({non_mochi, Json3}))
	  .


%% @doc
%% Function: decode_test/0
%% Purpose: Test the json_lib:decode/1 by attempting to decode various json objects
%% Returns: ok | {error, term()}
%%
%% @end
decode_test() ->
    ?assertEqual(?DECODE_RESULT1, lib_json:decode({pretty, ?JSON1})).

%% @doc
%% Function: encode_decode_test/0
%% Purpose: Test the json_lib:encode/1 and lib_json:decode/1 by attempting to 
%%          decode and encode consecutive calls of the functions
%% Returns: ok | {error, term()}
%%
%% @end
encode_decode_test() ->
    ?assertEqual(?ENCODE_RESULT1, lib_json:encode(lib_json:decode(lib_json:encode(?JSON1)))),
    ?assertEqual(?DECODE_RESULT1, lib_json:decode({pretty, lib_json:encode(?JSON1)})).

%% @doc
%% Function: json_macros_test/0
%% Purpose: Test the to see if macros in json.hrl produces the same result as 
%%          the manual built-up jsob objects
%% Returns: ok | {error, term()}
%%
%% @end
json_macro_test() ->
    Json1 = 
	"{"
	"\"name\":\"Name1\","
	"\"friend\": ["
	++ ?JSON_STRUCT([{name, {s, "FriendName1"}}, {nickname, {s, "NickName1"}}]) ++ ","	
	++ ?JSON_STRUCT([{name, {s, "FriendName2"}}, {nickname, {l, ["NickName2", "NickName3"]}}]) ++ ","
	++ ?JSON_STRUCT([{name, {s, "FriendName3"}}, {nickname, {l, ["NickName4", "NickName5"]}}]) ++	
	"]"
	"}",

    Json2 = 
	"{"
	"\"name\":\"Name1\","
	"\"friend\": " ++ ?JSON_STRUCT([{name, {s, "FriendName2"}}, {nickname, {l, ["NickName2", "NickName3"]}}]) ++	
	"}",

    ?assertEqual(?JSON1, Json1),
    ?assertEqual(?JSON2, Json2).
