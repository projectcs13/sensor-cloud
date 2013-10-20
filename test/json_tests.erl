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

%% @doc
%% Function: get_field_test/0
%% Purpose: Test the json_lib:get_field/2 by attempting to get various fields 
%%          from string json objects
%% Returns: ok | {error, term()}
%%
%% @end
get_field_test() ->
    Json1 = 
	"{"
	"\"name\":\"Name1\","
	"\"friends\": ["
	"{\"name\":\"FriendName1\", \"nickname\":\"NickName1\"},"
	"{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
	"]"
	"}",
    Json2 = 
	"{"
	"\"name\":\"Name1\","
	"\"friend\": {\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
	"}",

    Result1 = "Name1",
    ?assertEqual(Result1, lib_json:get_field(Json1, "name")),

    Result2 = [
	       [{"name","FriendName1"},{"nickname","NickName1"}],
	       [{"name","FriendName2"},{"nickname",["NickName2","NickName3"]}]
	      ],
    ?assertEqual(Result2, lib_json:get_field(Json1, "friends")),

    Result3 = [{"name", "FriendName1"}, {"nickname", "NickName1"}],
    ?assertEqual(Result3, lib_json:get_field(Json1, "friends[0]")),

    Result4 = "FriendName1",
    ?assertEqual(Result4, lib_json:get_field(Json1, "friends[0].name")),

    Result5 = "NickName1",
    ?assertEqual(Result5, lib_json:get_field(Json1, "friends[0].nickname")),

    Result6 = ["NickName2", "NickName3"],
    ?assertEqual(Result6, lib_json:get_field(Json1, "friends[1].nickname")),

    Result7 = "NickName2",
    ?assertEqual(Result7, lib_json:get_field(Json1, "friends[1].nickname[0]")),

    Result8 = undefined,
    ?assertEqual(Result8, lib_json:get_field(Json1, "friends[0].nick")),

    Result9 = [{"name","FriendName2"}, {"nickname",["NickName2","NickName3"]}],
    ?assertEqual(Result9, lib_json:get_field(Json2, "friend")).

%% @doc
%% Function: get_field_value_test/0
%% Purpose: Test the json_lib:get_field_value/2 by attempting to get various fields 
%%          from string json objects
%% Returns: ok | {error, term()}
%%
%% @end
get_field_value_test() ->
    Json1 = 
	"{"
	"\"name\":\"Name1\","
	"\"friends\": ["
	"{\"name\":\"FriendName1\", \"nickname\":\"NickName1\"},"
	"{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]},"
	"{\"name\":\"FriendName3\", \"nickname\":[\"NickName4\", \"NickName5\"]}"
	"]"
	"}",
    Json2 = 
	"{"
	"\"name\":\"Name1\","
	"\"friends\": ["
	"{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
	"]"
	"}",
    Result1 = "FriendName2",
    ?assertEqual(Result1, lib_json:get_field_value(Json1, "friends[1].name", "FriendName2")),

    Result2 = false,
    ?assertEqual(Result2, lib_json:get_field_value(Json1, "friends[0].name", "FriendName2")),

    Result3 = "NickName1",
    ?assertEqual(Result3, lib_json:get_field_value(Json1, "friends[*].nickname", "NickName1")),

    Result4 = "NickName3",
    ?assertEqual(Result4, lib_json:get_field_value(Json1, "friends[*].nickname", "NickName3")),

    Result5 = false,
    ?assertEqual(Result5, lib_json:get_field_value(Json1, "friends[*].name", "NickName3")),

    Result6 = false,
    ?assertEqual(Result6, lib_json:get_field_value(Json2, "friends[*].name", "NickName3")),

    Result7 = "NickName3",
    ?assertEqual(Result7, lib_json:get_field_value(Json2, "friends[*].nickname", "NickName3")),

    Result8 = [{"name", "FriendName1"}, {"nickname", "NickName1"}],
    ?assertEqual(Result8, lib_json:get_field_value(Json1, "friends[0]", Result8)),

    Result9 = [
	       [{"name","FriendName1"},{"nickname","NickName1"}],
	       [{"name","FriendName2"},{"nickname",["NickName2","NickName3"]}],
	       [{"name","FriendName3"},{"nickname",["NickName4","NickName5"]}]	       
	      ],
    ?assertEqual(Result9, lib_json:get_field_value(Json1, "friends", Result9)),


    %% This call will produce an error. Added here as an example of how 
    %% lib_json:get_field_value/3 NOT should be used. The second argument is not
    %% allowed to end with [*]
    %% See lib_json:get_field_value/3 for details of how to use the function
    Try = try lib_json:get_field_value(Json1, "friends[*]", "") of
	      _ -> will_not_happen
	  catch
	      error:_Error -> error
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
    Json1 = 
	"{"
	"\"name\":\"Name1\","
	"\"friends\": ["
	"{\"name\":\"FriendName1\", \"nickname\":\"NickName1\"},"
	"{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]},"
	"{\"name\":\"FriendName3\", \"nickname\":[\"NickName4\", \"NickName5\"]}"
	"]"
	"}",

    Json2 = 
	"{"
	"\"name\":\"Name1\","
	"\"friends\": ["
	"{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
	"]"
	"}",
    ?assertEqual(true, lib_json:field_value_exist(Json1, "friends[1].name", "FriendName2")),

    ?assertEqual(false, lib_json:field_value_exist(Json1, "friends[0].name", "FriendName2")),

    ?assertEqual(true, lib_json:field_value_exist(Json1, "friends[*].nickname", "NickName1")),

    ?assertEqual(true, lib_json:field_value_exist(Json1, "friends[*].nickname", "NickName3")),

    ?assertEqual(false, lib_json:field_value_exist(Json1, "friends[*].name", "NickName3")),

    ?assertEqual(false, lib_json:field_value_exist(Json2, "friends[*].name", "NickName3")),

    ?assertEqual(true, lib_json:field_value_exist(Json2, "friends[*].nickname", "NickName3")),

    ?assertEqual(true, lib_json:field_value_exist(Json1, "friends[0]", [{"name", "FriendName1"}, {"nickname", "NickName1"}])),

    Result9 = [
	       [{"name","FriendName1"},{"nickname","NickName1"}],
	       [{"name","FriendName2"},{"nickname",["NickName2","NickName3"]}],
	       [{"name","FriendName3"},{"nickname",["NickName4","NickName5"]}]	       
	      ],
    ?assertEqual(true, lib_json:field_value_exist(Json1, "friends", Result9)).

%% @doc
%% Function: encode_test/0
%% Purpose: Test the json_lib:encode/1 by attempting to encode various json objects
%% Returns: ok | {error, term()}
%%
%% @end
encode_test() ->
    Json1 = 
	"{"
	"\"name\":\"Name1\","
	"\"friends\": ["
	"{\"name\":\"FriendName1\", \"nickname\":\"NickName1\"},"
	"{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
	"]"
	"}",

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

    Result1 = 
	[$\{,[$\", <<"name">>,$\"],$:,[$\",<<"Name1">>,$\"],$,,[$\",<<"friends">>,$\"],$:,[$[,[${,[$\",<<"name">>,$\"],$:,[$\",<<"FriendName1">>,$\"],$,,[$\",<<"nickname">>,$\"],$:,[$\",<<"NickName1">>,$\"],$}],$,,[${,[$\",<<"name">>,$\"],$:,[$\",<<"FriendName2">>,$\"],$,,[$\",<<"nickname">>,$\"],$:,[$[,[$\",<<"NickName2">>,$\"],$,,[$\",<<"NickName3">>,$\"],$]],$}],$]],$}],

    ?assertEqual(Result1, lib_json:encode(Json1)),


    Result2 = 
	"{"
	"\"name\":\"Name1\","
	  "\"friend\": ["
	  "{\"name\":\"FriendName1\", \"nickname\":\"NickName1\"},"
	  "{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
	  "]"
	  "}",
	  ?assertEqual(Result2, lib_json:encode({non_mochi, Json2})),
	  
    Result3 = 
	"{"
	  "\"name\":\"Name1\","
	  "\"friends\": ["
	  "{\"name\":\"FriendName1\", \"nickname\":\"NickName1\"},"
	  "{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
	  "]"
	  "}",
	  ?assertEqual(Result3, lib_json:encode({non_mochi, Json3})).


%% @doc
%% Function: decode_test/0
%% Purpose: Test the json_lib:decode/1 by attempting to decode various json objects
%% Returns: ok | {error, term()}
%%
%% @end
decode_test() ->
    Json1 = 
	"{"
	"\"name\":\"Name1\","
	"\"friends\": ["
	"{\"name\":\"FriendName1\", \"nickname\":\"NickName1\"},"
	"{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
	"]"
	"}",
    Result1 = [{"name","Name1"},
	       {"friends",[
			   [{"name","FriendName1"},{"nickname","NickName1"}],
			   [{"name","FriendName2"},{"nickname",["NickName2","NickName3"]}]
			  ]
	       }
	      ],

    ?assertEqual(Result1, lib_json:decode({pretty, Json1})).

%% @doc
%% Function: encode_decode_test/0
%% Purpose: Test the json_lib:encode/1 and lib_json:decode/1 by attempting to 
%%          decode and encode consecutive calls of the functions
%% Returns: ok | {error, term()}
%%
%% @end
encode_decode_test() ->
    Json1 = 
	"{"
	"\"name\":\"Name1\","
	"\"friends\": ["
	"{\"name\":\"FriendName1\", \"nickname\":\"NickName1\"},"
	"{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
	"]"
	"}",

    Result1 = [${,[$\", <<"name">>,$\"],$:,[$\",<<"Name1">>,$\"],$,,[$\",<<"friends">>,$\"],$:,[$[,[${,[$\",<<"name">>,$\"],$:,[$\",<<"FriendName1">>,$\"],$,,[$\",<<"nickname">>,$\"],$:,[$\",<<"NickName1">>,$\"],$}],$,,[${,[$\",<<"name">>,$\"],$:,[$\",<<"FriendName2">>,$\"],$,,[$\",<<"nickname">>,$\"],$:,[$[,[$\",<<"NickName2">>,$\"],$,,[$\",<<"NickName3">>,$\"],$]],$}],$]],$}],

    Result2 = [{"name","Name1"},
	       {"friends",[
			   [{"name","FriendName1"},{"nickname","NickName1"}],
			   [{"name","FriendName2"},{"nickname",["NickName2","NickName3"]}]
			  ]
	       }
	      ],
		   ?assertEqual(Result1, lib_json:encode(lib_json:decode(lib_json:encode(Json1)))),
		   ?assertEqual(Result2, lib_json:decode({pretty, lib_json:encode(Json1)})).

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
	"{\"name\":\"FriendName1\", \"nickname\":\"NickName1\"},"
	"{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
	"]"
	"}",

    Json1b = 
	"{"
	"\"name\":\"Name1\","
	"\"friend\": ["
	++ ?JSON_STRUCT([{name, {s, "FriendName1"}}, {nickname, {s, "NickName1"}}]) ++ ","	
	++ ?JSON_STRUCT([{name, {s, "FriendName2"}}, {nickname, {l, ["NickName2", "NickName3"]}}]) ++	
	"]"
	"}",

    Json2 = 
	"{"
	"\"name\":\"Name1\","
	"\"friend\": {\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
	"}",

    Json2b = 
	"{"
	"\"name\":\"Name1\","
	"\"friend\": " ++ ?JSON_STRUCT([{name, {s, "FriendName2"}}, {nickname, {l, ["NickName2", "NickName3"]}}]) ++	
	"}",

    ?assertEqual(Json1, Json1b),
    ?assertEqual(Json2, Json2b).
