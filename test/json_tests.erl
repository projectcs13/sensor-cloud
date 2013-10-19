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
	"\"friend\": {"
	"\"name\":\"FriendName2\", "
	"\"nickname\":[\"NickName2\", \"NickName3\"]}"
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


get_field_value_test() ->
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
	"\"friends\": ["
	    "{\"name\":\"FriendName2\", \"nickname\":[\"NickName2\", \"NickName3\"]}"
	             "]"
	"}",
    
%    Result1 = "FriendName2",
%    ?assertEqual(Result1, lib_json:get_field_value(Json1, "friends[1].name", "FriendName2")),

%    Result2 = not_found,
%    ?assertEqual(Result2, lib_json:get_field_value(Json1, "friends[0].name", "FriendName2"))

    Result3 = "NickName1",
    ?assertEqual(Result3, lib_json:get_field_value(Json1, "friends[*].nickname", "NickName1"))

    %% Result4 = "NickName3",
    %% ?assertEqual(Result4, lib_json:get_field_value(Json1, "friends[*].nickname", "NickName3")),
    
    %% Result5 = not_found,
    %% ?assertEqual(Result5, lib_json:get_field_value(Json1, "friends[*].name", "NickName3")),

    %% Result6 = "NickName3",
    %% ?assertEqual(Result6, lib_json:get_field_value(Json2, "friends[*].name", "NickName3"))
	.
