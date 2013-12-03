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
-include("debug.hrl").
-export([]).

-define(WEBMACHINE_URL, api_help:get_webmachine_url()).
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
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \"search1\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [],"application/json", "{\"name\" : \"search\",\"user_id\" : \"search1\", \"private\" : \"false\"}"}, [], []),
    {ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [],"application/json", "{\"name\" : \"search\",\"user_id\" : \"search1\", \"private\" : \"true\"}"}, [], []),
    DocId1 = lib_json:get_field(Body1,"_id"),
    DocId2 = lib_json:get_field(Body2,"_id"),
    api_help:refresh(),
    {ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {?WEBMACHINE_URL++"/streams/_search", [],"application/json", "{\"query\":{\"match_all\":{}}}"}, [], []),
    {ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, _Body8}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
    {ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, _Body9}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	{ok, {{_Version10, 200, _ReasonPhrase10}, _Headers10, _Body10}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/search1", []}, [], []),
	% Test for the search
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
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \"search2\"}"}, [], []),
	api_help:refresh(),
	% Test create
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [],"application/json", "{\"name\" : \"get\", \"user_id\" : \"search2\", \"private\" : \"false\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [],"application/json", "{\"name\" : \"get\", \"user_id\" : \"search2\", \"private\" : \"false\"}"}, [], []),
	DocId1 = lib_json:get_field(Body1,"_id"),
	DocId2 = lib_json:get_field(Body2,"_id"),
	api_help:refresh(),
	% Test get and search
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {?WEBMACHINE_URL++"/users/search2/streams", []}, [], []),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(post, {?WEBMACHINE_URL++"/streams/_search",[],"application/json", "{\"query\":{\"term\" : { \"name\" : \"get\" }}}"}, [], []),
	% Test get for missing index
	{ok, {{_Version7, 404, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(get, {?WEBMACHINE_URL++"/streams/1", []}, [], []),
	% Test delete
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	{ok, {{_Version10, 200, _ReasonPhrase10}, _Headers10, Body10}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/search2", []}, [], []),
	{{Year,Month,Day},_} = calendar:local_time(),
	Date = generate_date([Year,Month,Day]),
	% Tests to make sure the correct creation date is added
	?assertEqual(true,lib_json:get_field(Body3,"creation_date") == list_to_binary(Date)),
	?assertEqual(<<"get">>,lib_json:get_field(Body3,"name")),
	?assertEqual(true,lib_json:get_field(Body3,"private") == <<"false">>),
	?assertEqual(true,lib_json:field_value_exists(Body4,"streams[*].name", <<"get">>)),
	?assertEqual(true,lib_json:get_field(Body6,"hits.total") >= 2), % Needed in case unempty elasticsearch
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
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \"search3\"}"}, [], []),
	api_help:refresh(),
	% Test create
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\n\"name\" : \"get\",\n\"private\" : \"true\"\n, \"user_id\" : \"search3\"}"}, [], []),

	{ok, {{_VersionUser, 200, _ReasonPhraseUser}, _HeadersUser, _BodyUser}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [], "application/json", "{\"username\":\"search\"}"}, [], []),
	api_help:refresh(),

	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {?WEBMACHINE_URL++"/users/search/streams", [], "application/json", "{\"name\":\"get\",\"private\" : \"true\"}"}, [], []),
	DocId1 = lib_json:get_field(Body1,"_id"),
	DocId2 = lib_json:get_field(Body2,"_id"),
	api_help:refresh(),
	% Test update
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, _Body3}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1), [], "application/json", "{\n\"name\" : \"put\",\n\"private\" : \"false\"\n}"}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, _Body4}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId2), [], "application/json", "{\n\"name\" : \"put\"\n}"}, [], []),
	% Test get
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(get, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(get, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	% Test delete
	{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, _Body9}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/search3", []}, [], []),
	% Test update on missing doc
	{ok, {{_Version10, 404, _ReasonPhrase10}, _Headers10, Body10}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/1", [], "application/json", "{\"name\" : \"put\"\n}"}, [], []),
	?assertEqual(<<"false">>,lib_json:get_field(Body5,"private")),
	?assertEqual(true,lib_json:get_field(Body5,"private") =/= <<"true">>),
	?assertEqual(true,lib_json:get_field(Body6,"private") =/= <<"false">>),
	?assertEqual(true,lib_json:get_field(Body6,"private") == <<"true">>),
	?assertEqual(true,lib_json:get_field(Body5,"name") == <<"put">>),
	?assertEqual(true,lib_json:get_field(Body5,"name") =/= <<"get">>),
	?assertEqual(true,lib_json:get_field(Body6,"name") == <<"put">>),
	?assertEqual(true,lib_json:get_field(Body6,"name") =/= <<"get">>),
	?assertEqual(true,lib_json:get_field(Body7,"_id") == DocId1),
	?assertEqual(true,lib_json:get_field(Body8,"_id") == DocId2),
	?assertEqual(true,string:str(Body10,"not found") =/= 0).


%% @doc
%% Function: delete_stream_test/0
%% Purpose: Test the delete_stream function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 2 document in elasticsearch and deletes them
%% @end
-spec delete_stream_test() -> ok | {error, term()}.

delete_stream_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \"search4\"}"}, [], []),
	api_help:refresh(),
	% Test create
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\n\"name\" : \"get\"\n, \"user_id\" : \"search4\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {?WEBMACHINE_URL++"/users/search4/streams", [], "application/json", "{\"name\":\"get\"\n}"}, [], []),
	DocId1 = lib_json:get_field(Body1,"_id"),
	DocId2 = lib_json:get_field(Body2,"_id"),
	api_help:refresh(),
	% Test delete
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, _Body5}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/search4", []}, [], []),
	api_help:refresh(),
	% Test delete on missing index
	{ok, {{_Version6, 404, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version7, 404, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	?assertEqual(true,lib_json:get_field(Body3,"_id") == DocId1),
	?assertEqual(true,lib_json:get_field(Body4,"_id") == DocId2).


%% @doc
%% Function: create_doc_without_user_test/0
%% Purpose: Test that error is given when creating stream without user
%% Returns: ok | {error, term()}
%%
%% @end
-spec create_doc_without_user_test() -> ok | {error, term()}.

create_doc_without_user_test() ->
	{ok, {{_Version1, 500, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\n\"name\" : \"get\"\n}"}, [], []),
	?assertEqual(true,string:str(Body1,"user_id missing") =/= 0).


%% @doc
%% Function: restricted_fields_create_test/0
%% Purpose: Test that creation with restricted fields are not allowed
%% Returns: ok | {error, term()}
%% @end
-spec restricted_fields_create_test() -> ok | {error, term()}.
restricted_fields_create_test() ->
	{ok, {{_Version1, 409, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\n\"quality\" : \"\"\n, \"user_id\" : \"asdascvsr213sda\"}"}, [], []),
	{ok, {{_Version2, 409, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\n\"user_ranking\" : \"\"\n, \"user_id\" : \"asdascvsr213sda\"}"}, [], []),
	{ok, {{_Version3, 409, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\n\"subscribers\" : \"\"\n, \"user_id\" : \"asdascvsr213sda\"}"}, [], []),
	{ok, {{_Version4, 409, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\n\"last_update\" : \"\"\n, \"user_id\" : \"asdascvsr213sda\"}"}, [], []),
	{ok, {{_Version5, 409, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\n\"creation_date\" : \"\"\n, \"user_id\" : \"asdascvsr213sda\"}"}, [], []),
	{ok, {{_Version6, 409, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\n\"history_size\" : \"\"\n, \"user_id\" : \"asdascvsr213sda\"}"}, [], []),
	?assertEqual(true,string:str(Body1,"error") =/= 0),
	?assertEqual(true,string:str(Body2,"error") =/= 0),
	?assertEqual(true,string:str(Body3,"error") =/= 0),
	?assertEqual(true,string:str(Body4,"error") =/= 0),
	?assertEqual(true,string:str(Body5,"error") =/= 0),
	?assertEqual(true,string:str(Body6,"error") =/= 0).
		
		
		
%% @doc
%% Function: restricted_fields_update_test/0
%% Purpose: Test that update with restricted fields are not allowed
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 1 document in elasticsearch and deletes it
%% @end
-spec restricted_fields_update_test() -> ok | {error, term()}.
restricted_fields_update_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \"search5\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\n\"name\" : \"restricted\"\n, \"user_id\" : \"search5\"}"}, [], []),
	DocId = lib_json:get_field(Body1,"_id"),
	api_help:refresh(),
	{ok, {{_Version2, 409, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"user_ranking\" : \"\"}"}, [], []),
	{ok, {{_Version3, 409, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"subscribers\" : \"\"}"}, [], []),
	{ok, {{_Version4, 409, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"last_update\" : \"\"}"}, [], []),
	{ok, {{_Version5, 409, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"creation_date\" : \"\"}"}, [], []),
	{ok, {{_Version6, 409, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"history_size\" : \"\"\}"}, [], []),
	{ok, {{_Version7, 409, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"quality\" : \"\"\}"}, [], []),
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, _Body8}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId), []}, [], []),
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, _Body9}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/search5", []}, [], []),
	?assertEqual(true,string:str(Body2,"error") =/= 0),
	?assertEqual(true,string:str(Body3,"error") =/= 0),
	?assertEqual(true,string:str(Body4,"error") =/= 0),
	?assertEqual(true,string:str(Body5,"error") =/= 0),
	?assertEqual(true,string:str(Body6,"error") =/= 0),
	?assertEqual(true,string:str(Body7,"error") =/= 0).		
	
%% @doc
%% Function: server_side_creation_test/0
%% Purpose: Test that the correct fields are added server side
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 1 document in elasticsearch and deletes it
%% @end
-spec server_side_creation_test() -> ok | {error, term()}.

server_side_creation_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \"search7\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\"user_id\" : \"search7\"}"}, [], []),
	DocId = lib_json:get_field(Body1,"_id"),
	api_help:refresh(),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(get, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId), []}, [], []),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, _Body3}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId), []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, _Body4}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/search7", []}, [], []),
	?assertEqual(true,lib_json:get_field(Body2,"quality") =/= undefined),
	?assertEqual(true,lib_json:get_field(Body2,"user_ranking") =/= undefined),
	?assertEqual(true,lib_json:get_field(Body2,"subscribers") =/= undefined),
	?assertEqual(true,lib_json:get_field(Body2,"last_updated") =/= undefined),
	?assertEqual(true,lib_json:get_field(Body2,"creation_date") =/= undefined),
	?assertEqual(true,lib_json:get_field(Body2,"history_size") =/= undefined).




%% @doc
%% Function: ranking_stream_test/0
%% Purpose: Test that the stream can be ranked by a user
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 1 document in elasticsearch and deletes it
%% @end
-spec ranking_stream_test() -> ok | {error, term()}.

ranking_stream_test() ->

		{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [], "application/json", "{\"username\" : \"RandomUser\"}"}, [], []),

                {ok, {{_VersionUser, 200, _ReasonPhraseUser}, _HeadersUser, _BodyUser}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [], "application/json", "{\"username\":\"RandomUser\"}"}, [], []),
 	        api_help:refresh(),

		{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\"name\" : \"test0001\",\"user_id\" : \"RandomUser\"}"}, [], []),
		DocId1 = lib_json:get_field(Body1,"_id"),
		api_help:refresh(),

		{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1)++ "/_rank",[], "application/json", "{\"user_id\":\"RandomUser\",\"ranking\":5.0}"}, [], []),
		{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1), []}, [], []),

		?assertEqual(5.0,lib_json:get_field(Body4,"user_ranking.average")),
		?assertEqual(1,lib_json:get_field(Body4,"user_ranking.nr_rankings")),

		{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [], "application/json", "{\"username\" : \"RandomUser2\"}"}, [], []),
		api_help:refresh(),
		{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1)++ "/_rank",[], "application/json", "{\"user_id\":\"RandomUser2\",\"ranking\":3.0}"}, [], []),
		{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(get, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
		api_help:refresh(),
		?assertEqual(4.0,lib_json:get_field(Body7,"user_ranking.average")),
		?assertEqual(2,lib_json:get_field(Body7,"user_ranking.nr_rankings")),

		{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1)++ "/_rank",[], "application/json", "{\"user_id\":\"RandomUser\",\"ranking\":2.0}"}, [], []),
		{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(get, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1), []}, [], []),

		?assertEqual(2.5,lib_json:get_field(Body9,"user_ranking.average")),
		?assertEqual(2,lib_json:get_field(Body9,"user_ranking.nr_rankings")),

	    {ok, {{_Version10, 409, _ReasonPhrase10}, _Headers10, Body10}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1)++ "/_rank",[], "application/json", "{\"user_id\":\"RandomUser\",\"ranking\":112.0}"}, [], []),
	    {ok, {{_Version11, 409, _ReasonPhrase11}, _Headers11, Body11}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1)++ "/_rank",[], "application/json", "{\"user_id\":\"RandomUser\",\"ranking\":-12.0}"}, [], []),
	   	{ok, {{_Version12, 404, _ReasonPhrase12}, _Headers12, Body12}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1)++ "/_rank",[], "application/json", "{\"user_id\":\"aRandomUser\",\"ranking\":12.0}"}, [], []),

		{ok, {{_Version13, 200, _ReasonPhrase13}, _Headers13, _Body13}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
		{ok, {{_Version14, 200, _ReasonPhrase14}, _Headers14, Body14}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/RandomUser", []}, [], []),
		{ok, {{_Version15, 200, _ReasonPhrase15}, _Headers15, Body15}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/RandomUser2", []}, [], []).	







%% @doc
%% Function: add_unsupported_field_test/0
%% Purpose: Test that unsuported fields are not allowed to be added 
%%          on create or update
%% Returns: ok | {error, term()}
%% @end
-spec add_unsupported_field_test() -> ok | {error, term()}.
add_unsupported_field_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [], "application/json", "{\"username\":\"test\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\"user_id\" : \"test\"}"}, [], []),
	DocId = lib_json:get_field(Body1,"_id"),
	api_help:refresh(),
	{ok, {{_Version2, 403, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [],"application/json", "{\"test\":\"asdas\",\"user_id\" : \"test\"}"}, [], []),
	{ok, {{_Version3, 403, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(put, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId), [],"application/json", "{\"test\":\"asdas\",\"name\" : \"test\"}"}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, _Body4}} = httpc:request(delete, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(DocId), []}, [], []),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, _Body5}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/test", []}, [], []).



%% @doc
%% Function: dont_get_private_streams_test/0
%% Purpose: Test private streams are not listed unless user_id is given
%% Returns: ok | {error, term()}
%% @end
-spec dont_get_private_streams_test() -> ok | {error, term()}.
dont_get_private_streams_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [], "application/json", "{\"username\":\"dgtest\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\"name\":\"Private\",\"user_id\" : \"dgtest\",\"private\":\"true\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\"name\":\"Public\",\"user_id\" : \"dgtest\",\"private\":\"false\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {?WEBMACHINE_URL++"/streams", []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {?WEBMACHINE_URL++"/users/dgtest/streams", []}, [], []),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, _Body5}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/dgtest", []}, [], []),
	
	?assertEqual(true,lib_json:field_value_exists(Body3,"streams[*].private", <<"false">>)),
	?assertEqual(true,lib_json:field_value_exists(Body4,"streams[*].private", <<"true">>)).


%% @doc
%% Function: dont_get_private_streams_test/0
%% Purpose: Test private streams are not listed unless user_id is given
%% Returns: ok | {error, term()}
%% @end
-spec delete_streams_for_a_user_test() -> ok | {error, term()}.
delete_streams_for_a_user_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [], "application/json", "{\"username\":\"dstest\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, _Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\"name\":\"Private\",\"user_id\" : \"dstest\",\"private\":\"true\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, _Body2}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [], "application/json", "{\"name\":\"Public\",\"user_id\" : \"dstest\",\"private\":\"false\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {?WEBMACHINE_URL++"/users/dstest/streams", []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, _Body4}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/dstest/streams", []}, [], []),
	api_help:refresh(),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(get, {?WEBMACHINE_URL++"/users/dstest/streams", []}, [], []),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, _Body6}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/dstest", []}, [], []),
	
	
	?assertEqual(true,lib_json:field_value_exists(Body3,"streams[*].name", <<"Private">>)),
	?assertEqual(true,lib_json:field_value_exists(Body3,"streams[*].name", <<"Public">>)),
	?assertEqual("{\"streams\":[]}",Body5).
	
%% @doc
%% Function: generate_date/2
%% Purpose: Used to create a date valid in ES
%%          from the input which should be the list
%%          [Year,Mounth,Day]
%% Returns: The generated timestamp
%%
%% @end
-spec generate_date(DateList::list()) -> string().

generate_date([First]) ->
	case First < 10 of
		true -> "0" ++ integer_to_list(First);
		false -> "" ++ integer_to_list(First)
	end;
generate_date([First|Rest]) ->
	case First < 10 of
		true -> "0" ++ integer_to_list(First) ++ "-" ++ generate_date(Rest);
		false -> "" ++ integer_to_list(First) ++ "-" ++ generate_date(Rest)
	end.
