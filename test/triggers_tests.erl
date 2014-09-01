%% @author Tomas S�vstr�m <tosa7943@student.uu.se>
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == triggers_tests ==
%% This module contains several tests to test the functionallity
%% in the module triggers which is done by calling the webbmachine.
%%
%% @end

-module(triggers_tests).
-include_lib("eunit/include/eunit.hrl").
-include("debug.hrl").
-include_lib("amqp_client.hrl").
-export([post_data_user_vstream/0]).

-define(WEBMACHINE_URL, api_help:get_webmachine_url()).
-define(ES_URL, api_help:get_elastic_search_url()).
-define(INDEX, "sensorcloud").
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
%% Function: create_delete_test/0
%% Purpose: Test the process_post and delete_resource functions by doing some HTTP requests
%% Returns: ok | {error, term()}
%% @end
create_delete_test() -> 
	%% Create
	{ok, {{_VersionU1, 200, _ReasonPhraseU1}, _HeadersU1, BodyU1}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \"tomas\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"test\",\"input\":5,\"streams\":\"test\", \"vstreams\":\"\"}"}, [], []),
	DocId1 = lib_json:get_field(Body1,"_id"),
	api_help:refresh(),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId1), []}, [], []),
	api_help:refresh(),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"test\",\"input\":5,\"streams\":\"test\", \"vstreams\":\"\"}"}, [], []),
	DocId2 = lib_json:get_field(Body3,"_id"),
	api_help:refresh(),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId2), []}, [], []),
	api_help:refresh(),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"test\",\"input\":6,\"streams\":\"test\", \"vstreams\":\"\"}"}, [], []),
	DocId3 = lib_json:get_field(Body5,"_id"),
	api_help:refresh(),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId3), []}, [], []),
	api_help:refresh(),
	{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"test\",\"input\":6,\"streams\":[\"test\",\"test2\"], \"vstreams\":\"\"}"}, [], []),
	DocId4 = lib_json:get_field(Body7,"_id"),
	api_help:refresh(),
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId4), []}, [], []),
	api_help:refresh(),
	
	%% Create tests
	?assertEqual(lib_json:get_field(Body2,"_source"),lib_json:get_field(Body4,"_source")),
	?assertEqual(DocId1,DocId3),
	?assertNotEqual(DocId1,DocId4),
	?assertEqual(true,lib_json:field_value_exists(Body6,"_source.outputlist[*].input",5)),
	?assertEqual(true,lib_json:field_value_exists(Body6,"_source.outputlist[*].input",6)),
	
	
	
	%% Delete 
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"test\",\"input\":5,\"streams\":\"test\", \"vstreams\":\"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version10, 200, _ReasonPhrase10}, _Headers10, Body10}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId1), []}, [], []),
	api_help:refresh(),
	{ok, {{_Version11, 200, _ReasonPhrase11}, _Headers11, Body11}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"test\",\"input\":6,\"streams\":\"test\", \"vstreams\":\"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version12, 200, _ReasonPhrase12}, _Headers12, Body12}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId3), []}, [], []),
	api_help:refresh(),
	{ok, {{_Version13, 200, _ReasonPhrase13}, _Headers13, Body13}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"test\",\"input\":6,\"streams\":[\"test\",\"test2\"], \"vstreams\":\"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version14, 200, _ReasonPhrase14}, _Headers14, Body14}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId4), []}, [], []),
	api_help:refresh(),
	{ok, {{_VersionU2, 200, _ReasonPhraseU2}, _HeadersU2, BodyU2}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/tomas", []}, [], []),
	api_help:refresh(),
	%% Delete tests
	?assertEqual(true,(lib_json:get_field(Body12,"exist") == false) or (lib_json:get_field(Body12,"_source.outputlist") == [])), %% Answer will depend on how quick messages to the triggersProcess are
	?assertEqual(true,(lib_json:get_field(Body14,"exist") == false) or (lib_json:get_field(Body14,"_source.outputlist") == [])). %% Answer will depend on how quick messages to the triggersProcess are



%% @doc
%% Function: post_data_test/0
%% Purpose: Test the triggersProcess by doing some posting for data to
%%          the streams the trigger is on
%% Returns: ok | {error, term()}
%% @end
post_data_exchange_test() ->
    Descript = "Trigger data exchange test",
    Setup = 
	fun() ->
		User1 = "test1",
		User2 = "test2",
		http:post(?WEBMACHINE_URL++"/users", "{\"username\" : \""++User1++"\"}"),
		http:post(?WEBMACHINE_URL++"/users", "{\"username\" : \""++User2++"\"}"),
		api_help:refresh(),
		{200, Body1} = http:post(?WEBMACHINE_URL++"/streams", "{\"name\":\"Stream1\",\"user_id\":\""++User1++"\"}"),
		{200, Body2} = http:post(?WEBMACHINE_URL++"/streams", "{\"name\":\"Stream2\",\"user_id\":\""++User1++"\"}"),
		api_help:refresh(),
		StreamId1 = lib_json:to_string(lib_json:get_field(Body1,"_id")),
		StreamId2 = lib_json:to_string(lib_json:get_field(Body2,"_id")),
		{200, Body3} = http:post(?WEBMACHINE_URL++"/users/"++User1++"/triggers/add",
					 "{\"function\":\"less_than\",\"input\":5,\"streams\":\""++StreamId1++"\", \"vstreams\":\"\"}"),		
		api_help:refresh(),
		{200, Body4} = http:post(?WEBMACHINE_URL++"/users/"++User2++"/triggers/add",
					 "{\"function\":\"less_than\",\"input\":5,\"streams\":\""++StreamId1++"\", \"vstreams\":\"\"}"),
		api_help:refresh(),
		{200, Body5} = http:post(?WEBMACHINE_URL++"/users/"++User1++"/triggers/add",
					 "{\"function\":\"less_than\",\"input\":10,\"streams\":\""++StreamId1++"\", \"vstreams\":\"\"}"),
		api_help:refresh(),
		{200, Body6} = http:post(?WEBMACHINE_URL++"/users/"++User1++"/triggers/add",
					 "{\"function\":\"less_than\",\"input\":6,\"streams\":[\""
					 ++StreamId1++"\",\""++lib_json:to_string(StreamId2)++"\"], \"vstreams\":\"\"}"),
		TriggerId1 = lib_json:to_string(lib_json:get_field(Body3, "_id")),
		TriggerId2 = lib_json:to_string(lib_json:get_field(Body6, "_id")),
		api_help:refresh(),

		%% Connect.
		{ok, Connection} = amqp_connection:start(#amqp_params_network{}),
		%% Open In and OUT channels.
		{ok, ChannelIn} = amqp_connection:open_channel(Connection),
		InputExchanges = [list_to_binary("trigger."++TriggerId1),
				  list_to_binary("trigger."++TriggerId2)],		
		triggersProcess:subscribe(ChannelIn, InputExchanges),
		{User1, User2, StreamId1, StreamId2, ChannelIn, Connection}
	end,
    Test = 
	fun({User1, User2, StreamId1, StreamId2,  _ChannelIn, _Connection}) ->
		{200, Body7} = http:post(?WEBMACHINE_URL++"/streams/"++StreamId1++"/data", "{\"value\" : 4}"),
		api_help:refresh(),
		{200, Body8} = http:post(?WEBMACHINE_URL++"/streams/"++StreamId1++"/data", "{\"value\" : 7}"),
		api_help:refresh(),
		{200, Body9} = http:post(?WEBMACHINE_URL++"/streams/"++StreamId2++"/data", "{\"value\" : 4}"),
		api_help:refresh(),	
		Messages = [{4,StreamId1,5, [{user,User2},{user,User1}], "stream"},
			    {4,StreamId1,10,[{user,User1}], "stream"},
			    {4,StreamId1,6, [{user,User1}], "stream"},
			    {7,StreamId1,10,[{user,User1}], "stream"},
			    {4,StreamId2,6, [{user,User1}], "stream"}],
		receive_loop(Messages)
	end,
    Cleanup = 
	fun({User1, User2, StreamId1, StreamId2, ChannelIn, Connection}) ->
		amqp_channel:close(ChannelIn),
		amqp_connection:close(Connection),
		api_help:refresh(),
		{200, Body10} = http:post(?WEBMACHINE_URL++"/users/"++User1++"/triggers/remove", 
					  "{\"function\" : \"less_than\",\"input\":5,\"streams\":\""++StreamId1++"\", \"vstreams\":\"\"}"),
		api_help:refresh(),
		{200, Body11} = http:post(?WEBMACHINE_URL++"/users/"++User2++"/triggers/remove", 
					  "{\"function\":\"less_than\",\"input\":5,\"streams\":\""++StreamId1++"\", \"vstreams\":\"\"}"),
		api_help:refresh(),
		{200, Body12} = http:post(?WEBMACHINE_URL++"/users/"++User1++"/triggers/remove",
					  "{\"function\":\"less_than\",\"input\":10,\"streams\":\""++StreamId1++"\", \"vstreams\":\"\"}"),
		api_help:refresh(),
		{200, Body13} = http:post(?WEBMACHINE_URL++"/users/"++User1++"/triggers/remove",
					  "{\"function\":\"less_than\",\"input\":6,\"streams\":[\""
					  ++StreamId1++"\",\""++StreamId2++"\"], \"vstreams\":\"\"}"),
	
		api_help:refresh(),
		{200, Body14} = http:delete(?WEBMACHINE_URL++"/users/"++User1),
		api_help:refresh(),
		{200, Body15} = http:delete(?WEBMACHINE_URL++"/users/"++User2),
		api_help:refresh()
	end,
    {timeout, 30, [{Descript, {setup, Setup, Cleanup, Test}}]}.


%% @doc
%% Function: post_data_test/0
%% Purpose: Test the triggersProcess by doing some posting for data to
%%          the streams the trigger is on
%% Returns: ok | {error, term()}
%% @end
post_data_user_test() ->
	User1 = "tomas",
	User2 = "erik",
    httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \""++User1++"\"}"}, [], []),
    httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \""++User2++"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [],"application/json", "{\"name\" : \"Stream1\",\"user_id\":\"tomas\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [],"application/json", "{\"name\" : \"Stream2\",\"user_id\":\"tomas\"}"}, [], []),
	StreamId1 = lib_json:get_field(Body1,"_id"),
	StreamId2 = lib_json:get_field(Body2,"_id"),
	api_help:refresh(),
	%% Create
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"less_than\",\"input\":5,\"streams\":\"" ++ lib_json:to_string(StreamId1) ++"\", \"vstreams\":\"\"}"}, [], []),
	TriggerId1 = lib_json:get_field(Body3, "_id"),
	api_help:refresh(),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(post, {?WEBMACHINE_URL++"/users/erik/triggers/add", [],"application/json", "{\"function\" : \"less_than\",\"input\":5,\"streams\":\"" ++ lib_json:to_string(StreamId1) ++"\", \"vstreams\":\"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"less_than\",\"input\":10,\"streams\":\"" ++ lib_json:to_string(StreamId1) ++"\", \"vstreams\":\"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"less_than\",\"input\":6,\"streams\":[\"" ++ lib_json:to_string(StreamId1) ++"\",\"" ++ lib_json:to_string(StreamId2) ++"\"], \"vstreams\":\"\"}"}, [], []),
	TriggerId2 = lib_json:get_field(Body6, "_id"),
	api_help:refresh(),
	
	
	{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(post, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(StreamId1) ++"/data", [],"application/json", "{\"value\" : 4.0}"}, [], []),
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(post, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(StreamId1) ++"/data", [],"application/json", "{\"value\" : 7.0}"}, [], []),
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(post, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(StreamId2) ++"/data", [],"application/json", "{\"value\" : 4.0}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version10, 200, _ReasonPhrase10}, _Headers10, Body10}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"less_than\",\"input\":5,\"streams\":\"" ++ lib_json:to_string(StreamId1) ++"\", \"vstreams\":\"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version11, 200, _ReasonPhrase11}, _Headers11, Body11}} = httpc:request(post, {?WEBMACHINE_URL++"/users/erik/triggers/remove", [],"application/json", "{\"function\" : \"less_than\",\"input\":5,\"streams\":\"" ++ lib_json:to_string(StreamId1) ++"\", \"vstreams\":\"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version12, 200, _ReasonPhrase12}, _Headers12, Body12}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"less_than\",\"input\":10,\"streams\":\"" ++ lib_json:to_string(StreamId1) ++"\", \"vstreams\":\"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version13, 200, _ReasonPhrase13}, _Headers13, Body13}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"less_than\",\"input\":6,\"streams\":[\"" ++ lib_json:to_string(StreamId1) ++"\",\"" ++ lib_json:to_string(StreamId2) ++"\"], \"vstreams\":\"\"}"}, [], []),
	api_help:refresh(),
	timer:sleep(3000),
	{ok, {{_VersionU3, 200, _ReasonPhraseU3}, _HeadersU3, BodyU3}} = httpc:request(get, {?WEBMACHINE_URL++"/users/tomas", []}, [], []),
	{ok, {{_VersionU4, 200, _ReasonPhraseU4}, _HeadersU4, BodyU4}} = httpc:request(get, {?WEBMACHINE_URL++"/users/erik", []}, [], []),
	api_help:refresh(),
	{ok, {{_VersionU5, 200, _ReasonPhraseU5}, _HeadersU5, BodyU5}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/tomas", []}, [], []),
	{ok, {{_VersionU6, 200, _ReasonPhraseU6}, _HeadersU6, BodyU6}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/erik", []}, [], []),
	NotificationList1 = lists:map(fun(A) -> lib_json:rm_field(A, "trigger.timestamp") end,lib_json:get_field(BodyU3,"notifications")),
	NotificationList2 = lists:map(fun(A) -> lib_json:rm_field(A, "trigger.timestamp") end,lib_json:get_field(BodyU4,"notifications")),
	ReferenceList1 = ["{\"trigger\":{\"input\":10,\"stream_id\":\"" ++ lib_json:to_string(StreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId1) ++ "\",\"type\":\"stream\",\"value\":4.0}}",
					  "{\"trigger\":{\"input\":5,\"stream_id\":\"" ++ lib_json:to_string(StreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId1) ++ "\",\"type\":\"stream\",\"value\":4.0}}",
					  "{\"trigger\":{\"input\":10,\"stream_id\":\"" ++ lib_json:to_string(StreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId1) ++ "\",\"type\":\"stream\",\"value\":7.0}}",
					  "{\"trigger\":{\"input\":6,\"stream_id\":\"" ++ lib_json:to_string(StreamId2) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId2) ++ "\",\"type\":\"stream\",\"value\":4.0}}",
					  "{\"trigger\":{\"input\":6,\"stream_id\":\"" ++ lib_json:to_string(StreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId2) ++ "\",\"type\":\"stream\",\"value\":4.0}}",
					  "{\"trigger\":{\"input\":6,\"stream_id\":\"" ++ lib_json:to_string(StreamId2) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId2) ++ "\",\"type\":\"stream\",\"value\":4.0}}",
					  "{\"trigger\":{\"input\":6,\"stream_id\":\"" ++ lib_json:to_string(StreamId2) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId2) ++ "\",\"type\":\"stream\",\"value\":4.0}}"],
	ReferenceList2 = ["{\"trigger\":{\"input\":5,\"stream_id\":\"" ++ lib_json:to_string(StreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId1) ++ "\",\"type\":\"stream\",\"value\":4.0}}"],
	?assertEqual(true, check_all_exist(NotificationList1,ReferenceList1)),
	?assertEqual(true, check_all_exist(NotificationList2,ReferenceList2)).
%% @doc
%% Function: list_triggers_test_/0
%% Purpose: Test the listing of triggers
%% Returns: ok | {error, term()}
%% @end
list_triggers_test_() ->
    Descript1 = "Testing listing of triggers",
    Setup1 = 
	fun() ->
		UserId1 = "tommy",
		%% Create users
		httpc:request(post,{?WEBMACHINE_URL++"/users",[],"application/json","{\"username\":\""++UserId1++"\"}"},[],[]),
		%% Create streams
		{ok,{{_,200,_},_,Body1}} = 
		    httpc:request(post,{?WEBMACHINE_URL++"/streams",[],"application/json",
					"{\"name\":\"Stream1\",\"user_id\":\""++UserId1++"\"}"},[],[]),
		{ok,{{_,200,_},_,Body2}} = 
		    httpc:request(post,{?WEBMACHINE_URL++"/streams",[],"application/json",
					"{\"name\":\"Stream2\",\"user_id\":\""++UserId1++"\"}"},[],[]),
		api_help:refresh(),
		StreamId1 = lib_json:to_string(lib_json:get_field(Body1,"_id")),
		StreamId2 = lib_json:to_string(lib_json:get_field(Body2,"_id")),
		%% Create a trigger
		{ok,{{_,200,_},_,_}} = httpc:request(post,{?WEBMACHINE_URL++"/users/"++UserId1++"/triggers/add",[],
							       "application/json",
							       "{\"function\" : \"less_than\",\"input\":5,\"streams\":[\"" 
							       ++ StreamId1 ++ "\",\""++StreamId2 ++"\"], \"vstreams\":\"\"}"}, [], []),
		{ok,{{_,200,_},_,_}} = httpc:request(post,{?WEBMACHINE_URL++"/users/"++UserId1++"/triggers/add",[],
							       "application/json",
							       "{\"function\" : \"less_than\",\"input\":4,\"streams\":\"" 
							       ++ StreamId2 ++"\", \"vstreams\":\"\"}"}, [], []),
		api_help:refresh(),
		{UserId1, StreamId1, StreamId2}
	end,
    Cleanup1 =
	fun({UserId1, StreamId1, StreamId2}) ->
		{ok,{{_,200,_},_,_}} = httpc:request(post,{?WEBMACHINE_URL++"/users/"++UserId1++"/triggers/remove",[],
							   "application/json",
							   "{\"function\" : \"less_than\",\"input\":5,\"streams\":[\"" 
							   ++ StreamId1 ++ "\",\""++StreamId2 ++"\"], \"vstreams\":\"\"}"}, [], []),
		{ok,{{_,200,_},_,_}} = httpc:request(post,{?WEBMACHINE_URL++"/users/"++UserId1++"/triggers/remove",[],
							       "application/json",
							       "{\"function\" : \"less_than\",\"input\":4,\"streams\":\"" 
							       ++StreamId2 ++"\", \"vstreams\":\"\"}"}, [], []),
		{ok,{{_,200,_},_,_}} = httpc:request(delete,{?WEBMACHINE_URL++"/streams/"++StreamId1,[]},[],[]),
		{ok,{{_,200,_},_,_}} = httpc:request(delete,{?WEBMACHINE_URL++"/streams/"++StreamId2,[]},[],[]),
		{ok,{{_,200,_},_,_}} = httpc:request(delete,{?WEBMACHINE_URL++"/users/"++UserId1,[]},[],[])
	end,
    Tests1 = fun list_triggers/1,
    {Descript1,{setup, Setup1, Cleanup1, Tests1}}.


%% @doc
%% Function: list_triggers/0
%% Purpose: Test the listing of triggers
%% Returns: ok | {error, term()}
%% @end

list_triggers({UserId1, StreamId1, StreamId2}) ->
    {ok,{{_,200,_},_,Body1}} = httpc:request(get, {?WEBMACHINE_URL++"/users/"++UserId1++"/triggers", []}, [], []),
    {ok,{{_,200,_},_,Body2}} = httpc:request(get, {?WEBMACHINE_URL++"/users/"++UserId1++"/streams/"
							    ++StreamId2++"/triggers", []}, [], []),
    {ok,{{_,200,_},_,Body3}} = httpc:request(get, {?WEBMACHINE_URL++"/users/"++UserId1++"/streams/"
							    ++StreamId1++"/triggers", []}, [], []),
    Result11 = 
	"{\"triggers\":[{\"function\":\"less_than\",\"input\":5,\"output_id\":\""++UserId1++"\",\"output_type\":\"user\",\"streams\":[\""++StreamId1++"\",\""++StreamId2++"\"],\"type\":\"stream\",\"vstreams\":[]},"
	"{\"function\":\"less_than\",\"input\":4,\"output_id\":\""++UserId1++"\",\"output_type\":\"user\",\"streams\":[\""++StreamId2++"\"],\"type\":\"stream\",\"vstreams\":[]}]}",
    Result21 = "{\"triggers\":[{\"function\":\"less_than\",\"input\":5,\"output_id\":\""++UserId1++"\",\"output_type\":\"user\",\"streams\":[\""++StreamId1++"\",\""++StreamId2++"\"],\"type\":\"stream\",\"vstreams\":[]}]}",
    Result12 = 
	"{\"triggers\":[{\"function\":\"less_than\",\"input\":5,\"output_id\":\""++UserId1++"\",\"output_type\":\"user\",\"streams\":[\""++StreamId2++"\",\""++StreamId1++"\"],\"type\":\"stream\",\"vstreams\":[]},"
	"{\"function\":\"less_than\",\"input\":4,\"output_id\":\""++UserId1++"\",\"output_type\":\"user\",\"streams\":[\""++StreamId2++"\"],\"type\":\"stream\",\"vstreams\":[]}]}",
    Result22 = "{\"triggers\":[{\"function\":\"less_than\",\"input\":5,\"output_id\":\""++UserId1++"\",\"output_type\":\"user\",\"streams\":[\""++StreamId2++"\",\""++StreamId1++"\"],\"type\":\"stream\",\"vstreams\":[]}]}",
	[?_assertEqual(true,(Result11 == Body1) or (Result12 == Body1)),
     ?_assertEqual(true,(Result11 == Body2) or (Result12 == Body2)),
     ?_assertEqual(true,(Result21 == Body3) or (Result22 == Body3))].



%% @doc
%% Function: start_up_triggers_test/0
%% Purpose: Test the trigger start function
%% Returns: ok | {error, term()}
%% @end

start_up_triggers_test() ->
	User1 = "tomas",
    httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \""++User1++"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [],"application/json", "{\"name\" : \"Stream1\",\"user_id\":\"tomas\"}"}, [], []),
	StreamId1 = lib_json:get_field(Body1,"_id"),
	api_help:refresh(),
	Trigger = lib_json:set_attrs([{"function",list_to_binary("less_than")},{"streams",[StreamId1]},{"vstreams",[]},{"type","stream"},{"outputlist","[{}]"},{"outputlist[0].input",10},{"outputlist[0].output",["{}"]},{"outputlist[0].output[0].output_id",list_to_binary("tomas")},{"outputlist[0].output[0].output_type",list_to_binary("user")}]),
	erlastic_search:index_doc_with_id(?INDEX,"trigger","1",Trigger),
	api_help:refresh(),
	triggers:start_all_triggers_in_es(),
	timer:sleep(1000),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, _Body2}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"less_than\",\"input\":5,\"streams\":\"" ++ lib_json:to_string(StreamId1) ++"\", \"vstreams\":\"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, _Body3}} = httpc:request(post, {?WEBMACHINE_URL++"/streams/"++ lib_json:to_string(StreamId1) ++ "/data" , [],"application/json", "{\"value\" : 4.0}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {?WEBMACHINE_URL++"/users/tomas", []}, [], []),
	NotificationList = lists:map(fun(A) -> lib_json:rm_field(A, "trigger.timestamp") end,lib_json:get_field(Body4,"notifications")),
	ReferenceList = ["{\"trigger\":{\"input\":10,\"stream_id\":\"" ++ lib_json:to_string(StreamId1) ++"\",\"trigger_id\":\"1\",\"type\":\"stream\",\"value\":4.0}}",
					 "{\"trigger\":{\"input\":5,\"stream_id\":\"" ++ lib_json:to_string(StreamId1) ++"\",\"trigger_id\":\"1\",\"type\":\"stream\",\"value\":4.0}}"],
	% Clean up
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, _Body5}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"less_than\",\"input\":5,\"streams\":\"" ++ lib_json:to_string(StreamId1) ++"\", \"vstreams\":\"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, _Body6}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"less_than\",\"input\":10,\"streams\":\"" ++ lib_json:to_string(StreamId1) ++"\", \"vstreams\":\"\"}"}, [], []),
	api_help:refresh(),
	httpc:request(delete, {?WEBMACHINE_URL++"/users/tomas", []}, [], []),


	?assertEqual(true, check_all_exist(NotificationList,ReferenceList)).



%% @doc
%% Function: create_delete_vstream_test/0
%% Purpose: Test the process_post and delete_resource functions on virtual streams by doing some HTTP requests
%% Returns: ok | {error, term()}
%% @end
create_delete_vstream_test() -> 
	%% Create
	{ok, {{_VersionU1, 200, _ReasonPhraseU1}, _HeadersU1, BodyU1}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \"tomas\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"test\",\"input\":5,\"streams\":\"\", \"vstreams\":\"test\"}"}, [], []),
	DocId1 = lib_json:get_field(Body1,"_id"),
	api_help:refresh(),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId1), []}, [], []),
	api_help:refresh(),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"test\",\"input\":5,\"streams\":\"\", \"vstreams\":\"test\"}"}, [], []),
	DocId2 = lib_json:get_field(Body3,"_id"),
	api_help:refresh(),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId2), []}, [], []),
	api_help:refresh(),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"test\",\"input\":6,\"streams\":\"\", \"vstreams\":\"test\"}"}, [], []),
	DocId3 = lib_json:get_field(Body5,"_id"),
	api_help:refresh(),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId3), []}, [], []),
	api_help:refresh(),
	{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"test\",\"input\":6,\"streams\":\"\", \"vstreams\":[\"test\",\"test2\"]}"}, [], []),
	DocId4 = lib_json:get_field(Body7,"_id"),
	api_help:refresh(),
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId4), []}, [], []),
	api_help:refresh(),
	
	%% Create tests
	?assertEqual(lib_json:get_field(Body2,"_source"),lib_json:get_field(Body4,"_source")),
	?assertEqual(DocId1,DocId3),
	?assertNotEqual(DocId1,DocId4),
	?assertEqual(true,lib_json:field_value_exists(Body6,"_source.outputlist[*].input",5)),
	?assertEqual(true,lib_json:field_value_exists(Body6,"_source.outputlist[*].input",6)),
	
	
	
	%% Delete 
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"test\",\"input\":5,\"streams\":\"\", \"vstreams\":\"test\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version10, 200, _ReasonPhrase10}, _Headers10, Body10}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId1), []}, [], []),
	api_help:refresh(),
	{ok, {{_Version11, 200, _ReasonPhrase11}, _Headers11, Body11}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"test\",\"input\":6,\"streams\":\"\", \"vstreams\":\"test\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version12, 200, _ReasonPhrase12}, _Headers12, Body12}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId3), []}, [], []),
	api_help:refresh(),
	{ok, {{_Version13, 200, _ReasonPhrase13}, _Headers13, Body13}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"test\",\"input\":6,\"streams\":\"\", \"vstreams\":[\"test\",\"test2\"]}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version14, 200, _ReasonPhrase14}, _Headers14, Body14}} = httpc:request(get, {?ES_URL++"/sensorcloud/trigger/" ++ lib_json:to_string(DocId4), []}, [], []),
	api_help:refresh(),
	{ok, {{_VersionU2, 200, _ReasonPhraseU2}, _HeadersU2, BodyU2}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/tomas", []}, [], []),
	api_help:refresh(),
	%% Delete tests
	?assertEqual(true,(lib_json:get_field(Body12,"exist") == false) or (lib_json:get_field(Body12,"_source.outputlist") == [])), %% Answer will depend on how quick messages to the triggersProcess are
	?assertEqual(true,(lib_json:get_field(Body14,"exist") == false) or (lib_json:get_field(Body14,"_source.outputlist") == [])). %% Answer will depend on how quick messages to the triggersProcess are



%% @doc
%% Function: post_data_vstream_test/0
%% Purpose: Test the triggersProcess by doing some posting for data to
%%          the virtual streams the trigger is on
%% Returns: ok | {error, term()}
%% @end
post_data_exchange_vstream_test() ->
    Descript = "Trigger data exchange test",
    Setup = 
	fun() ->
		User1 = "test1",
		User2 = "test2",
		http:post(?WEBMACHINE_URL++"/users", "{\"username\" : \""++User1++"\"}"),
		http:post(?WEBMACHINE_URL++"/users", "{\"username\" : \""++User2++"\"}"),
		api_help:refresh(),
		{200, SBody1} = http:post(?WEBMACHINE_URL++"/streams", "{\"name\":\"Stream1\",\"user_id\":\""++User1++"\"}"),
		{200, SBody2} = http:post(?WEBMACHINE_URL++"/streams", "{\"name\":\"Stream2\",\"user_id\":\""++User1++"\"}"),
		api_help:refresh(),
		StreamId1 = lib_json:to_string(lib_json:get_field(SBody1,"_id")),
		StreamId2 = lib_json:to_string(lib_json:get_field(SBody2,"_id")),
		api_help:refresh(),
		{200, Body1} = http:post(?WEBMACHINE_URL++"/vstreams", "{\"user_id\":\""++User1++"\",\"name\":\"VStream1\",\"tags\":\"average,temperature,celsius,uppsala\",\"description\":\"Diff\",\"private\":\"false\",\"timestampfrom\" : \"now-1w\",\"streams_involved\": [\"" ++ StreamId1 ++ "\"],\"function\":[\"diff\", \"2s\"]}"),
		{200, Body2} = http:post(?WEBMACHINE_URL++"/vstreams", "{\"user_id\":\""++User2++"\",\"name\":\"VStream1\",\"tags\":\"average,temperature,celsius,uppsala\",\"description\":\"Diff\",\"private\":\"false\",\"timestampfrom\" : \"now-1w\",\"streams_involved\": [\"" ++ StreamId2 ++ "\"],\"function\":[\"diff\", \"2s\"]}"),
		api_help:refresh(),
		VStreamId1 = lib_json:to_string(lib_json:get_field(Body1,"_id")),
		VStreamId2 = lib_json:to_string(lib_json:get_field(Body2,"_id")),
		{200, Body3} = http:post(?WEBMACHINE_URL++"/users/"++User1++"/triggers/add",
					 "{\"function\":\"less_than\",\"input\":5,\"streams\":\"\", \"vstreams\":\""++VStreamId1++"\"}"),		
		api_help:refresh(),
		{200, Body4} = http:post(?WEBMACHINE_URL++"/users/"++User2++"/triggers/add",
					 "{\"function\":\"less_than\",\"input\":5,\"streams\":\"\", \"vstreams\":\""++VStreamId1++"\"}"),
		api_help:refresh(),
		{200, Body5} = http:post(?WEBMACHINE_URL++"/users/"++User1++"/triggers/add",
					 "{\"function\":\"less_than\",\"input\":10,\"streams\":\"\", \"vstreams\":\""++VStreamId1++"\"}"),
		api_help:refresh(),
		{200, Body6} = http:post(?WEBMACHINE_URL++"/users/"++User1++"/triggers/add",
					 "{\"function\":\"less_than\",\"input\":6,\"streams\":\"\", \"vstreams\":[\""++VStreamId1++"\",\""++VStreamId2++"\"]}"),
		TriggerId1 = lib_json:to_string(lib_json:get_field(Body3, "_id")),
		TriggerId2 = lib_json:to_string(lib_json:get_field(Body6, "_id")),
		api_help:refresh(),

		%% Connect.
		{ok, Connection} = amqp_connection:start(#amqp_params_network{}),
		%% Open In and OUT channels.
		{ok, ChannelIn} = amqp_connection:open_channel(Connection),
		InputExchanges = [list_to_binary("trigger."++TriggerId1),
				  list_to_binary("trigger."++TriggerId2)],		
		triggersProcess:subscribe(ChannelIn, InputExchanges),
		{User1, User2, StreamId1, StreamId2, ChannelIn, Connection,VStreamId1,VStreamId2}
	end,
    Test = 
	fun({User1, User2, StreamId1, StreamId2,  _ChannelIn, _Connection,VStreamId1,VStreamId2}) ->
		{200, Body7} = http:post(?WEBMACHINE_URL++"/streams/"++StreamId1++"/data", "{\"value\" : 4}"),
		api_help:refresh(),
		{200, Body8} = http:post(?WEBMACHINE_URL++"/streams/"++StreamId1++"/data", "{\"value\" : 7}"),
		api_help:refresh(),
		{200, Body9} = http:post(?WEBMACHINE_URL++"/streams/"++StreamId2++"/data", "{\"value\" : 4}"),
		api_help:refresh(),	
		{200, Body10} = http:post(?WEBMACHINE_URL++"/streams/"++StreamId2++"/data", "{\"value\" : 4}"),
		api_help:refresh(),
		Messages = [{3,VStreamId1,5, [{user,User2},{user,User1}], "vstream"},
			    	{3,VStreamId1,10,[{user,User1}], "vstream"},
			    	{0,VStreamId2,6, [{user,User1}], "vstream"}],
		receive_loop(Messages)
	end,
    Cleanup = 
	fun({User1, User2, StreamId1, StreamId2, ChannelIn, Connection,VStreamId1,VStreamId2}) ->
		amqp_channel:close(ChannelIn),
		amqp_connection:close(Connection),
		api_help:refresh(),
		{200, Body11} = http:post(?WEBMACHINE_URL++"/users/"++User1++"/triggers/remove",
					 "{\"function\":\"less_than\",\"input\":5,\"streams\":\"\", \"vstreams\":\""++VStreamId1++"\"}"),		
		api_help:refresh(),
		{200, Body12} = http:post(?WEBMACHINE_URL++"/users/"++User2++"/triggers/remove",
					 "{\"function\":\"less_than\",\"input\":5,\"streams\":\"\", \"vstreams\":\""++VStreamId1++"\"}"),
		api_help:refresh(),
		{200, Body13} = http:post(?WEBMACHINE_URL++"/users/"++User1++"/triggers/remove",
					 "{\"function\":\"less_than\",\"input\":10,\"streams\":\"\", \"vstreams\":\""++VStreamId1++"\"}"),
		api_help:refresh(),
		{200, Body14} = http:post(?WEBMACHINE_URL++"/users/"++User1++"/triggers/remove",
					 "{\"function\":\"less_than\",\"input\":6,\"streams\":\"\", \"vstreams\":[\""++VStreamId1++"\",\""++VStreamId2++"\"]}"),
		api_help:refresh(),
		{200, Body15} = http:delete(?WEBMACHINE_URL++"/vstreams/"++VStreamId1),
		api_help:refresh(),
		{200, Body16} = http:delete(?WEBMACHINE_URL++"/vstreams/"++VStreamId2),
		api_help:refresh(),
		{200, Body17} = http:delete(?WEBMACHINE_URL++"/users/"++User1),
		api_help:refresh(),
		{200, Body18} = http:delete(?WEBMACHINE_URL++"/users/"++User2),
		api_help:refresh()
	end,
    {timeout, 30, [{Descript, {setup, Setup, Cleanup, Test}}]}.


%% @doc
%% Function: post_data_vstream_test/0
%% Purpose: Test the triggersProcess by doing some posting for data to
%%          the streams the trigger is on
%% Returns: ok | {error, term()}
%% @end
post_data_user_vstream_test() ->
	{timeout,30,fun post_data_user_vstream/0}.

post_data_user_vstream() ->
	timer:sleep(2000),
	User1 = "tomas",
	User2 = "erik",
    httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \""++User1++"\"}"}, [], []),
    httpc:request(post, {?WEBMACHINE_URL++"/users", [],"application/json", "{\"username\" : \""++User2++"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, SBody1}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [],"application/json", "{\"name\" : \"Stream1\",\"user_id\":\"tomas\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, SBody2}} = httpc:request(post, {?WEBMACHINE_URL++"/streams", [],"application/json", "{\"name\" : \"Stream2\",\"user_id\":\"tomas\"}"}, [], []),
	StreamId1 = lib_json:to_string(lib_json:get_field(SBody1,"_id")),
	StreamId2 = lib_json:to_string(lib_json:get_field(SBody2,"_id")),
	api_help:refresh(),
	{200, Body1} = http:post(?WEBMACHINE_URL++"/vstreams", "{\"user_id\":\""++User1++"\",\"name\":\"VStream1\",\"tags\":\"average,temperature,celsius,uppsala\",\"description\":\"Diff\",\"private\":\"false\",\"timestampfrom\" : \"now-1w\",\"streams_involved\": [\"" ++ StreamId1 ++ "\"],\"function\":[\"diff\", \"2s\"]}"),
	{200, Body2} = http:post(?WEBMACHINE_URL++"/vstreams", "{\"user_id\":\""++User2++"\",\"name\":\"VStream1\",\"tags\":\"average,temperature,celsius,uppsala\",\"description\":\"Diff\",\"private\":\"false\",\"timestampfrom\" : \"now-1w\",\"streams_involved\": [\"" ++ StreamId2 ++ "\"],\"function\":[\"diff\", \"2s\"]}"),
	api_help:refresh(),
	VStreamId1 = lib_json:to_string(lib_json:get_field(Body1,"_id")),
	VStreamId2 = lib_json:to_string(lib_json:get_field(Body2,"_id")),
	api_help:refresh(),
	%% Create
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"less_than\",\"input\":5,\"streams\":\"\", \"vstreams\":\"" ++ lib_json:to_string(VStreamId1) ++"\"}"}, [], []),
	TriggerId1 = lib_json:get_field(Body3, "_id"),
	api_help:refresh(),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(post, {?WEBMACHINE_URL++"/users/erik/triggers/add", [],"application/json", "{\"function\" : \"less_than\",\"input\":5,\"streams\":\"\", \"vstreams\":\"" ++ lib_json:to_string(VStreamId1) ++"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"less_than\",\"input\":10,\"streams\":\"\", \"vstreams\":\"" ++ lib_json:to_string(VStreamId1) ++"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/add", [],"application/json", "{\"function\" : \"less_than\",\"input\":6,\"streams\":\"\", \"vstreams\":[\"" ++ lib_json:to_string(VStreamId1) ++"\",\"" ++ lib_json:to_string(VStreamId2) ++"\"]}"}, [], []),
	TriggerId2 = lib_json:get_field(Body6, "_id"),
	api_help:refresh(),
	{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(post, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(StreamId1) ++"/data", [],"application/json", "{\"value\" : 4.0}"}, [], []),
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(post, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(StreamId1) ++"/data", [],"application/json", "{\"value\" : 7.0}"}, [], []),
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(post, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(StreamId2) ++"/data", [],"application/json", "{\"value\" : 4.0}"}, [], []),
	{ok, {{_Version10, 200, _ReasonPhrase10}, _Headers10, Body10}} = httpc:request(post, {?WEBMACHINE_URL++"/streams/" ++ lib_json:to_string(StreamId2) ++"/data", [],"application/json", "{\"value\" : 4.0}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version11, 200, _ReasonPhrase11}, _Headers11, Body11}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"less_than\",\"input\":5,\"streams\":\"\", \"vstreams\":\"" ++ lib_json:to_string(VStreamId1) ++"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version12, 200, _ReasonPhrase12}, _Headers12, Body12}} = httpc:request(post, {?WEBMACHINE_URL++"/users/erik/triggers/remove", [],"application/json", "{\"function\" : \"less_than\",\"input\":5,\"streams\":\"\", \"vstreams\":\"" ++ lib_json:to_string(VStreamId1) ++"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version13, 200, _ReasonPhrase13}, _Headers13, Body13}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"less_than\",\"input\":10,\"streams\":\"\", \"vstreams\":\"" ++ lib_json:to_string(VStreamId1) ++"\"}"}, [], []),
	api_help:refresh(),
	{ok, {{_Version14, 200, _ReasonPhrase14}, _Headers14, Body14}} = httpc:request(post, {?WEBMACHINE_URL++"/users/tomas/triggers/remove", [],"application/json", "{\"function\" : \"less_than\",\"input\":6,\"streams\":\"\", \"vstreams\":[\"" ++ lib_json:to_string(VStreamId1) ++"\",\"" ++ lib_json:to_string(VStreamId2) ++"\"]}"}, [], []),
	api_help:refresh(),
	timer:sleep(2000),
	{ok, {{_VersionU3, 200, _ReasonPhraseU3}, _HeadersU3, BodyU3}} = httpc:request(get, {?WEBMACHINE_URL++"/users/tomas", []}, [], []),
	{ok, {{_VersionU4, 200, _ReasonPhraseU4}, _HeadersU4, BodyU4}} = httpc:request(get, {?WEBMACHINE_URL++"/users/erik", []}, [], []),
	api_help:refresh(),
	{ok, {{_VersionU5, 200, _ReasonPhraseU5}, _HeadersU5, BodyU5}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/tomas", []}, [], []),
	{ok, {{_VersionU6, 200, _ReasonPhraseU6}, _HeadersU6, BodyU6}} = httpc:request(delete, {?WEBMACHINE_URL++"/users/erik", []}, [], []),
	api_help:refresh(),
	%{200, Body15} = http:delete(?WEBMACHINE_URL++"/vstreams/"++VStreamId1),
	api_help:refresh(),
	%{200, Body16} = http:delete(?WEBMACHINE_URL++"/vstreams/"++VStreamId2),
	api_help:refresh(),
	NotificationList1 = lists:map(fun(A) -> lib_json:rm_field(A, "trigger.timestamp") end,lib_json:get_field(BodyU3,"notifications")),
	NotificationList2 = lists:map(fun(A) -> lib_json:rm_field(A, "trigger.timestamp") end,lib_json:get_field(BodyU4,"notifications")),
	
	
	
	ReferenceList1 = ["{\"trigger\":{\"input\":10,\"stream_id\":\"" ++ lib_json:to_string(VStreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId1) ++ "\",\"type\":\"vstream\",\"value\":0.0}}",
					  "{\"trigger\":{\"input\":5,\"stream_id\":\"" ++ lib_json:to_string(VStreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId1) ++ "\",\"type\":\"vstream\",\"value\":0.0}}",
					  "{\"trigger\":{\"input\":10,\"stream_id\":\"" ++ lib_json:to_string(VStreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId1) ++ "\",\"type\":\"vstream\",\"value\":3.0}}",
					  "{\"trigger\":{\"input\":5,\"stream_id\":\"" ++ lib_json:to_string(VStreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId1) ++ "\",\"type\":\"vstream\",\"value\":3.0}}",
					  "{\"trigger\":{\"input\":6,\"stream_id\":\"" ++ lib_json:to_string(VStreamId2) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId2) ++ "\",\"type\":\"vstream\",\"value\":0.0}}",
					  "{\"trigger\":{\"input\":6,\"stream_id\":\"" ++ lib_json:to_string(VStreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId2) ++ "\",\"type\":\"vstream\",\"value\":0.0}}",
					  "{\"trigger\":{\"input\":6,\"stream_id\":\"" ++ lib_json:to_string(VStreamId2) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId2) ++ "\",\"type\":\"vstream\",\"value\":0.0}}",
					  "{\"trigger\":{\"input\":6,\"stream_id\":\"" ++ lib_json:to_string(VStreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId2) ++ "\",\"type\":\"vstream\",\"value\":3.0}}",
					  "{\"trigger\":{\"input\":6,\"stream_id\":\"" ++ lib_json:to_string(VStreamId2) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId2) ++ "\",\"type\":\"vstream\",\"value\":0.0}}",
					  "{\"trigger\":{\"input\":6,\"stream_id\":\"" ++ lib_json:to_string(VStreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId2) ++ "\",\"type\":\"vstream\",\"value\":3.0}}",
					  "{\"trigger\":{\"input\":6,\"stream_id\":\"" ++ lib_json:to_string(VStreamId2) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId2) ++ "\",\"type\":\"vstream\",\"value\":0.0}}",
					  "{\"trigger\":{\"input\":6,\"stream_id\":\"" ++ lib_json:to_string(VStreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId2) ++ "\",\"type\":\"vstream\",\"value\":3.0}}"],
	ReferenceList2 = ["{\"trigger\":{\"input\":5,\"stream_id\":\"" ++ lib_json:to_string(VStreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId1) ++ "\",\"type\":\"vstream\",\"value\":3.0}}",
					  "{\"trigger\":{\"input\":5,\"stream_id\":\"" ++ lib_json:to_string(VStreamId1) ++"\",\"trigger_id\":\"" ++ lib_json:to_string(TriggerId1) ++ "\",\"type\":\"vstream\",\"value\":0.0}}"],
	?assertEqual(true, check_all_exist(NotificationList1,ReferenceList1)),
	?assertEqual(true, check_all_exist(NotificationList2,ReferenceList2)).


%% @doc
%% Function: receive_loop/1
%% Purpose: Receives the messages in the list
%%          will be stuck if it do not get the
%%          messages in the list and the test
%%          will time-out
%% Returns: ok 
%% @end
receive_loop([]) ->
	ok;
receive_loop([{Value,StreamId,Threshold,Users,Type}|Rest]) ->
	receive
		{#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
			{Value1,_,StreamId1,Threshold1,Users1,Type1} = binary_to_term(Body),
			case {Value1,StreamId1,Threshold1,Users1,Type1} == {Value,StreamId,Threshold,Users,Type} of
				true ->
					receive_loop(Rest);
				false ->
					self() ! {#'basic.deliver'{}, #amqp_msg{payload = Body}},
					receive_loop([{Value,StreamId,Threshold,Users,Type}|Rest])
			end
	end.
%% @doc
%% Function: check_all_exist/2
%% Purpose: See if both lists contain the same elements
%% Returns: true if both lists contain the same elements, false otherwise
%% @end

check_all_exist([],[]) ->
	true;
check_all_exist([],_Reference) ->
	false;
check_all_exist([First|Rest],Reference) ->
	case lists:member(First, Reference) of
		true ->
			check_all_exist(Rest,lists:delete(First, Reference));
		false ->
			false
	end.
	


