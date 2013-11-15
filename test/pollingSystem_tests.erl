%% @author Li Hao
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == poll_system_tests ==
%% This module contains tests of the polling system
%% needed for the polling system. 
%%
%% @end

-module(pollingSystem_tests).

-include("common.hrl").
-include("poller.hrl").
-include("parser.hrl").
-include("state.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inets/include/mod_auth.hrl").

%% before running this testing code, please change the following address to your address
%% the python server code locates in scripts/python/cgi-bin folder
%% under the python folder, run the following command:
%% python -m CGIHTTPServer 8000
-ifndef(POLL_ADD).
-define(POLL_ADD, "http://130.238.15.222:8000/cgi-bin/resource.py").
-endif.

-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc
%% Function: inti_test/0
%% Purpose: Used to start the inets to be able to do HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: Start inets
%% @end
-spec init_test() -> ok | {error, term()}.
init_test() ->
	inets:start(),
	
	%%insert a new resource
	clear_resource_type(),
	timer:sleep(500),
    post_resource_with_id(1, "test", ?POLL_ADD, 1000, "application/json"),
	
    %%insert two new parsers
	clear_parser_type(),
	timer:sleep(500),
    post_parser(1,17,"application/json","streams/temperature/value"),
	post_parser(1,15,"application/json","streams/humidity/value"),
	timer:sleep(1000).

%% @doc
%% Function: initialization_test/0
%% Purpose: Test if the testing data has been inserted into elasticsearch
%% Returns: ok | {error, term()}.
%% @end
initialization_test()->
	PollerInforList = poll_help:json_to_record_resources(poll_help:get_resources_using_polling()),
	Resource  = lists:nth(1, PollerInforList),
	?assertEqual(1, length(PollerInforList)),
	?assertEqual("1", Resource#pollerInfo.resourceid),
	?assertEqual("test", Resource#pollerInfo.name),
	?assertEqual(?POLL_ADD, Resource#pollerInfo.url),
	?assertEqual(1000, Resource#pollerInfo.frequency),
	
	ParserList = poll_help:get_parsers_by_id("1"),
	?assertEqual(2, length(ParserList)).

%% @doc
%% Function: polling_system_test/0
%% Purpose: Test if the polling system could be started and generate necessary pollers
%% Returns: ok | {error, term()}.
%% @end
polling_system_test()->
	pollingSystem:start_link(),
	timer:sleep(1000),
	?assertNotEqual(undefined, whereis(polling_supervisor)),
	?assertNotEqual(undefined, whereis(polling_monitor)),
	ChildrenList = supervisor:which_children(polling_monitor),
	?assertEqual(1, length(ChildrenList)),
	{_, Pid, _, _} = lists:nth(1, ChildrenList),
	{info, State} = gen_server:call(Pid, {check_info}),
	?assertEqual(true, is_record(State, state)),
	?assertEqual("1", State#state.resourceid),
	?assertEqual(?POLL_ADD, State#state.url),
	ParserList = State#state.parserslist,
	?assertEqual(2, length(ParserList)),
	Parser1 = lists:nth(1, ParserList),
	Parser2 = lists:nth(2, ParserList),
	?assertEqual(true, is_record(Parser1, parser)),
	?assertEqual(true, is_record(Parser2, parser)),
	?assertEqual(1, Parser1#parser.resource_id),
	?assertEqual(1, Parser2#parser.resource_id),
	?assertEqual(true, lists:member(Parser1#parser.stream_id, [15, 17])),
	?assertEqual(true, lists:member(Parser2#parser.stream_id, [15, 17])),
	?assertNotEqual(Parser1#parser.stream_id, Parser2#parser.stream_id),

	case Parser1#parser.stream_id of
		17->
			?assertEqual("streams/temperature/value", Parser1#parser.input_parser),
			?assertEqual("application/json", Parser1#parser.input_type),
			?assertEqual("streams/humidity/value", Parser2#parser.input_parser),
			?assertEqual("application/json", Parser2#parser.input_type);
		15->
			?assertEqual("streams/temperature/value", Parser2#parser.input_parser),
			?assertEqual("application/json", Parser2#parser.input_type),
			?assertEqual("streams/humidity/value", Parser1#parser.input_parser),
			?assertEqual("application/json", Parser1#parser.input_type);
		_->
			?assertEqual(1,2)
	end.

%% @doc
%% Function: rebuild_system_test/0
%% Purpose: Test if the polling sytem could rebuild the poller
%% Returns: ok | {error, term()}.
%% @end
rebuild_system_test()->

	%% testing rebuild
	clear_resource_type(),
	timer:sleep(500),
	
	%% this new url is fake, only for testing
    post_resource_with_id(1, "test", "http://130.238.15.222:8080/", 1000, "application/json"),
	timer:sleep(500),
	gen_server:cast(polling_supervisor, {rebuild, "1"}),
	timer:sleep(1000),
	ChildrenList = supervisor:which_children(polling_monitor),
	?assertEqual(1, length(ChildrenList)),
	{_, Pid, _, _} = lists:nth(1, ChildrenList),
	{info, State} = gen_server:call(Pid, {check_info}),
	?assertEqual(true, is_record(State, state)),
	?assertEqual("1", State#state.resourceid),
	?assertEqual("http://130.238.15.222:8080/", State#state.url),
	ParsersList = State#state.parserslist,
	?assertEqual(2, length(ParsersList)),

	%% test after rebuild, if the pollers could poll in right way
	%% remember to uncomment the last line of the parser function, to let parser print json data to shell
	%% if succeed polling, there should be some output on the shell
	erlang:display("!!!!!!!!!!!!!!!!!"),
	erlang:display("has change url to right one"),
	erlang:display("!!!!!!!!!!!!!!!!!"),

	clear_resource_type(),
	timer:sleep(500),
	post_resource_with_id(1, "test", ?POLL_ADD, 1000, "application/json"),
	timer:sleep(500),
	gen_server:cast(polling_supervisor, {rebuild, "1"}),
	timer:sleep(1000),
	ChildrenList2 = supervisor:which_children(polling_monitor),
	?assertEqual(1, length(ChildrenList2)),
	{_, Pid2, _, _} = lists:nth(1, ChildrenList2),
	{info, State2} = gen_server:call(Pid2, {check_info}),
	?assertEqual(true, is_record(State2, state)),
	?assertEqual("1", State2#state.resourceid),
	?assertEqual(?POLL_ADD, State2#state.url),
	ParsersList2 = State2#state.parserslist,
	?assertEqual(2, length(ParsersList2)).

%% @doc
%% Function: terminate_system_test/0
%% Purpose: Test if polling system could terminate the poller
%% Returns: ok | {error, term()}.
%% @end
terminate_system_test()->
	ChildrenList = supervisor:which_children(polling_monitor),
	?assertEqual(1, length(ChildrenList)),
	gen_server:cast(polling_supervisor, {terminate, "1"}),
	timer:sleep(1000),
	ChildrenList2 = supervisor:which_children(polling_monitor),
	?assertEqual(0, length(ChildrenList2)).

%% @doc
%% Function: clear_system_test/0
%% Purpose: clear all the data what have been inserted into elasticsearch
%% Returns: ok | {error, term()}.
%% @end
clear_system_test()->

	%% clear all already stored resource in elasticsearch
	clear_resource_type(),
	clear_parser_type(),
	clear_datapoint_type().

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Function: post_resource_with_id/5
%% Purpose: Post a resource using the values provided.
%% Returns: {ok, Result} | {error, Reason}.
%% @end
post_resource_with_id(Id, Name, Url, Freq, Type)->
	N = case Name of
			"" -> "";
			_ -> "\"name\" : \"" ++ Name ++ "\""
		end,
	U = case Url of
			"" -> "";
			_ -> ", \"url\" : \"" ++ Url ++ "\""
		end,
	F = case is_integer(Freq) of
			true->
				", \"polling_freq\" :" ++ integer_to_list(Freq);
			_->
				", \"polling_freq\" :" ++ Freq
		end,
	T = ", \"type\":\"" ++ Type ++"\"",
	Data = "{"++N++U++F++T++"}",
	erlastic_search:index_doc_with_id(?ES_INDEX, "resource", 1, Data).

%% @doc
%% Function: post_parser/4
%% Purpose: Post a parser using the values provided, if 'InputType' or 'InputParser' is
%%          empty they are ignored.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec post_parser(ResourceId :: integer(), StreamId :: integer(), InputType :: string(), InputParser :: string()) ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
post_parser(ResourceId, StreamId, InputType, InputParser) when is_integer(ResourceId), is_integer(StreamId)->
	It = case InputType of
			 "" -> "";
			 _ -> ", \"input_type\":\"" ++ InputType ++ "\""
		 end,
	Ip = case InputParser of
			 "" -> "";
			 _ -> ", \"input_parser\":\"" ++ InputParser ++ "\""
		 end,
	Ri = integer_to_list(ResourceId),
	Si = integer_to_list(StreamId),
	{ok, Res} = httpc:request(post, {?ES_ADDR ++ "/parser", [],
						 "application/json",
						 "{\"resource_id\":"++Ri++", \"stream_id\":"++Si++It++Ip++"}"
						}, [],[]).

%% @doc
%% Function: clear_resource_type/0
%% Purpose: Delete all the resource in elasticsearch.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec clear_resource_type() ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
clear_resource_type() ->
	httpc:request(delete, {?ES_ADDR ++ "/resource", []}, [], []).

%% @doc
%% Function: clear_parser_type/0
%% Purpose: Delete all the parsers in elasticsearch.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec clear_parser_type() ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
clear_parser_type() ->
	httpc:request(delete, {?ES_ADDR ++ "/parser", []}, [], []).

%% @doc
%% Function: clear_datapoint_type/0
%% Purpose: Delete all the datapoints in elasticsearch.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec clear_datapoint_type() ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
clear_datapoint_type() ->
	httpc:request(delete, {?ES_ADDR ++ "/datapoint", []}, [], []).