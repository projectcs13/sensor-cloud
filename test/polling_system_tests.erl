%% @author Li Hao
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == polling_system_tests ==
%% This module contains tests of the polling system
%% needed for the polling system. 
%%
%% @end

-module(polling_system_tests).

-include("common.hrl").
-include("poller.hrl").
-include("parser.hrl").
-include("state.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include_lib("amqp_client.hrl").

%% before running this testing code, please change the following address to your address
%% the python server code locates in scripts/python/cgi-bin folder
%% under the python folder, run the following command:
%% python -m CGIHTTPServer 8001
-ifndef(POLL_ADD).
-define(POLL_ADD, "http://localhost:8001/cgi-bin/temperature.py").
-endif.

-ifndef(POLL_ADD2).
-define(POLL_ADD2 ,"http://localhost:8002/cgi-bin/humidity.py").
-endif.

-export([test_rabbit_messages/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc
%% Function: inti_test/0
%% Purpose: Used to start the inets to be able to do HTTP requests
%% Returns: {ok/error, {{Version, Code, Reason}, Headers, Body}}
%%
%% Side effects: Start inets
%% @end
-spec init_test() -> {ok, tuple()} | {error, tuple()}.
init_test() ->
	inets:start(),
	case whereis(polling_supervisor) of
		undefined->continue;
		Pid->exit(Pid, "stops!"),
			 exit(whereis(polling_monitor), "stops!")
	end,
	
	%%insert a new stream
	clear_stream_type(),
	api_help:refresh(),
    post_stream_with_id("1", "test", ?POLL_ADD, 1, "application/json"),
	post_stream_with_id("2", "test2", ?POLL_ADD2, 2, "application/json"),
	
    %%insert two new parsers
	clear_parser_type(),
	api_help:refresh(),
    post_parser("1", "application/json","streams/temperature/value"),
	post_parser("2", "application/json","streams/humidity/value"),
	api_help:refresh().

%% @doc
%% Function: initialization_test/0
%% Purpose: Test if the testing data has been inserted into elasticsearch
%% Returns: true.
%% @end
-spec initialization_test() -> true.
initialization_test()->
	PollerInforList = poll_help:json_to_record_streams(poll_help:get_streams_using_polling()),
	Stream1 = lists:nth(1, PollerInforList),
	Stream2 = lists:nth(2, PollerInforList), 
	?assertEqual(2, length(PollerInforList)),
	?assertEqual("1", Stream1#pollerInfo.stream_id),
	?assertEqual("test", Stream1#pollerInfo.name),
	?assertEqual(?POLL_ADD, Stream1#pollerInfo.uri),
	?assertEqual(1, Stream1#pollerInfo.frequency),
	
	?assertEqual("2", Stream2#pollerInfo.stream_id),
	?assertEqual("test2", Stream2#pollerInfo.name),
	?assertEqual(?POLL_ADD2, Stream2#pollerInfo.uri),
	?assertEqual(2, Stream2#pollerInfo.frequency),
	
	Parser1 = poll_help:get_parser_by_id("1"),
	?assertEqual(true, is_record(Parser1, parser)),
	?assertEqual("application/json", Parser1#parser.input_type),
	?assertEqual("streams/temperature/value", Parser1#parser.input_parser),
	Parser2 = poll_help:get_parser_by_id("2"),
	?assertEqual(true, is_record(Parser2, parser)),
	?assertEqual("application/json", Parser2#parser.input_type),
	?assertEqual("streams/humidity/value", Parser2#parser.input_parser),
	
	%% generate threads to receive commming messages
	register(test_rabbit_1, spawn(?MODULE, test_rabbit_messages, ["1"])),
	register(test_rabbit_2, spawn(?MODULE, test_rabbit_messages, ["2"])).

%% @doc
%% Function: polling_system_test/0
%% Purpose: Test if the polling system could be started and generate necessary pollers
%% Returns: ok | {error, term()}.
%% @end
-spec polling_system_test() -> ok | {error, term()}.
polling_system_test()->
	polling_system:start_link(),
	timer:sleep(1000),
	?assertNotEqual(undefined, whereis(polling_supervisor)),
	?assertNotEqual(undefined, whereis(polling_monitor)),
	ChildrenList = supervisor:which_children(polling_monitor),
	?assertEqual(2, length(ChildrenList)),
	{_, Pid1, _, _} = lists:nth(1, ChildrenList),
	{_, Pid2, _, _} = lists:nth(2, ChildrenList),
	{info, State1} = gen_server:call(Pid1, {check_info}),
	{info, State2} = gen_server:call(Pid2, {check_info}),
	
	?assertEqual(true, is_record(State1, state)),
	?assertEqual(true, is_record(State2, state)),
	
	case State1#state.stream_id of
		"1"->
			?assertEqual("2", State2#state.stream_id),
			?assertEqual(?POLL_ADD2, State2#state.uri),
			?assertEqual(<<"streams.2">>, State2#state.exchange),
			?assertNotEqual(undefined, State2#state.channel),
			Parser2 = State2#state.parser,
			?assertEqual("application/json", Parser2#parser.input_type),
			?assertEqual("streams/humidity/value", Parser2#parser.input_parser),
			
			?assertEqual(?POLL_ADD, State1#state.uri),
			?assertEqual(<<"streams.1">>, State1#state.exchange),
			?assertNotEqual(undefined, State1#state.channel),
			Parser1 = State1#state.parser,
			?assertEqual("application/json", Parser1#parser.input_type),
			?assertEqual("streams/temperature/value", Parser1#parser.input_parser);
		"2"->
			?assertEqual("1", State2#state.stream_id),
			?assertEqual(?POLL_ADD, State2#state.uri),
			Parser2 = State2#state.parser,
			?assertEqual("application/json", Parser2#parser.input_type),
			?assertEqual("streams/temperature/value", Parser2#parser.input_parser),
			
			?assertEqual(?POLL_ADD2, State1#state.uri),
			Parser1 = State1#state.parser,
			?assertEqual("application/json", Parser1#parser.input_type),
			?assertEqual("streams/humidity/value", Parser1#parser.input_parser);
		_->
			?assert(false)
	end.

%% @doc
%% Function: rebuild_system_test/0
%% Purpose: Test if the polling sytem could rebuild the poller
%% Returns: ok | {error, term()}.
%% @end
-spec rebuild_system_test() -> ok | {error, term()}.
rebuild_system_test()->

	%% testing rebuild
	clear_stream_type(),
	clear_parser_type(),
	api_help:refresh(),
	
    post_stream_with_id(1, "test2", ?POLL_ADD2, 1, "application/json"),
	post_stream_with_id(2, "test1", ?POLL_ADD, 2, "application/json"),
	
	post_parser("2", "application/json","streams/temperature/value"),
	post_parser("1", "application/json","streams/humidity/value"),
	
	api_help:refresh(),
	gen_server:call(polling_supervisor, {rebuild, "1"}),
	gen_server:call(polling_supervisor, {rebuild, "2"}),
	ChildrenList = supervisor:which_children(polling_monitor),
	?assertEqual(2, length(ChildrenList)),
	{_, Pid1, _, _} = lists:nth(1, ChildrenList),
	{_, Pid2, _, _} = lists:nth(2, ChildrenList),
	
	{info, State1} = gen_server:call(Pid1, {check_info}),
	{info, State2} = gen_server:call(Pid2, {check_info}),
	
	?assertEqual(true, is_record(State1, state)),
	?assertEqual(true, is_record(State2, state)),
	
	case State1#state.stream_id of
		"1"->
			?assertEqual(?POLL_ADD2, State1#state.uri),
			?assertEqual("2", State2#state.stream_id),
			?assertEqual(?POLL_ADD, State2#state.uri);
		"2"->
			?assertEqual(?POLL_ADD, State1#state.uri),
			?assertEqual("1", State2#state.stream_id),
			?assertEqual(?POLL_ADD2, State2#state.uri);
		_->
			erlang:display("the stream id of state1: "++State1#state.stream_id),
			?assert(false)
	end,

	%% test after rebuild, if the pollers could poll in right way
	%% remember to uncomment the last line of the parser function, to let parser print json data to shell
	%% if succeed polling, there should be some output on the shell
	erlang:display("!!!!!!!!!!!!!!!!!"),
	erlang:display("has change url to right one"),
	erlang:display("!!!!!!!!!!!!!!!!!"),

	clear_stream_type(),
	clear_parser_type(),
	api_help:refresh(),
	post_stream_with_id(1, "test", ?POLL_ADD, 1, "application/json"),
	post_stream_with_id(2, "test2", ?POLL_ADD2, 2, "application/json"),
	
	post_parser("1", "application/json","streams/temperature/value"),
	post_parser("2", "application/json","streams/humidity/value"),
	
	api_help:refresh(),
	gen_server:call(polling_supervisor, {rebuild, "1"}),
	gen_server:call(polling_supervisor, {rebuild, "2"}),
	
	ChildrenList2 = supervisor:which_children(polling_monitor),
	?assertEqual(2, length(ChildrenList2)),
	{_, Pid21, _, _} = lists:nth(1, ChildrenList2),
	{_, Pid22, _, _} = lists:nth(2, ChildrenList2),
	
	{info, State21} = gen_server:call(Pid21, {check_info}),
	{info, State22} = gen_server:call(Pid22, {check_info}),
	
	?assertEqual(true, is_record(State21, state)),
	?assertEqual(true, is_record(State22, state)),
	
	case State21#state.stream_id of
		"1"->
			?assertEqual(?POLL_ADD, State21#state.uri),
			?assertEqual("2", State22#state.stream_id),
			?assertEqual(?POLL_ADD2, State22#state.uri);
		"2"->
			?assertEqual(?POLL_ADD2, State21#state.uri),
			?assertEqual("1", State22#state.stream_id),
			?assertEqual(?POLL_ADD, State22#state.uri);
		_->
			erlang:display("the stream id of state21: "++State21#state.stream_id),
			?assert(false)
	end.

%% @doc
%% Function: terminate_system_test/0
%% Purpose: Test if polling system could terminate the poller
%% Returns: ok | {error, term()}.
%% @end
-spec terminate_system_test() -> ok | {error, term()}.
terminate_system_test()->
	ChildrenList = supervisor:which_children(polling_monitor),
	?assertEqual(2, length(ChildrenList)),
	gen_server:cast(polling_supervisor, {terminate, "1"}),
	timer:sleep(1000),
	ChildrenList2 = supervisor:which_children(polling_monitor),
	?assertEqual(1, length(ChildrenList2)),
	gen_server:cast(polling_supervisor, {terminate, "2"}),
	timer:sleep(1000),
	ChildrenList3 = supervisor:which_children(polling_monitor),
	?assertEqual(0, length(ChildrenList3)).

%% @doc
%% Function: clear_system_test/0
%% Purpose: clear all the data what have been inserted into elasticsearch
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec clear_system_test() -> {ok, term()} | {ok, saved_to_file} | {error, string()}.
clear_system_test()->

	%% terminate the rabbit testing threads
	case {whereis(test_rabbit_1), whereis(test_rabbit_2)} of
		{undefined, undefined}->
			continue;
		{Pid, undefined}->
			exit(Pid, "it is time to sleep");
		{undefined, Pid}->
			exit(Pid, "it is time to sleep");
		{Pid1, Pid2}->
			exit(Pid1, "it is time to sleep"),
			exit(Pid2, "it is time to sleep")
	end,
	
	%% clear all already stored resource in elasticsearch
	clear_stream_type(),
	clear_parser_type(),
	clear_datapoint_type().

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Function: post_stream_with_id/5
%% Purpose: Post a stream using the values provided.
%% Returns: {ok, Result} | {error, Reason}.
%% @end
-spec post_stream_with_id(Id :: string(), Name :: string(), Uri :: string(), Freq :: integer()|string(), Type :: string()) ->
		  {ok, list()}
		 |{error, string()}.
post_stream_with_id(Id, Name, Uri, Freq, Type)->
	N = case Name of
			"" -> "";
			_ -> "\"name\" : \"" ++ Name ++ "\""
		end,
	U = case Uri of
			"" -> "";
			_ -> ", \"uri\" : \"" ++ Uri ++ "\""
		end,
	F = case is_integer(Freq) of
			true->
				", \"polling_freq\" :" ++ integer_to_list(Freq);
			_->
				", \"polling_freq\" :" ++ Freq
		end,
	T = ", \"data_type\":\"" ++ Type ++"\"",
	Data = "{"++N++U++F++T++"}",
	{ok, _} = erlastic_search:index_doc_with_id(?ES_INDEX, "stream", Id, Data).

%% @doc
%% Function: post_parser/3
%% Purpose: Post a parser using the values provided, if 'InputType' or 'InputParser' is
%%          empty they are ignored.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec post_parser(StreamId :: string(), InputType :: string(), InputParser :: string()) ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
post_parser(StreamId, InputType, InputParser) when is_list(StreamId)->
	It = case InputType of
			 "" -> "";
			 _ -> ", \"input_type\":\"" ++ InputType ++ "\""
		 end,
	Ip = case InputParser of
			 "" -> "";
			 _ -> ", \"input_parser\":\"" ++ InputParser ++ "\""
		 end,
	Si = "\""++StreamId++"\"",
	{ok, Res} = httpc:request(post, {api_help:get_elastic_search_url() ++ "/sensorcloud" ++ "/parser", [],
						 "application/json",
						 "{\"stream_id\":"++Si++It++Ip++"}"
						}, [],[]).

%% @doc
%% Function: clear_stream_type/0
%% Purpose: Delete all the streams in elasticsearch.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec clear_stream_type() ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
clear_stream_type() ->
	httpc:request(delete, {api_help:get_elastic_search_url() ++ "/sensorcloud" ++ "/stream", []}, [], []).

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
	httpc:request(delete, {api_help:get_elastic_search_url() ++ "/sensorcloud" ++ "/parser", []}, [], []).

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
	httpc:request(delete, {api_help:get_elastic_search_url() ++ "/sensorcloud" ++ "/datapoint", []}, [], []).

%% @doc
%% Function: test_rabbit_messages/1
%% Purpose: test if the poller could succeed sending the messages to rabbit MQ, accepts one parameter: the id of stream 
%% Returns: ok
%% @end
-spec test_rabbit_messages(StreamId :: string()) -> ok.
test_rabbit_messages(StreamId)->
	%% Exchange name binarys
	StreamExchange = list_to_binary("streams."++StreamId),
	
	%% Connect
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{host = "localhost"}),
	
	%% Open In and OUT channels
	{ok, ChannelIn} = amqp_connection:open_channel(Connection),
	
	%% Declare INPUT exchange and queue
	amqp_channel:call(ChannelIn, #'exchange.declare'{exchange = StreamExchange, type = <<"fanout">>}),
	#'queue.declare_ok'{queue = QueueIn} = amqp_channel:call(ChannelIn, #'queue.declare'{exclusive = true}),
	amqp_channel:call(ChannelIn, #'queue.bind'{exchange = StreamExchange, queue = QueueIn}),
	
	%% Subscribe to INPUT queue
	amqp_channel:subscribe(ChannelIn, #'basic.consume'{queue = QueueIn, no_ack = true}, self()),
	
	loop(StreamId, ChannelIn).

%% @doc
%% Function: loop/2
%% Purpose: loop function which waits for comming messages and print them on the shell 
%% Returns: ok
%% @end
-spec loop(StreamId :: string(), ChannelIn :: pid()) -> ok.
loop(StreamId, ChannelIn)->
	receive
		{#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
			erlang:display("receive message: "++binary_to_list(Body)),			
			%% Recurse
			loop(StreamId, ChannelIn)
	end.