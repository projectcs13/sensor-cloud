%% @author Gabriel Tholsgård
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%%
%% @doc == virtual_stream_process_tests ==
%% This module contains several tests to test the functionallity
%% of the pub/sub system, including streams and virtual streams.
%%
%% @end

-module(virtual_stream_process_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client.hrl").
-include_lib("pubsub.hrl").
-include_lib("json.hrl").
-include_lib("common.hrl").
-include_lib("debug.hrl").



%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================



virtual_stream_process_test_() ->
	S = fun() -> ok end,
	C = fun(_) -> ok end,
	[
	 {timeout, 30, [{setup, S, C, fun start_and_terminate/1}]},
	 {timeout, 30, [{setup, S, C, fun vstream_subscribe_to_a_stream/1}]},
	 {timeout, 30, [{setup, S, C, fun vstream_subscribe_to_streams_interval/1}]},
	 {timeout, 30, [{setup, S, C, fun vstream_subscribe_to_streams_sim/1}]}
	].




%% @doc
%% Function: start_and_terminate/1
%% Purpose: Starts a virtual stream process and terminates it.
%% Returns: ok | {error, term()}
%%
%% @end
-spec start_and_terminate(_) -> ok | {error, term()}.
start_and_terminate(_) ->
	Pid = spawn(virtual_stream_process, create, ["1", [], "test"]),
	Info1 = is_process_alive(Pid),
	%%?assertNotEqual(undefined, process_info(Pid)),
	Pid ! quit,
	timer:sleep(1500),
	Info2 = is_process_alive(Pid),
	%%?assertEqual(undefined, process_info(Pid)).
	[?_assertEqual(true, Info1),
	 ?_assertEqual(false, Info2)].






%% @doc
%% Function: vstream_subscribe_to_a_stream/1
%% Purpose: Starts a virtual stream process that subscribes to a stream, gets a
%%          value, apply the function and publish it.
%% Returns: ok | {error, term()}.
%% Side effects: Create one stream and stores a data point in ES.
%% 
%% @end
-spec vstream_subscribe_to_a_stream(_) -> ok | {error, term()}.
vstream_subscribe_to_a_stream(_) ->

	%% Create a stream in ?ES_INDEX
	StreamId = create_a_stream_on_index(?ES_INDEX),
	
	%% Create virtual stream listening to our stream
	VPid = create_virtual_stream("testID", [{stream, StreamId}], "test"),
	
	%% Create a publisher and consumer exchanges
	OutExchange = list_to_binary("streams." ++ StreamId),
	InExchange = list_to_binary("vstreams.testID"),
	
	%% Establish connection to RabbitMQ
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{}),
	{ok, ChannelIn} = amqp_connection:open_channel(Connection),
	{ok, ChannelOut} = amqp_connection:open_channel(Connection),
	
	%% Subscribe to InExchange
	subscribe(ChannelIn, InExchange),
	
	%% Declare the outgoing exchange to publish to
	amqp_channel:call(ChannelOut,
					  #'exchange.declare'{exchange = OutExchange,
										  type = <<"fanout">>}),
	
	%% Needed for the RabbitMQ to have time to set up the system.
	timer:sleep(1000),
	
	%% Publish a message
	Msg = create_data_point(StreamId, 32),
	amqp_channel:cast(ChannelOut,
					  #'basic.publish'{exchange = OutExchange},
					  #amqp_msg{payload = Msg}),	
	
	
	%% Listen for the update
	Rec = receive
			{#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
				Data = binary_to_list(Body),
				case erlson:is_json_string(Data) of
					%% New value from the source as a Json
					true ->
						Value = lib_json:get_field(Data, "value"),
						Id = binary_to_list(
								lib_json:get_field(Data, "stream_id")),
						{Value, Id};
					_ -> false
				end;
			_ -> false
		  end,
	
	%% Close and clean
	VPid ! quit,
	amqp_channel:close(ChannelIn),
	amqp_channel:close(ChannelOut),
	amqp_connection:close(Connection),
	
	timer:sleep(1000),

	[?_assertEqual(true, is_list(StreamId)),
	 ?_assertEqual({32, "testID"}, Rec)].
	







%% @doc
%% Function: vstream_subscribe_to_streams_interval/1
%% Purpose: Starts a virtual stream process that subscribes to streams, gets a
%%          value, apply the function and publish it.
%% Returns: ok | {error, term()}.
%% Side effects: Deletes and creates ?ES_INDEX in ES, create two streams
%%               and stores two data point in ES.
%% 
%% @end
-spec vstream_subscribe_to_streams_interval(_) -> ok | {error, term()}.
vstream_subscribe_to_streams_interval(_) ->
	
	%% Create two streams in ?ES_INDEX
	StreamId1 = create_a_stream_on_index(?ES_INDEX),
	StreamId2 = create_a_stream_on_index(?ES_INDEX),

	%% Create virtual stream listening to our streams
	VPid = create_virtual_stream("testID",
								 [{stream, StreamId1}, {stream, StreamId2}],
								 "test"),
	
	%% Create publisher and consumer exchanges
	OutExchange1 = list_to_binary("streams." ++ StreamId1),
	OutExchange2 = list_to_binary("streams." ++ StreamId2),
	InExchange = list_to_binary("vstreams.testID"),
	
	%% Establish connection to RabbitMQ
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{}),
	{ok, ChannelIn} = amqp_connection:open_channel(Connection),
	{ok, ChannelOut} = amqp_connection:open_channel(Connection),
	
	%% Declare the outgoing exchange 1 and 2 to publish to
	amqp_channel:call(ChannelOut,
					  #'exchange.declare'{exchange = OutExchange1,
										  type = <<"fanout">>}),
	amqp_channel:call(ChannelOut,
					  #'exchange.declare'{exchange = OutExchange2,
										  type = <<"fanout">>}),
	
	%% Subscribe to InExchange
	subscribe(ChannelIn, InExchange),
	
	%% Needed for the RabbitMQ to have time to set up the system.
	timer:sleep(1000),
	
	%% Publish a message
	Msg = create_data_point(StreamId1, 32),
	amqp_channel:cast(ChannelOut,
					  #'basic.publish'{exchange = OutExchange1},
					  #amqp_msg{payload = Msg}),
	
	%% Listen for the update
	Rec1 = receive
			{#'basic.deliver'{}, #amqp_msg{payload = Body1}} ->
				Data1 = binary_to_list(Body1),
				case erlson:is_json_string(Data1) of
					%% New value from the source as a Json
					true ->
						Val1 = lib_json:get_field(Data1, "value"),
						Id1 = binary_to_list(
								lib_json:get_field(Data1, "stream_id")),
						{Val1, Id1};
					_ -> false
				end;
			_ -> false
		  end,	
	
	
	
	%% Publish a message
	Msg2 = create_data_point(StreamId2, 32),
	amqp_channel:cast(ChannelOut,
					  #'basic.publish'{exchange = OutExchange2},
					  #amqp_msg{payload = Msg2}),
	
	%% Listen for the update
	Rec2 = receive
			{#'basic.deliver'{}, #amqp_msg{payload = Body2}} ->
				Data2 = binary_to_list(Body2),
				case erlson:is_json_string(Data2) of
					%% New value from the source as a Json
					true ->
						Val2 = lib_json:get_field(Data2, "value"),
						Id2 = binary_to_list(
								lib_json:get_field(Data2, "stream_id")),
						{Val2, Id2};
					_ -> false
				end;
			_ -> false
		  end,
	
	%% Close and clean
	VPid ! quit,
	amqp_channel:close(ChannelIn),
	amqp_channel:close(ChannelOut),
	amqp_connection:close(Connection),

	[?_assertEqual(true, is_list(StreamId1)),
	 ?_assertEqual(true, is_list(StreamId2)),
	 ?_assertEqual({32, "testID"}, Rec1),
	 ?_assertEqual({64, "testID"}, Rec2)].








%% @doc
%% Function: vstream_subscribe_to_streams_sim/1
%% Purpose: Starts a virtual stream process that subscribes to streams, gets a
%%          value, apply the function and publish it.
%% Returns: ok | {error, term()}.
%% Side effects: Deletes and creates ?ES_INDEX in ES, create two streams
%%               and stores two data point in ES.
%% 
%% @end
-spec vstream_subscribe_to_streams_sim(_) -> ok | {error, term()}.
vstream_subscribe_to_streams_sim(_) ->
	
	%% Create two streams in ?ES_INDEX
	StreamId1 = create_a_stream_on_index(?ES_INDEX),
	StreamId2 = create_a_stream_on_index(?ES_INDEX),

	%% Create virtual stream listening to our streams
	VPid = create_virtual_stream("testID",
								 [{stream, StreamId1}, {stream, StreamId2}],
								 "test"),
	
	%% Create publisher and consumer exchanges
	OutExchange1 = list_to_binary("streams." ++ StreamId1),
	OutExchange2 = list_to_binary("streams." ++ StreamId2),
	InExchange = list_to_binary("vstreams.testID"),
	
	%% Establish connection to RabbitMQ
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{}),
	{ok, ChannelIn} = amqp_connection:open_channel(Connection),
	{ok, ChannelOut} = amqp_connection:open_channel(Connection),
	
	%% Declare the outgoing exchange 1 and 2 to publish to
	amqp_channel:call(ChannelOut,
					  #'exchange.declare'{exchange = OutExchange1,
										  type = <<"fanout">>}),
	amqp_channel:call(ChannelOut,
					  #'exchange.declare'{exchange = OutExchange2,
										  type = <<"fanout">>}),
	
	%% Subscribe to InExchange
	subscribe(ChannelIn, InExchange),
	
	%% Needed for the RabbitMQ to have time to set up the system.
	timer:sleep(1000),
	
	%% Publish a message
	Msg = create_data_point(StreamId1, 32),
	amqp_channel:cast(ChannelOut,
					  #'basic.publish'{exchange = OutExchange1},
					  #amqp_msg{payload = Msg}),
	
	%% Publish a message
	Msg2 = create_data_point(StreamId2, 32),
	amqp_channel:cast(ChannelOut,
					  #'basic.publish'{exchange = OutExchange2},
					  #amqp_msg{payload = Msg2}),
	
	%% Listen for the update
	Rec2 = receive
			{#'basic.deliver'{}, #amqp_msg{payload = Body2}} ->
				Data2 = binary_to_list(Body2),
				case erlson:is_json_string(Data2) of
					%% New value from the source as a Json
					true ->
						Val2 = lib_json:get_field(Data2, "value"),
						Id2 = binary_to_list(
								lib_json:get_field(Data2, "stream_id")),
						{Val2, Id2};
					_ -> false
				end;
			_ -> false
		  end,
	
	%% Close and clean
	VPid ! quit,
	amqp_channel:close(ChannelIn),
	amqp_channel:close(ChannelOut),
	amqp_connection:close(Connection),

	[?_assertEqual(true, is_list(StreamId1)),
	 ?_assertEqual(true, is_list(StreamId2)),
	 ?_assertEqual({64, "testID"}, Rec2)].








%% ====================================================================
%% Helper functions
%% ====================================================================


%% @doc
%% Function: create_a_stream_on_index()/1
%% Purpose: Creates a stream in the specified ES index.
%% Args: Index - The index into which to add the stream.
%% Returns: StreamId | {error, Reason}.
%% Side effects: Creates a stream on the index 'Index' in ES.
%% 
%% @end
-spec create_a_stream_on_index(Index :: string()) ->
		  StreamId :: string() | {error, Reason :: term()}.
create_a_stream_on_index(Index) ->
	case erlastic_search:index_doc(Index, "stream", {}) of
		{error, Reason} -> {error, Reason};
		{ok, Data} ->
			binary_to_list(element(2, lists:nth(4, element(2, Data))))
	end.





%% @doc
%% Function: create_virtual_stream/3
%% Purpose: Creates a virtual stream subscribing to the
%%          specified streams/virtual streams. 
%% Args: VStreamId - The Id of the virtual stream,
%%       List - A list specifying the type and id of the streams/virtual
%%              streams to subscribe to,
%%       Func - The function specifier.
%% Returns: Pid.
%% Side effects: Creates a virtual stream process that subscribes to the
%%               specified streams and publish to an exchange "vstreams.'Id'".
%% Example: > create_virtual_stream("1",
%%                                  [{"stream", "1"}, {"vstream", "4"}],
%%                                  "add").
%%          > <1.32.1>
%% 
%% @end
-spec create_virtual_stream(
		VStreamId :: string(),
		List :: [{Type :: string(), Id :: string()}],
		Func :: string()) -> Pid :: pid().
create_virtual_stream(VStreamId, List, Func) ->
	spawn(virtual_stream_process, create, [VStreamId, List, Func]).





%% @doc
%% Function: subscribe/2
%% Purpose: Subscribes to exchange 'Exchange' on the channel 'Channel'
%% Args: Channel - The channel on which communication with RabbitMQ occurs,
%%       Exchange - The exchange to subscribe to.
%% Returns: ok.
%% Side effects: Subscribes to the given exchange on the given channel.
%% 
%% @end
-spec subscribe(Channel :: pid(), Exchange :: binary()) -> ok.
subscribe(Channel, Exchange) ->
	amqp_channel:call(Channel,
					  #'exchange.declare'{exchange = Exchange,
										  type = <<"fanout">>}),
	#'queue.declare_ok'{queue = QueueIn} =
						   amqp_channel:call(
							 Channel,
							 #'queue.declare'{exclusive = true}),
	amqp_channel:call(Channel, #'queue.bind'{exchange = Exchange,
											 queue = QueueIn}),
	amqp_channel:subscribe(Channel,
						   #'basic.consume'{queue = QueueIn, no_ack = true},
						   self()),
	receive
		#'basic.consume_ok'{} -> ok
	end.




%% @doc
%% Function: create_data_point/2
%% Purpose: Creates a data point with the value 'Value' for the stream 'Stream'.
%% Args: StreamId - The stream id of whom the data point should belong,
%%       Value - The value of the data point.
%% Returns: DataPoint as binary
%% 
%% @end
-spec create_data_point(StreamId :: string(), Value :: number()) ->
		  DataPoint :: binary().
create_data_point(StreamId, Value) ->
	Timestamp = ?TIME_NOW(erlang:localtime()),
	Msg = ?TUPLE(?QUOTE("stream_id") ++ ?COLON ++ ?QUOTE(StreamId) ++ "," ++
					 ?QUOTE("timestamp") ++ ?COLON ++ ?QUOTE(Timestamp) ++
					 "," ++
					 ?QUOTE("value") ++ ?COLON ++ number_to_list(Value)),
	list_to_binary(Msg).



%% @doc
%% Function: number_to_list/1
%% Purpose: Converts a number to a list.
%% Args: Value - The number to convert.
%% Returns: The number as a list.
%% 
%% @end
-spec number_to_list(Value :: number()) -> list().
number_to_list(Value) when is_integer(Value) ->
	integer_to_list(Value);
number_to_list(Value) when is_float(Value) ->
	float_to_list(Value).


