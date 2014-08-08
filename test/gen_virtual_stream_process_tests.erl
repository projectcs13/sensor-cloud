%% @author Gabriel Tholsgård
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%%
%% @doc == virtual_stream_process_tests ==
%% This module contains several tests to test the functionallity
%% of the pub/sub system, including streams and virtual streams.
%%
%% @end

-module(gen_virtual_stream_process_tests).

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


%% virtual_stream_process_test_/0
%% ====================================================================
%% @doc
%% Function: virtual_stream_process_test_/0
%% Purpose: Runs all the tests with extended timeouts
%% @end
%% ====================================================================
virtual_stream_process_test_() ->
	Children = supervisor:which_children(vstream_sup),
	[(element(2, X)) ! quit || X <- Children],
	timer:sleep(500),
	S = fun() ->
			StreamId1 = create_a_stream_on_index(?ES_INDEX),
			StreamId2 = create_a_stream_on_index(?ES_INDEX),
			VStreamId = create_a_virtual_stream_on_index(?ES_INDEX, [StreamId1, StreamId2]),
			{VStreamId, StreamId1, StreamId2}
		end,
	C = fun({VStreamId, StreamId1, StreamId2}) ->
			remove_stream_on_index(?ES_INDEX, StreamId1),
			remove_stream_on_index(?ES_INDEX, StreamId2),
			remove_virtual_stream_on_index(?ES_INDEX, VStreamId)
		end,
	[
	 {timeout, 30, [{setup, S, C, fun start_supervisor/1}]},
	 {timeout, 30, [{setup, S, C, fun start_and_terminate/1}]},
	 {timeout, 30, [{setup, S, C, fun vstream_subscribe_to_a_stream/1}]},
	 {timeout, 30, [{setup, S, C, fun vstream_subscribe_to_streams_interval/1}]},
	 {timeout, 30, [{setup, S, C, fun clean_up/1}]}
	].




start_supervisor(_) ->
	case whereis(vstream_sup) of
		undefined ->
			{ok, Pid} = virtual_stream_process_supervisor:start_link(),
			ok;
		Pid -> ok
	end,
	[?_assertEqual(true, is_process_alive(Pid))].



clean_up(_) ->
	timer:sleep(1000),
	remove_any_vsdatapoints(?ES_INDEX),
	Children = length(supervisor:which_children(vstream_sup)),
	[?_assertEqual(0, Children)].




%% @doc
%% Function: start_and_terminate/1
%% Purpose: Starts a virtual stream process and terminates it.
%% Returns: ok | {error, term()}
%%
%% @end
-spec start_and_terminate(_) -> ok | {error, term()}.
start_and_terminate({VStreamId, StreamId1, _StreamId2}) ->
	SupPid = whereis(vstream_sup),
	Childrens1 = length(supervisor:which_children(vstream_sup)),
	
	virtual_stream_process_supervisor:add_child(VStreamId, [{stream, StreamId1}], [list_to_binary("total"), "2s"]),

	Childrens2 = length(supervisor:which_children(vstream_sup)),
	Pid = element(2, lists:nth(1, supervisor:which_children(vstream_sup))),
	Info1 = is_process_alive(Pid),
	
	virtual_stream_process_supervisor:terminate_child(VStreamId),
	timer:sleep(1500),

	Info2 = is_process_alive(Pid),
	[?_assertEqual(true, is_pid(SupPid)),
	 ?_assertEqual(0, Childrens1),
	 ?_assertEqual(1, Childrens2),
	 ?_assertEqual(true, Info1),
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
vstream_subscribe_to_a_stream({VStreamId, StreamId1, _StreamId2}) ->
	
	%% Create virtual stream listening to our stream
	virtual_stream_process_supervisor:add_child(VStreamId, [{stream, StreamId1}], [list_to_binary("diff"), "2s"]),
	
	%% Create a publisher and consumer exchanges
	OutExchange = list_to_binary("streams." ++ StreamId1),
	InExchange = list_to_binary("vstreams." ++ VStreamId),
	
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
	Msg1 = create_data_point(StreamId1, 32),
	amqp_channel:cast(ChannelOut,
					  #'basic.publish'{exchange = OutExchange},
					  #amqp_msg{payload = Msg1}),	
	
	
	%% Listen for the update
	Rec1 = receive
			{#'basic.deliver'{}, #amqp_msg{payload = Body1}} ->
				Data1 = binary_to_list(Body1),
				case erlson:is_json_string(Data1) of
					%% New value from the source as a Json
					true ->
						Value1 = lib_json:get_field(Data1, "value"),
						Id1 = binary_to_list(
								lib_json:get_field(Data1, "stream_id")),
						{Value1, Id1};
					_ -> false
				end;
			_ -> false
		  end,


	%% Publish a message
	Msg2 = create_data_point(StreamId1, 30),
	amqp_channel:cast(ChannelOut,
					  #'basic.publish'{exchange = OutExchange},
					  #amqp_msg{payload = Msg2}),	
	
	
	%% Listen for the update
	Rec2 = receive
			{#'basic.deliver'{}, #amqp_msg{payload = Body2}} ->
				Data2 = binary_to_list(Body2),
				case erlson:is_json_string(Data2) of
					%% New value from the source as a Json
					true ->
						Value2 = lib_json:get_field(Data2, "value"),
						Id2 = binary_to_list(
								lib_json:get_field(Data2, "stream_id")),
						{Value2, Id2};
					_ -> false
				end;
			_ -> false
		  end,


	%% Publish a message
	Msg3 = create_data_point(StreamId1, 32),
	amqp_channel:cast(ChannelOut,
					  #'basic.publish'{exchange = OutExchange},
					  #amqp_msg{payload = Msg3}),	
	
	
	%% Listen for the update
	Rec3 = receive
			{#'basic.deliver'{}, #amqp_msg{payload = Body3}} ->
				Data3 = binary_to_list(Body3),
				case erlson:is_json_string(Data3) of
					%% New value from the source as a Json
					true ->
						Value3 = lib_json:get_field(Data3, "value"),
						Id3 = binary_to_list(
								lib_json:get_field(Data3, "stream_id")),
						{Value3, Id3};
					_ -> false
				end;
			_ -> false
		  end,
	
	%% Close and clean
	amqp_channel:close(ChannelIn),
	amqp_channel:close(ChannelOut),
	amqp_connection:close(Connection),
	virtual_stream_process_supervisor:terminate_child(VStreamId),
	
	timer:sleep(1000),

	[?_assertEqual(true, is_list(StreamId1)),
	 ?_assertEqual({0.0, VStreamId}, Rec1),
	 ?_assertEqual({-2, VStreamId}, Rec2),
	 ?_assertEqual({2, VStreamId}, Rec3)].
	







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
vstream_subscribe_to_streams_interval({VStreamId, StreamId1, StreamId2}) ->

	%% Create virtual stream listening to our streams
	virtual_stream_process_supervisor:add_child(VStreamId, [{stream, StreamId1}, {stream, StreamId2}], [list_to_binary("total"), "2s"]),
	
	%% Create publisher and consumer exchanges
	OutExchange1 = list_to_binary("streams." ++ StreamId1),
	OutExchange2 = list_to_binary("streams." ++ StreamId2),
	InExchange = list_to_binary("vstreams." ++ VStreamId),
	
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
	amqp_channel:close(ChannelIn),
	amqp_channel:close(ChannelOut),
	amqp_connection:close(Connection),
	virtual_stream_process_supervisor:terminate_child(VStreamId),

	[?_assertEqual(true, is_list(StreamId1)),
	 ?_assertEqual(true, is_list(StreamId2)),
	 ?_assertEqual({32, VStreamId}, Rec1),
	 ?_assertEqual({64, VStreamId}, Rec2)].








%% ====================================================================
%% Helper functions
%% ====================================================================


%% @doc
%% Function: create_a_stream_on_index/1
%% Purpose: Creates a stream in the specified ES index.
%% Args: Index - The index into which to add the stream.
%% Returns: StreamId | {error, Reason}.
%% Side effects: Creates a stream on the index 'Index' in ES.
%% 
%% @end
-spec create_a_stream_on_index(Index :: string()) ->
		  StreamId :: string() | {error, Reason :: term()}.
create_a_stream_on_index(Index) ->
	case erlastic_search:index_doc(Index, "stream", "{}") of
		{error, Reason} -> {error, Reason};
		{ok, Data} ->
			binary_to_list(lib_json:get_field(Data, "_id"))
	end.



remove_stream_on_index(Index, StreamId) ->
	erlastic_search:delete_doc(Index, "stream", StreamId).



%% @doc
%% Function: create_a_virtual_stream_on_index/2
%% Purpose: Creates a virtual stream in the specified ES index.
%% Args: Index - The index into which to add the stream,
%%       Id - A list of stream ids' to subscribe to.
%% Returns: VStreamId | {error, Reason}.
%% Side effects: Creates a virtual stream on the index 'Index' in ES.
%% 
%% @end
-spec create_a_virtual_stream_on_index(Index :: string(), [Id :: string()]) ->
		  VStreamId :: string() | {error, Reason :: term()}.
create_a_virtual_stream_on_index(_Index, []) -> {error, "Not a valid VStream"};
create_a_virtual_stream_on_index(Index, List) ->
	Involved = "[" ++ ["\"" ++ X ++ "\"," || X <- lists:sublist(List, 1, length(List)-1)] ++ "\"" ++ lists:last(List) ++ "\"]",
	case erlastic_search:index_doc(Index, "virtual_stream", "{\"name\":\"test\", \"streams_involved\":" ++ Involved ++ ", \"function\":[\"total\", \"2s\"]}") of
		{error, Reason} -> {error, Reason};
		{ok, Data} ->
			binary_to_list(lib_json:get_field(Data, "_id"))
	end.


remove_virtual_stream_on_index(Index, VStreamId) ->
	erlastic_search:delete_doc(Index, "virtual_stream", VStreamId).





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






remove_any_vsdatapoints(Index) ->
	case erlastic_search:search(Index, "vsdatapoint", "*") of
		{error, {Code, Body}} ->
			ErrorString = api_help:generate_error(Body, Code),
			{error, ErrorString};
		{ok, JsonStruct} ->
		    List = lib_json:get_field(JsonStruct, "hits.hits"),
		    [erlastic_search:delete_doc(Index, "vsdatapoint", binary_to_list(lib_json:get_field(X, "_id"))) || X <- List]
	end.
		    

