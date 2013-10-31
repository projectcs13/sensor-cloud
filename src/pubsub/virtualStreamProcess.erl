-module(virtualStreamProcess).

-include_lib("amqp_client.hrl").
-include_lib("pubsub.hrl").

-export([create/4]).

create(VStreamId, InputType, InputId, Function) ->
	io:format("VStream: f = ~p~n",[Function]),
	%% Exchange name binarys
	InputExchange = list_to_binary(InputType++"s."++InputId),
	VStreamExchange = list_to_binary("vstreams."++VStreamId),

	%% Connect
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{host = "localhost"}),
	
	%% Open In and OUT channels
	{ok, ChannelIn} = amqp_connection:open_channel(Connection),
	{ok, ChannelOut} = amqp_connection:open_channel(Connection),

	%% Declare INPUT queue
	amqp_channel:call(ChannelIn, #'exchange.declare'{exchange = InputExchange, type = <<"fanout">>}),
	#'queue.declare_ok'{queue = QueueIn} = amqp_channel:call(ChannelIn, #'queue.declare'{exclusive = true}),
	amqp_channel:call(ChannelIn, #'queue.bind'{exchange = InputExchange, queue = QueueIn}),

	%% Subscribe to INPUT queue
	amqp_channel:subscribe(ChannelIn, #'basic.consume'{queue = QueueIn, no_ack = true}, self()),
	receive
		#'basic.consume_ok'{} -> ok
	end,

	%% Declare OUTPUT exchange
	io:format("Listening to ~p~n", [binary_to_list(InputExchange)]),
	amqp_channel:call(ChannelOut, #'exchange.declare'{exchange = VStreamExchange, type = <<"fanout">>}),
	
	loop(VStreamId, ChannelIn, {ChannelOut, VStreamExchange}, Function).

loop(VStreamId, ChannelIn, {ChannelOut, VStreamExchange}, Function) ->
	%% Receive from the subscribeTopic!
	receive
		{#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
			case binary_to_term(Body) of
				%% Get request
				{get, GetVar} ->
					io:format("GET: ~p~n", [GetVar]);

				%% Post request
				{post, JSON} ->
					io:format("POST: ~p~n", [JSON]);
					%% Parse JSON

					%% Store value

					%% Propagete
%					send(ChannelOut, VStreamExchange, Body),

				%% Delete request
				{delete} ->
					io:format("DELETE~n");

				%% New value from the source
				#'datapoint'{id = Id, timestamp = TimeStamp, value = Value} ->
					%% Store value

					%% Apply function
					{Val, _} = string:to_integer(Value),
					Data = Function(Val),

					%% Create Message
					Msg = term_to_binary(#'datapoint'{
							id = VStreamId,
							timestamp = TimeStamp,
							value = integer_to_list(Data)}),

					%% Propagete
					send(ChannelOut, VStreamExchange, Msg),
					io:format("DATAPOINT: {\"timestamp\" : ~p, \"value\" : ~p} -> ~p~n", [TimeStamp, Data, VStreamExchange]);
				_ ->
					io:format("CRAP! We are getting CRAP!~n")
			end,
			%% Recurse
			loop(VStreamId, ChannelIn, {ChannelOut, VStreamExchange}, Function)
	end.

send(Channel, Exchange, Message) ->
	amqp_channel:cast(Channel,
					  #'basic.publish'{exchange = Exchange},
					  #amqp_msg{payload = Message}).

%% HOW TO SEND TO A STREAMPROCESS
%			StreamId = "1",
%			ResourceId = "1",
%			Exchange = list_to_binary("resources."++ResourceId),
%
%			{ok, Connection} = amqp_connection:start(#amqp_params_network{host = "localhost"}),
%			{ok, Channel} = amqp_connection:open_channel(Connection),
%
%			amqp_channel:call(Channel,
%					#'exchange.declare'{exchange = Exchange,
%							type = <<"fanout">>}),
%
%			PID = spawn(streamProcess, create, [StreamId, ResourceId]),
%			io:format("Node Created: ~p~n", [PID]),
%
%			%% Needed for the RabbitMQ to have time to set up the system.
%			timer:sleep(1000),
%
%			amqp_channel:cast(Channel,
%					  #'basic.publish'{exchange = Exchange},
%					  #amqp_msg{payload = term_to_binary({post, DatapointJson})}),
%
%			ok = amqp_channel:close(Channel),
%			ok = amqp_connection:close(Connection),
