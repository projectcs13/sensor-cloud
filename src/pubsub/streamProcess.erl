%% @author Anders Steinrud
%% @author Gabriel Tholsgård
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == streamProcess ==
%% This module represents a stream process which subscribes to data points from
%% the pub/sub system for a specified resource and publish it back into the
%% pub/sub system for the specified stream 
%%
%% @end

-module(streamProcess).

-include_lib("amqp_client.hrl").
-include_lib("pubsub.hrl").

-export([create/2]).

%% ====================================================================
%% API functions
%% ====================================================================


%% @doc
%% Function: create/2
%% Purpose: Used to initialize the stream process which subscribes to the
%%          exchange beloning to the specified resource and publish it into
%%          the exchange for the specified stream.
%% Args: StreamId - The id of a stream.
%%       ResourceId - The id of a resource.
%% Returns: Return ok.
%% Side effects: Non terminating loop receiving datapoints from the pub/sub
%%               system for a resource with id 'ResourceId' and sending data
%%               into the pub/sub system for a stream with id 'StreamId'.
%%
%% @end
-spec create(string(), string()) -> ok.
create(StreamId, ResourceId) ->
	%% Exchange name binarys
	ResourceExchange = list_to_binary("resources."++ResourceId),
	StreamExchange = list_to_binary("streams."++StreamId),
	
	%% Connect
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{host = "localhost"}),
	
	%% Open In and OUT channels
	{ok, ChannelIn} = amqp_connection:open_channel(Connection),
	{ok, ChannelOut} = amqp_connection:open_channel(Connection),
	
	%% Declare INPUT exchange and queue
	amqp_channel:call(ChannelIn, #'exchange.declare'{exchange = ResourceExchange, type = <<"fanout">>}),
	#'queue.declare_ok'{queue = QueueIn} = amqp_channel:call(ChannelIn, #'queue.declare'{exclusive = true}),
	amqp_channel:call(ChannelIn, #'queue.bind'{exchange = ResourceExchange, queue = QueueIn}),
	
	%% Subscribe to INPUT queue
	amqp_channel:subscribe(ChannelIn, #'basic.consume'{queue = QueueIn, no_ack = true}, self()),
	receive
		#'basic.consume_ok'{} -> ok
	end,
	
	%% Declare OUTPUT exchange
	io:format("Listening to ~p~n", [binary_to_list(ResourceExchange)]),
	amqp_channel:call(ChannelOut, #'exchange.declare'{exchange = StreamExchange, type = <<"fanout">>}),
	
	loop(StreamId, ChannelIn, {ChannelOut, StreamExchange}).


%% ====================================================================
%% Internal functions
%% ====================================================================



%% @doc
%% Function: loop/3
%% Purpose: Used to receive data points from the pub/sub system from a specified
%%          exchange and publish it into the pub/sub system for the exchange
%%          of the specified stream.
%% Args: StreamId - The id of a stream,
%%       ChannelIn - The channel on which we are subscribing to data points,
%%       PubInfo - A tuple holding the channel for publishing and which exchange
%%                 to publish to.
%% Returns: Return ok.
%% Side effects: Non terminating loop receiving data points from the pub/sub
%%               system and publishing it to the exchange specified in PubInfo.
%%
%% @end
-spec loop(string(), pid(), tuple()) -> ok.
loop(StreamId, ChannelIn, {ChannelOut, StreamExchange}) ->
	%% Receive from the subscribeTopic!
	receive
		{#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
			
			%% Propagete
			send(ChannelOut, StreamExchange, Body),
			%io:format("DATAPOINT: {\"timestamp\" : ~p, \"value\" : ~p} -> ~p~n", [TimeStamp, Value, StreamExchange]);
			
			%% Recurse
			loop(StreamId, ChannelIn, {ChannelOut, StreamExchange})
	end.


%% @doc
%% Function: send/3
%% Purpose: Used to publish a message into specified exchange in the pub/sub
%%          system on the specified channel.
%% Args: Channel - The channel on which we publish,
%%       Exchange - The exchange we publish to,
%%       Message - The message that we want to publish.
%% Returns: Return ok.
%% Side effects: Publish the message 'Message' into the exchange 'Exchange'.
%%
%% @end
-spec send(pid(), string(), binary()) -> ok.
send(Channel, Exchange, Message) ->
	amqp_channel:cast(Channel,
					  #'basic.publish'{exchange = Exchange},
					  #amqp_msg{payload = Message}).

%% HOW TO SEND TO A STREAMPROCESS
%                        StreamId = "1",
%                        ResourceId = "1",
%                        Exchange = list_to_binary("resources."++ResourceId),
%
%                        {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "localhost"}),
%                        {ok, Channel} = amqp_connection:open_channel(Connection),
%
%                        amqp_channel:call(Channel,
%                                        #'exchange.declare'{exchange = Exchange,
%                                                        type = <<"fanout">>}),
%
%                        PID = spawn(streamProcess, create, [StreamId, ResourceId]),
%                        io:format("Node Created: ~p~n", [PID]),
%
%                        %% Needed for the RabbitMQ to have time to set up the system.
%                        timer:sleep(1000),
%
%                        amqp_channel:cast(Channel,
%                                         #'basic.publish'{exchange = Exchange},
%                                         #amqp_msg{payload = term_to_binary({post, DatapointJson})}),
%
%                        ok = amqp_channel:close(Channel),
%                        ok = amqp_connection:close(Connection),

