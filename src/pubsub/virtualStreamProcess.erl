%% @author Anders Steinrud
%% @author Gabriel Tholsgård
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == virtualStreamProcess ==
%% This module represents a virtual stream process which subscribes to data
%% points from the pub/sub system for a specified stream/virtual stream and
%% publish it back into the pub/sub system for the specified virtual stream. 
%%
%% @end
-module(virtualStreamProcess).

-include_lib("amqp_client.hrl").
-include_lib("pubsub.hrl").

-export([create/4]).


%% ====================================================================
%% API functions
%% ====================================================================


%% @doc
%% Function: create/4
%% Purpose: Used to initialize the virtual stream process which subscribes to
%%          the exchange beloning to the specified stream/virtual stream and
%%          publish it into the exchange for the specified virtual stream.
%% Args: VStreamId - The id of the publishing virtual stream,
%%       InputType - Specifer for subscription to a stream or virtual stream,
%%                   could either be "stream" or "vstream",
%%       InputId - The id of the stream/virtual stream to subscribe from,
%%       Function - Holder of function that will be applied to the incoming
%%                  data points before publishing.
%% Returns: Return ok.
%% Side effects: Non terminating loop receiving datapoints from the pub/sub
%%               system for a stream/virtual stream with id 'InputId' and
%%               sending data into the pub/sub system for a virtual stream
%%               with id 'VStreamId'.
%%
%% @end
-spec create(string(), string()) -> ok.
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



%% ====================================================================
%% Internal functions
%% ====================================================================



%% @doc
%% Function: loop/4
%% Purpose: Used to receive data points from the pub/sub system from a specified
%%          exchange and publish the result of the function with the data point
%%          as argument it into the pub/sub system for the exchange of the
%%          specified virtual stream.
%% Args: VStreamId - The id of a virtual stream,
%%       ChannelIn - The channel on which we are subscribing to data points,
%%       PubInfo - A tuple holding the channel for publishing and which exchange
%%                 to publish to,
%%       Function - The function to apply to the received data point.
%% Returns: Return ok.
%% Side effects: Non terminating loop receiving data points from the pub/sub
%%               system, applying the function 'Function' to it and publishing
%%               the result to the exchange specified in PubInfo.
%%
%% @end
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
				%% send(ChannelOut, VStreamExchange, Body),
				
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