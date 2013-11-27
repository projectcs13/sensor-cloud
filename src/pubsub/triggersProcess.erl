-module(triggersProcess).

-include_lib("amqp_client.hrl").
-include_lib("pubsub.hrl").

-export([create/1]).

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
-spec create(string()) -> ok.
create(StreamId) ->
	%% Exchange name binarys
	CommandExchange = list_to_binary("triggers."++StreamId),
	DataExchange = list_to_binary("streams."++StreamId),
	
	%% Connect
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{host = "localhost"}),
	
	%% Open 2 In channels
	{ok, ChannelInData} = amqp_connection:open_channel(Connection),
	{ok, ChannelInCommand} = amqp_connection:open_channel(Connection),
	
	%% Declare INPUT exchange and queue
	amqp_channel:call(ChannelInData, #'exchange.declare'{exchange = DataExchange, type = <<"fanout">>}),
	#'queue.declare_ok'{queue = QueueInData} = amqp_channel:call(ChannelInData, #'queue.declare'{exclusive = true}),
	amqp_channel:call(ChannelInData, #'queue.bind'{exchange = DataExchange, queue = QueueInData}),
	
	%% Subscribe to INPUT queue
	amqp_channel:subscribe(ChannelInData, #'basic.consume'{queue = QueueInData, no_ack = true}, self()),
	receive
		#'basic.consume_ok'{} -> ok
	end,
	
	%% Declare OUTPUT exchange
	io:format("Listening to ~p~n", [binary_to_list(DataExchange)]),
	
	%% Declare INPUT exchange and queue
	amqp_channel:call(ChannelInCommand, #'exchange.declare'{exchange = CommandExchange, type = <<"fanout">>}),
	#'queue.declare_ok'{queue = QueueInCommand} = amqp_channel:call(ChannelInCommand, #'queue.declare'{exclusive = true}),
	amqp_channel:call(ChannelInCommand, #'queue.bind'{exchange = CommandExchange, queue = QueueInCommand}),
	
	%% Subscribe to INPUT queue
	amqp_channel:subscribe(ChannelInCommand, #'basic.consume'{queue = QueueInCommand, no_ack = true}, self()),
	receive
		#'basic.consume_ok'{} -> ok
	end,
	
	%% Declare OUTPUT exchange
	io:format("Listening to ~p~n", [binary_to_list(CommandExchange)]),

	
	loop(StreamId, ChannelInData, ChannelInCommand,[]).


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
-spec loop(string(), pid(), pid(),list()) -> ok.
loop(StreamId, ChannelInData, ChannelInCommand,Triggers) ->
	%% Receive from the subscribeTopic!
	receive
		{#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
			Msg = binary_to_term(Body),
			case erlson:is_json_string(Msg) of
				true ->
					% Here it should run the trigger functions
					NewTriggers = Triggers,
					erlang:display("Data: " ++ Msg);
				false ->
					NewTriggers = handle_command(Msg,Triggers),
					erlang:display(NewTriggers)
			end,
			%% Propagete
			%send(ChannelOut, StreamExchange, Body),
			%io:format("DATAPOINT: {\"timestamp\" : ~p, \"value\" : ~p} -> ~p~n", [TimeStamp, Value, StreamExchange]);
			
			%% Recurse
			loop(StreamId, ChannelInData, ChannelInCommand,NewTriggers)
	end.

% Add command
handle_command({add,Username,Func},[]) ->
	[{Func,[Username]}];
handle_command({add,Username,Func},[{Func,Users}|Rest]) ->
	[{Func,[Username|Users]}|Rest];

% Remove command
handle_command({remove,Username,Func},[]) ->
	[];
handle_command({remove,Username,Func},[{Func,[Username]}|Rest]) ->
	Rest;
handle_command({remove,Username,Func},[{Func,Users}|Rest]) ->
	[{Func,lists:delete(Username, Users)}|Rest];

% Non matching 
handle_command(Command,[First|Rest]) ->
	[First|handle_command(Command,Rest)].




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
