-module(streamProcess).

-include_lib("../../lib/rabbitmq-erlang-client/include/amqp_client.hrl").
-include_lib("../../include/pubsub.hrl").

-export([create/2]).

create(StreamId, ResourceId) ->

    %% Connect
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    %% Open a channel?
    {ok, Channel} = amqp_connection:open_channel(Connection),

    %% Declare global Exchange? 
    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"exchange">>,
                                                   type = <<"topic">>}),
    %% Declare INPUT queue, stored in the ResourceId
    #'queue.declare_ok'{queue = Queue} =
        amqp_channel:call(Channel, #'queue.declare'{exclusive = true}),

    amqp_channel:call(Channel, #'queue.bind'{exchange = <<"exchange">>,
                                              routing_key = list_to_binary("resources."++ResourceId),
                                              queue = Queue}),

    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,

    {ok, ChannelOut} = amqp_connection:open_channel(Connection),

    amqp_channel:call(ChannelOut, #'exchange.declare'{exchange = <<"exchange">>,
                                                      type = <<"topic">>}),
    loop(Channel, ChannelOut, list_to_binary("streams."++StreamId)).

loop(Channel, ChannelOut, RoutingKeyOut) ->
    %% Receive from the subscribeTopic!
    receive
        {#'basic.deliver'{routing_key = RoutingKey}, #amqp_msg{payload = Body}} ->
            io:format(" [x] ~p:~p~n", [RoutingKey, Body]),
            case binary_to_term(Body) of
                %% Get request
                {get, GetVar} ->
                    io:format("GET: ~p~n", [GetVar]);

                %% Put request
                {put, JSON} ->
                    io:format("PUT: ~p~n", [JSON]);
                    %% Store value

                    %% Propagete
                    %send(ChannelOut, RoutingKeyOut, Body),

                %% Delete request
                {delete} ->
                    io:format("DELETE~n");

                %% New value from the source
                #'datapoint'{timestamp = TimeStamp, value = Value} ->
                    %% Store value

                    %% Propagete
                    send(ChannelOut, RoutingKeyOut, Body)
            end,
            %% Recurse
            loop(Channel, ChannelOut, RoutingKeyOut)
    end.

send(Channel, RoutingKey, Message) ->
    amqp_channel:cast(Channel,
                      #'basic.publish'{exchange = <<"exchange">>,
                                       routing_key = RoutingKey},
                                       #amqp_msg{payload = Message}).
