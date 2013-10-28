%% @author Gabriel Tholsgård
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == streams ==
%% This module will contain all functions needed to handle 
%% receiving data from the pub/sub system for a specific topic (stream)
%%
%% @end

-module(stream_reciever).

-include_lib("amqp_client.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([main/1]).


main(Argv) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost", port = 5672}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"topic_logs">>,
                                                   type = <<"fanout">>}),

    #'queue.declare_ok'{queue = Queue} =
        amqp_channel:call(Channel, #'queue.declare'{exclusive = true}),

    [amqp_channel:call(Channel, #'queue.bind'{exchange = <<"topic_logs">>,
                                              routing_key = list_to_binary(BindingKey),
                                              queue = Queue})
     || BindingKey <- Argv],

    io:format(" [*] Waiting for logs. To exit press CTRL+C~n"),

    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,
    loop(Channel).

loop(Channel) ->
    receive
        {#'basic.deliver'{routing_key = RoutingKey}, #amqp_msg{payload = Body}} ->
            io:format(" [x] ~p:~p~n", [RoutingKey, Body]),
            loop(Channel)
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================


