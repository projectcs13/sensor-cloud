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
        amqp_connection:start(#amqp_params_network{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
	
	StreamId = lists:nth(1, Argv),
	Exchange = list_to_binary("vstreams." ++ StreamId),
	

    amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange,
                                                   type = <<"fanout">>}),

    #'queue.declare_ok'{queue = Queue} =
        amqp_channel:call(Channel, #'queue.declare'{exclusive = true}),

    amqp_channel:call(Channel, #'queue.bind'{exchange = Exchange, queue = Queue}),

    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,
	io:format("Waiting for messages from the exchange: ~p~n", [binary_to_list(Exchange)]),
    loop(Channel).

loop(Channel) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
            io:format(" [x] Message:~p~n", [Body]),
            loop(Channel)
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================


