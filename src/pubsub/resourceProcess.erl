%%#!/usr/bin/env escript
%%! -pz ./amqp_client ./rabbit_common ./amqp_client/ebin ./rabbit_common/ebin
-module(resourceProcess).

-include_lib("amqp_client.hrl").
-include_lib("pubsub.hrl").

-export([create/1]).

create(ResourceId) ->
    %% Exchange namn binarys
    ResourceExchange = list_to_binary("resources."++ResourceId),

    %% Connect to RabbitMQ server
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),

    %% Open channel
    {ok, Channel} = amqp_connection:open_channel(Connection),

    %% Create/Declare exchange
    amqp_channel:call(Channel, #'exchange.declare'{exchange = ResourceExchange, type = <<"fanout">>}),
    
    %% Start Loop
    loop(Channel, ResourceExchange).

loop(Channel, Exchange) ->
    %% Get Value from source

    %% Parse Values from Value


    %% get Timestamp
    %{{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
    %% Create Message
    %Msg = term_to_binary(#'datapoint'{timestamp = string:join([integer_to_list(Year),
    %                                                           integer_to_list(Month),
    %                                                           integer_to_list(Day)], "-")++
    %                                              " "++
    %                                              string:join([integer_to_list(Hour),
    %                                                           integer_to_list(Min),
    %                                                           integer_to_list(Sec)], ":"),
    %                                  value = Data}),

    %% Send Msg to exchange
    %io:format("~p -> ~p~n", [binary_to_term(Msg) ,binary_to_list(Exchange)]),
    %amqp_channel:cast(Channel, #'basic.publish'{exchange = Exchange}, #amqp_msg{payload = Msg}),

    %% Sleep a polling freq.
    timer:sleep(5*60*60*1000),

    %% Recurse
    loop(Channel, Exchange).