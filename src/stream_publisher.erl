%% @author Gabriel Tholsgård
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == streams ==
%% This module will contain all functions needed to handle 
%% posting data to the pub/sub system for a specific topic (stream)
%%
%% @end

-module(stream_publisher).

-include_lib("amqp_client.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/1]).



main(Argv) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"topic_logs">>,
                                                   type = <<"topic">>}),

    {RoutingKey, Message} = case Argv of
                                [] ->
                                    {<<"anonymous.info">>, <<"Hello World!">>};
                                [R] ->
                                    {list_to_binary(R), <<"Hello World!">>};
                                [R | Msg] ->
                                    {list_to_binary(R), list_to_binary(string:join(Msg, " "))}
                            end,
    amqp_channel:cast(Channel,
                      #'basic.publish'{
                        exchange = <<"topic_logs">>,
                        routing_key = RoutingKey},
                      #amqp_msg{payload = Message}),
    io:format(" [x] Sent ~p:~p~n", [RoutingKey, Message]),
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),
    ok.


%% ====================================================================
%% Internal functions
%% ====================================================================





