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
-export([main/2]).



main(Id, Val) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
	
	StreamId = Id,
	Exchange = list_to_binary("streams." ++ StreamId),
	Json =  "{\"stream_id\" : \"" ++ StreamId ++ "\", \"timestamp\" : \"2013-11-21T12:02:42.000\", \"value\" : " ++ integer_to_list(Val) ++ "}",
	Message = list_to_binary(Json),
	
    amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange,
                                                   type = <<"fanout">>}),
	
    amqp_channel:cast(Channel,
                      #'basic.publish'{exchange = Exchange},
                      #amqp_msg{payload = Message}),
    io:format(" [x] Sent to ~p:~p~n", ["streams." ++ StreamId, Message]),
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),
    ok.


%% ====================================================================
%% Internal functions
%% ====================================================================





