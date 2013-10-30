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
-include_lib("pubsub.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/1]).

-define(EXCHANGE, "resources.1").

main(Argv) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "130.238.15.206", port = 5672}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<?EXCHANGE>>,
                                                   type = <<"fanout">>}),
	
	{Year, Month, Day, Hour, Min, Sec} = create_uniform_time(erlang:localtime()),
	Date = string:join([Year, Month, Day], "-") ++ " " ++ string:join([Hour, Min, Sec], ":"),
	
    Message = case Argv of
				  [] ->
					  
					  term_to_binary(#datapoint{timestamp = Date,
												value = integer_to_list(random:uniform(100)),
												streamid = "034e32-42f3769"});
				  [Value] ->
					  term_to_binary(#datapoint{timestamp = Date,
												value = integer_to_list(Value),
												streamid = "034e32-42f376b"})
			  end,
    amqp_channel:cast(Channel,
                      #'basic.publish'{
                        exchange = <<?EXCHANGE>>,
                        routing_key = <<"">>},
                      #amqp_msg{payload = Message}),
    io:format(" [x] Sent ~p:~p~n", ["", Message]),
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),
    ok.


%% ====================================================================
%% Internal functions
%% ====================================================================


create_uniform_time({{Year, Month, Day}, {Hour, Min, Sec}}) ->
	{integer_to_list(Year), check_format(Month), check_format(Day),
	 check_format(Hour), check_format(Min), check_format(Sec)}.


check_format(Value) when is_integer(Value) ->
	case Value < 10 of
		true -> "0" ++ integer_to_list(Value);
		false -> integer_to_list(Value)
	end.
	
