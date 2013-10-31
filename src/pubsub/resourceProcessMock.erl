%%#!/usr/bin/env escript
%%! -pz ./amqp_client ./rabbit_common ./amqp_client/ebin ./rabbit_common/ebin
-module(resourceProcessMock).

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
    loop(ResourceId, Channel, ResourceExchange).

loop(ResourceId, Channel, Exchange) ->
    {S1,S2,S3} = erlang:now(),
    %% Seed random generator
    random:seed(S1,S2,S3),
    %% Random a value
    Data = random:uniform(5),
    %% get Timestamp
    {Year, Month, Day, Hour, Min, Sec} = create_uniform_time(erlang:localtime()),
	Date = string:join([Year, Month, Day], "-") ++ " " ++ string:join([Hour, Min, Sec], ":"),
	
    %% Create Message
    %Msg = term_to_binary(#'datapoint'{timestamp = Date,
    %                                  value = integer_to_list(Data),
    %									  streamid = ResourceId}),
    Msg = list_to_binary("{\"id\" : \""++ResourceId++"\", \"timestamp\" : \""++Date++"\", \"value\" : \""++Data++"\"}")

    %% Send Msg to exchange
    io:format("~p -> ~p~n", [binary_to_term(Msg) ,binary_to_list(Exchange)]),
    amqp_channel:cast(Channel, #'basic.publish'{exchange = Exchange}, #amqp_msg{payload = Msg}),

    %% Sleep 1.0s
    timer:sleep(1000),

    %% Recurse
    loop(ResourceId, Channel, Exchange).



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
