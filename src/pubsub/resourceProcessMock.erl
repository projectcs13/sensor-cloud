%% @author Anders Steinrud
%% @author Gabriel Tholsgård
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == resourceProcessMock ==
%% This module simulates a single resource providing data points every second.
%%
%% @end
-module(resourceProcessMock).

-include_lib("amqp_client.hrl").
-include_lib("pubsub.hrl").

-export([create/1]).


%% ====================================================================
%% API functions
%% ====================================================================

%% @doc
%% Function: create/1
%% Purpose: Used to initialize the simulator process.
%% Args: ResourceId - The id of a resource.
%% Returns: Return ok.
%% Side effects: Non terminating loop sending data into the pub/sub system
%%               for a resource with id 'ResourceId'.
%% @end
-spec create(string()) -> ok.
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


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Function: loop/3
%% Purpose: Used to publish auto generated data points into the pub/sub system
%%          for a specified resource.
%% Args: ResourceId - The id of a resource,
%%       Channel - The channel on which we are publishing data,
%%       Exchange - The exchange we are pushing data to
%% Returns: Return ok.
%% Side effects: Non terminating loop sending auto generated data points into
%%               the pub/sub system for a resource with id 'ResourceId'.
%% @end
-spec loop(string(), pid(), string()) -> ok.
loop(ResourceId, Channel, Exchange) ->
    {S1,S2,S3} = erlang:now(),
    %% Seed random generator
    random:seed(S1,S2,S3),
    %% Random a value
    Data = random:uniform(5),
    %% get Timestamp
    Date = uniform_time(erlang:localtime()),
    %% Create Message
    %Msg = term_to_binary(#'datapoint'{timestamp = Date,
    % value = integer_to_list(Data),
    %                                                                         streamid = ResourceId}),
    Msg = list_to_binary("{\"id\" : \""++ResourceId++"\", \"timestamp\" : \""++Date++"\", \"value\" : "++Data++"}"),

    %% Send Msg to exchange
    io:format("~p -> ~p~n", [binary_to_term(Msg) ,binary_to_list(Exchange)]),
    amqp_channel:cast(Channel, #'basic.publish'{exchange = Exchange}, #amqp_msg{payload = Msg}),

    %% Sleep 1.0s
    timer:sleep(1000),

    %% Recurse
    loop(ResourceId, Channel, Exchange).


%% @doc
%% Function: uniform_time/1
%% Purpose: Transform a timestamp to have the form YYYY:MM:DD HH:MM:SS
%% Returns: String of the timestamp in the form "YYYY:MM:DD HH:MM:SS"
%% @end
-spec uniform_time(calendar:datetime()) -> string().
uniform_time({{Year, Month, Day},{Hour, Min, Sec}}) ->
        string:join([integer_to_list(Year),
                                 check_format(Month),
                                 check_format(Day)], ":") ++
                " " ++
                string:join([check_format(Hour),
                                         check_format(Min),
                                         check_format(Sec)], ":").



%% @doc
%% Function: check_format/1
%% Purpose: Transform a integer X < 10 to be a string "0X" otherwise "X"
%% Returns: If X < 10 then "0X" else "X"
%% @end
-spec check_format(integer()) -> string().
check_format(Integer) when is_integer(Integer) ->
        case Integer < 10 of
                true -> "0" ++ integer_to_list(Integer);
                _ -> "" ++ integer_to_list(Integer)
        end.