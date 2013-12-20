%% @author Anders Steinrud, Gabriel Tholsg�rd, Tomas S�vstr�m <tosa7943@student.uu.se>, Andreas Moreg�rd Haubenwaller
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == triggersProcess ==
%% This module represents a trigger process that will
%% listen to some streams and then apply a trigger function
%% to the data when a new datapoint is added
%%
%% @end
-module(triggersProcess).

-include_lib("erlastic_search.hrl").
-include_lib("amqp_client.hrl").
-include_lib("pubsub.hrl").
-include_lib("common.hrl").
-include_lib("json.hrl").
-include_lib("debug.hrl").

-export([create/5, subscribe/2]).


-define(INDEX, "sensorcloud").

%% ====================================================================
%% API functions
%% ====================================================================


%% @doc
%% Function: create/4
%% Purpose: Used to start the triggersProcess by setting up all comunication
%%          channels needed by the loop function
%% Returns: ok 
%% @end
-spec create(TriggerId :: string(), InputIds :: [StreamInfo],
                         Function :: atom(), OutputList::list(),Type::string()) -> ok when
		  StreamInfo :: {Type, Id},
          Type :: atom(),
          Id :: string().
create(TriggerId, InputIds, Function, OutputList,Type) ->
		erlang:display("Starting trigger process for id:" ++ TriggerId ++ " and type " ++ Type),
        %% Exchange name binarys.
        InputExchanges = create_input_exchanges(InputIds),
		CommandExchange = list_to_binary("command.trigger." ++ TriggerId),
        TriggerExchange = list_to_binary("trigger." ++ TriggerId),
        
        %% Connect.
        {ok, Connection} =
                amqp_connection:start(#amqp_params_network{}),
        
        %% Open In and OUT channels.
        {ok, ChannelIn} = amqp_connection:open_channel(Connection),
        {ok, ChannelOut} = amqp_connection:open_channel(Connection),
        
        %% Declare INPUT queues and subscribe.
        subscribe(ChannelIn, [CommandExchange|InputExchanges]),
        
        %% Declare OUTPUT exchange.
        amqp_channel:call(ChannelOut,
                                         #'exchange.declare'{exchange = TriggerExchange,
                                                                                 type = <<"fanout">>}),

        %% Needed for the RabbitMQ to have time to set up the system.
        timer:sleep(1000),
        
        %% Get the latest value for each datapoint.
        DataPoints = get_latest_value_for_streams(InputIds),
        
        loop(TriggerId, DataPoints, TriggerExchange,
                 {Connection, ChannelIn, ChannelOut}, Function, OutputList,Type).



%% ====================================================================
%% Internal functions
%% ====================================================================



%% @doc
%% Function: loop/6
%% Purpose: Used to wait for new messages and then to handle them,
%%          if the message is a new datapoint then the trigger function 
%%          will be run, if the message is a command the the command will
%%          be performed, these are now {add,{Input,User}} or {remove,{Input,User}}
%%          all other things will be ignored.
%% Returns: ok 
%% @end
-spec loop(TriggerId :: string(),
                 DataPoints :: [{Id :: string(), DataPoint :: json() | undefined}],
                 TriggerExchange :: binary(),
                 Net :: {Connection :: pid(), ChannelIn :: pid(), ChannelOut :: pid()},
                 Function :: string(),
                 OutputList :: list(),
		   Type::string()) -> ok.
loop(TriggerId, DataPoints, TriggerExchange, Net, Function, OutputList,Type) ->
        receive
                {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
                        Data = binary_to_list(Body),
						erlang:display(Body),
                        case erlson:is_json_string(Data) of
							%% New value from the source as a Json
							true ->
								Id = binary_to_list(lib_json:get_field(Data, "stream_id")),
								NewDataPoints =
									lists:keyreplace(Id, 1, DataPoints, {Id, Data}),
								erlang:display(NewDataPoints),
								erlang:display("Outputlist"),
								erlang:display(OutputList),
								%% Apply function to the new values
								TriggerList = apply_function(Function, NewDataPoints, OutputList),
								%% Create timestamp to avoid issues with asynchronus
								%% datapoints.
								Timestamp = ?TIME_NOW(erlang:localtime()),
								
								Messages = create_messages(TriggerList, Timestamp,[],Type),
								erlang:display("TriggerList for " ++ TriggerId ++ " and type " ++ Type),
								erlang:display(TriggerList),
								ChannelOut = element(3, Net),
                                %% Send messages to live update
								send_messages(TriggerExchange,ChannelOut,Messages),
                                %% Send to standard output
                                %% TODO: Parallelize, spawn one process for each Message?
                                send_to_output(TriggerId, Messages),
								loop(TriggerId, NewDataPoints, TriggerExchange,Net, Function, OutputList,Type);
							
							false ->
								Command = binary_to_term(Body),
								erlang:display(Command),
								case handle_command(Command,OutputList) of
									[] ->
										case erlastic_search:delete_doc(?INDEX,"trigger", TriggerId) of
											{error, _} -> 
												erlang:display("Error when deleting trigger:" ++ TriggerId ++" with no users");
											{ok,_List} -> 
												erlang:display("Deleted trigger:" ++ TriggerId ++" with no users")
										end,
										{Connection, ChannelIn, ChannelOut} = Net,
										amqp_channel:close(ChannelIn),
										amqp_channel:close(ChannelOut),
										amqp_connection:close(Connection),
										ok;
			
									NewOutputList ->
										erlang:display(NewOutputList),
										loop(TriggerId, DataPoints, TriggerExchange, Net, Function, NewOutputList,Type)
								end
                        end;
                quit ->
                        {Connection, ChannelIn, ChannelOut} = Net,
                        amqp_channel:close(ChannelIn),
                        amqp_channel:close(ChannelOut),
                        amqp_connection:close(Connection),
                        ok;
                _ -> loop(TriggerId, DataPoints, TriggerExchange,
                                 Net, Function, OutputList,Type)
        end.



%% ====================================================================
%% Helper functions
%% ====================================================================




%% @doc
%% Function: send/3
%% Purpose: Used to publish a message into specified exchange in the pub/sub
%% system on the specified channel.
%% Args: Channel - The channel on which we publish,
%% Exchange - The exchange we publish to,
%% Message - The message that we want to publish.
%% Returns: ok.
%% Side effects: Publish the message 'Message' into the exchange 'Exchange'.
%%
%% @end
-spec send(Channel :: pid(), Exchange :: binary(), Message :: binary()) -> ok.
send(Channel, Exchange, Message) ->
        amqp_channel:cast(Channel,
                                         #'basic.publish'{exchange = Exchange},
                                         #amqp_msg{payload = Message}).



%% @doc
%% Function: create_input_exchanges/1
%% Purpose: Used to create names of exchanges from a list of streams and
%% virtual streams, i.e [{stream, Id}, {vstream, Id}].
%% Args: List - A list of streams, i.e [{stream, Id}, {vstream, Id}].
%% Returns: [] | [Exchange]
%% @end
-spec create_input_exchanges(List) -> [] | [Exchange] when
                 List :: [StreamInfo],
          StreamInfo :: {Type, Id},
          Type :: atom(),
          Id :: string(),
          Exchange :: binary().
create_input_exchanges(List) ->
        create_input_exchanges(List, []).


%% @doc
%% Function: create_input_exchanges/2
%% Purpose: Used to create names of exchanges from a list of streams and
%% virtual streams, i.e [{stream, Id}, {vstream, Id}].
%% Args: List - A list of streams, i.e [{stream, Id}, {vstream, Id}],
%% Exchanges - The list to store the created exchanges in.
%% Returns: Exchanges | [Exchange] ++ Exchanges
%% @end
-spec create_input_exchanges(List, Exchanges) ->
                 Exchanges
                | [Exchange | Exchanges] when
                 List :: [StreamInfo],
          Exchanges :: list(),
          StreamInfo :: {Type, Id},
          Type :: atom(),
          Id :: string(),
          Exchange :: binary().
create_input_exchanges([], Exchanges) -> Exchanges;
create_input_exchanges([{Type, Id} | InputIds], Exchanges) when
  Type =:= stream; Type =:= vstream ->
        Exchange = list_to_binary(atom_to_list(Type) ++ "s." ++ Id),
        create_input_exchanges(InputIds, [Exchange | Exchanges]).
        



%% @doc
%% Function: subscribe/2
%% Purpose: Used to subscribe to the given exchanges on the given channel.
%% Args: ChannelIn - The channel on which communication to the server occurs,
%% InputExchanges - The list of exchanges to subscribe to.
%% Returns: ok
%% Side-effects: Creates one queue in RabbitMQ for each exchange and binds it
%% to the exchange.
%% @end
-spec subscribe(ChannelIn :: pid(), InputExchanges :: [binary()]) -> ok.
subscribe(_, []) -> ok;
subscribe(ChannelIn, [InputExchange | InputExchanges]) ->
        %% Declare INPUT queues
        amqp_channel:call(ChannelIn,
                                         #'exchange.declare'{exchange = InputExchange,
                                                                                 type = <<"fanout">>}),
        #'queue.declare_ok'{queue = QueueIn} =
                                                 amqp_channel:call(
                                                         ChannelIn,
                                                         #'queue.declare'{exclusive = true}),
        amqp_channel:call(ChannelIn, #'queue.bind'{exchange = InputExchange,
                                                                                         queue = QueueIn}),
        amqp_channel:subscribe(ChannelIn,
                                                 #'basic.consume'{queue = QueueIn, no_ack = true},
                                                 self()),
        receive
                #'basic.consume_ok'{} -> ok
        end,
        subscribe(ChannelIn, InputExchanges).




%% @doc
%% Function: get_first_value_for_streams()/1
%% Purpose: Used to get the latest value for each stream/virtual stream
%% in the provided list.
%% Args: List - The list of streams and virtual streams.
%% Returns: [] | [{Id, Value}].
%% @end
-spec get_latest_value_for_streams(List :: [{Type :: atom(), Id :: string()}])
        -> [] | [{Id :: string(), Value :: json() | undefined}].
get_latest_value_for_streams([]) -> [];
get_latest_value_for_streams([{Type, Id} | T]) ->
        JsonQuery = "{\"query\" : {\"term\" : {\"stream_id\" : \"" ++ Id ++ "\"}},"
                                        ++ "\"sort\" : [{\"timestamp\" : {\"order\" : \"desc\"}}],"
                                        ++ "\"size\" : 1}",
        DataPointType = case Type of
                                                stream -> "datapoint";
                                                vstream -> "vsdatapoint"
                                        end,
        case erlastic_search:search_json(#erls_params{}, ?ES_INDEX,
                                                                         DataPointType, JsonQuery) of
                {ok, Result} ->
                        Datapoint = lib_json:get_field(Result, "hits.hits"),
                        case Datapoint of
                                [] -> [{Id, undefined} | get_latest_value_for_streams(T)];
                                [Json] ->
                                        Value = lib_json:get_field(Json, "_source"),
                                        [{Id, Value} | get_latest_value_for_streams(T)]
                        end;
                {error, _} -> get_latest_value_for_streams(T)
        end.



%% @doc
%% Function: apply_function()/3
%% Purpose: Applies the specified predefined function
%%          with the list of datapoints and the input
%%          and returns the datapoint, streamid, input and 
%%          the list of users for that input if the trigger triggers
%% Returns: List of all triggers that triggered with the
%%          datapoint, streamid, input and users given for
%%          each of the triggered triggers
%% Side effects: Applies a predefined function
%% @end
-spec apply_function(Function :: atom(),List :: [{Id :: string(),DataPoint :: json() | undefined}], 
					 InputList :: [{Input::term(),Users::list()}]) -> list().
apply_function(_Function, _DataPoints, []) -> [];
apply_function(Function, DataPoints, InputList) ->
		Fun = list_to_atom(Function),
        DataList =
                lists:foldr(fun({StreamId, Data}, Acc) ->
                                                        case Data of
                                                                undefined -> Acc;
                                                                _ -> [{StreamId,Data}|Acc]
                                                        end
                                        end,
                                        [], DataPoints),
        triggers_lib:Fun(InputList,DataList).

        
        

%% @doc
%% Function: create_messages/3
%% Purpose: Used to create the messages being sent to 
%%          the output exchange
%% Returns: the list of messages created
%% @end
-spec create_messages(MessageList::list(),Timestamp::string(),Acc::list(),Type::string()) -> list().

create_messages([], _Timestamp,Acc,Type) ->
	Acc;
create_messages([{Value, StreamId, Input, Users}|Rest], Timestamp, Acc,Type) ->
	create_messages(Rest, Timestamp, 
				   [term_to_binary({Value,Timestamp,StreamId,Input,Users,Type})|Acc],Type).

%% @doc
%% Function: send_messages/3
%% Purpose: Used to send the messages on the 
%%          output exchange
%% Returns: ok
%% @end
-spec send_messages(TriggerExchange::binary(),ChannelOut::pid(),MessageList::list()) -> list().

send_messages(TriggerExchange,ChannelOut,[]) ->
	ok;
send_messages(TriggerExchange,ChannelOut,[Msg|Rest]) ->
  send(ChannelOut, TriggerExchange,Msg),
  send_messages(TriggerExchange,ChannelOut,Rest).



%% @doc
%% Function: send_to_output/2
%% Purpose: Used to send the messages to the different outputs 
%% the outputs can either be a user or a uri. If it is a user
%% then the message will be added to the notifications list in
%% the user. If it is a uri, the message will be POSTed to the
%% uri. The message is a json object with the following format:
%% 
%% Example message: {"trigger": {"value": Value, "timestamp": Timestamp, "stream_id": StreamId, "trigger_id": TriggerId}}
%%
%% Returns: ok | {error,{Code, Body}}
%% @end
-spec send_to_output(TriggerId::string(),tuple()) ->  ok | {error,_}.
send_to_output(TriggerId, []) ->
    ok;
send_to_output(TriggerId, [Head|Tail]) when is_binary(Head) ->
    send_to_output(TriggerId, [binary_to_term(Head)|Tail]);
send_to_output(TriggerId, [{Value, Timestamp, StreamId, Input, [],Type}|Messages]) ->
    send_to_output(TriggerId, Messages);
send_to_output(TriggerId, [{Value, Timestamp, StreamId, Input, [{user,UserId}|Rest],Type}|Messages]) ->
    Message = lib_json:set_attrs([{trigger, "{}"},
        {"trigger.value", Value},
        {"trigger.timestamp", list_to_binary(Timestamp)},
        {"trigger.stream_id", list_to_binary(StreamId)},
        {"trigger.trigger_id", list_to_binary(TriggerId)},
        {"trigger.input", Input},
		{"trigger.type", list_to_binary(Type)}]),
    UpdateJson = "{\"script\":\"ctx._source.notifications += msg\",\"params\":{\"msg\":"++ Message ++"}}",
    case api_help:update_doc(?INDEX, "user", UserId, UpdateJson, []) of
        {error, {Code, Body}} ->
            {error, {Code, Body}};
        {ok, Response} ->
            ok
    end,
    send_to_output(TriggerId, [{Value, Timestamp, StreamId, Input, Rest,Type}|Messages]);
send_to_output(TriggerId, [{Value, Timestamp, StreamId, Input, [{uri,{URI,_}}|Rest],Type}|Messages]) ->
    Message = lib_json:set_attrs([{trigger, "{}"},
        {"trigger.value", Value},
        {"trigger.timestamp", list_to_binary(Timestamp)},
        {"trigger.stream_id", list_to_binary(StreamId)},
        {"trigger.trigger_id", list_to_binary(TriggerId)},
        {"trigger.input", Input},
		{"trigger.type", list_to_binary(Type)}]),
    erlang:display(URI),
    case httpc:request(post, {URI, [],"application/json", Message}, [{timeout, 5000}], []) of
        {ok,{{_, 200, _}, _, _}} ->
            ok;
        {ok,{{_, 204, _}, _, _}} ->
            ok;
        {ok, {{_, Code, _}, _, Body}} ->
            erlang:display("ERROR"),
            erlang:display(Body),
            erlang:display(Code),
            {error, {Code, Body}};
		{error, Reason} ->
			erlang:display(Reason)
    end,
    send_to_output(TriggerId, [{Value, Timestamp, StreamId, Input, Rest,Type}|Messages]);
send_to_output(TriggerId, [{Value, Timestamp, StreamId, Input, [_|Rest],Type}|Messages]) ->
    erlang:display("Invalid output!"),
    send_to_output(TriggerId, [{Value, Timestamp, StreamId, Input, Rest,Type}|Messages]).
              

%% @doc
%% Function: handle_command/2
%% Purpose: Used to handle commands sent on the
%%          command exchange
%% Returns: The new output list
%% @end
-spec handle_command(TriggerExchange::{atom(),{term(),term()}},OutputList::list()) -> list().
handle_command({add,{Input,User}},[]) ->
	[{Input,[User]}];
handle_command({add,{Input,User}},[{Input,Users}|Rest]) ->
	case lists:member(User, Users) of
		true ->
			[{Input,Users}|Rest];
		false ->
			[{Input,[User|Users]}|Rest]
	end;
handle_command({add,{Input,User}},[First|Rest]) ->
	[First|handle_command({add,{Input,User}},Rest)];

handle_command({remove,{Input,User}},[]) ->
	[];
handle_command({remove,{Input,User}},[{Input,[User]}|Rest]) ->
	Rest;
handle_command({remove,{Input,User}},[{Input,Users}|Rest]) ->
	[{Input,lists:delete(User, Users)}|Rest];
handle_command({remove,{Input,User}},[First|Rest]) ->
	[First|handle_command({remove,{Input,User}},Rest)];
handle_command(UnknowCommand,List) ->
	List.


	


%% HOW TO SEND TO A STREAMPROCESS
% StreamId = "1",
% ResourceId = "1",
% Exchange = list_to_binary("resources."++ResourceId),
%
% {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "localhost"}),
% {ok, Channel} = amqp_connection:open_channel(Connection),
%
% amqp_channel:call(Channel,
% #'exchange.declare'{exchange = Exchange,
% type = <<"fanout">>}),
%
% PID = spawn(streamProcess, create, [StreamId, ResourceId]),
% io:format("Node Created: ~p~n", [PID]),
%
% %% Needed for the RabbitMQ to have time to set up the system.
% timer:sleep(1000),
%
% amqp_channel:cast(Channel,
% #'basic.publish'{exchange = Exchange},
% #amqp_msg{payload = term_to_binary({post, DatapointJson})}),
%
% ok = amqp_channel:close(Channel),
% ok = amqp_connection:close(Connection),
