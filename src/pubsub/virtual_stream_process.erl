%% @author Anders Steinrud, Gabriel Tholsgård
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == virtual_stream_process ==
%% This module represents a virtual stream process which subscribes to data
%% points from the pub/sub system for specified streams/virtual streams and
%% publish it back into the pub/sub system for the represented virtual stream
%% after applying the specified function with the data points as arguments. 
%%
%% @end
-module(virtual_stream_process).

-include_lib("erlastic_search.hrl").
-include_lib("amqp_client.hrl").
-include_lib("pubsub.hrl").
-include_lib("common.hrl").
-include_lib("json.hrl").
-include_lib("debug.hrl").

-export([create/3]).


%% ====================================================================
%% API functions
%% ====================================================================


%% @doc
%% Function: create/3
%% Purpose: Used to initialize the virtual stream process which subscribes to
%%          the exchanges beloning to the specified streams/virtual streams and
%%          publish it into the exchange for the specified virtual stream.
%% Args: VStreamId - The id of the publishing virtual stream,
%%       InputIds - List ids' of the streams/virtual streams to subscribe from,
%%                  i.e [{stream, Id}, ... , {vstream, Id}, ...].
%%       Function - Tag of which function that will be applied to the incoming
%%                  data points before publishing.
%% Returns: Return ok.
%% Side effects: Starts loop receiving data points from the pub/sub
%%               system for streams/virtual streams and sending data
%%               into the pub/sub system for a virtual stream
%%               with id 'VStreamId'.
%%
%% @end
-spec create(VStreamId :: string(), InputIds :: [StreamInfo],
			 Function :: atom()) ->  ok when
		  StreamInfo :: {Type, Id},
          Type :: atom(),
          Id :: string().
create(VStreamId, InputIds, Function) ->

	%% Exchange name binarys.
	InputExchanges = create_input_exchanges(InputIds),
	VStreamExchange = list_to_binary("vstreams." ++ VStreamId),
	
	%% Connect.
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{}),
	
	%% Open In and OUT channels.
	{ok, ChannelIn} = amqp_connection:open_channel(Connection),
	{ok, ChannelOut} = amqp_connection:open_channel(Connection),
	
	%% Declare INPUT queues and subscribe.
	subscribe(ChannelIn, InputExchanges),
	
	%% Declare OUTPUT exchange.
	amqp_channel:call(ChannelOut,
					  #'exchange.declare'{exchange = VStreamExchange,
										  type = <<"fanout">>}),

	%% Needed for the RabbitMQ to have time to set up the system.
	timer:sleep(1000),
	
	%% Get the latest value for each datapoint.
	DataPoints = get_first_value_for_streams(InputIds),
	
	loop(VStreamId, DataPoints, VStreamExchange,
		 {Connection, ChannelIn, ChannelOut}, Function, false).



%% ====================================================================
%% Internal functions
%% ====================================================================



%% @doc
%% Function: loop/6
%% Purpose: Receives data points from the pub/sub system for specified
%%          exchanges and publish the result of the function with the
%%          data points as argument it into the pub/sub system for the
%%          exchange of the specified virtual stream.
%% Args: VStreamId - Id of a virtual stream,
%%       DataPoints - A list of the latest data points for each stream,
%%       VStreamExchange - Exchange to publish to,
%%       Net - A tuple holding the connection to the RMQ server and on which
%%             channels ingoing and outgoing communication occurs, 
%%       Function - Function specifier to apply to the received data point(s),
%%       Calculate - Boolean which specifies if a value should be calculated.
%% Returns: ok.
%% Side effects: Non terminating loop receiving data points from the pub/sub
%%               system, applying the specified function to it and publishing
%%               the result to the exchange VStreamExchange.
%% @end
-spec loop(VStreamId :: string(),
		   DataPoints :: [{Id :: string(), DataPoint :: json() | undefined}],
		   VStreamExchange :: binary(),
		   Net :: {Connection :: pid(), ChannelIn :: pid(), ChannelOut :: pid()},
		   Function :: string(),
		   Calculate :: boolean()) -> ok.
loop(VStreamId, DataPoints, VStreamExchange, Net, Function, Calculate) ->
	
	%% Receive from the subscribeTopic!
	receive
		{#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
			Data = binary_to_list(Body),
			case erlson:is_json_string(Data) of
				%% New value from the source as a Json
				true ->
					Id = binary_to_list(lib_json:get_field(Data, "stream_id")),
					NewDataPoints =
						lists:keyreplace(Id, 1, DataPoints, {Id, Data}),
					
					%% Apply function to the new values
					[Value] = apply_function(list_to_binary(VStreamId),
											 Function, NewDataPoints),
					
					%% Create timestamp to avoid issues with asynchronus
					%% datapoints.
					Timestamp = ?TIME_NOW(erlang:localtime()),
					
					%% Create new datapoint message
					Msg = lib_json:replace_field(Value, "timestamp",
												 list_to_binary(Timestamp)),
					
					%% Store value in ES
					case erlastic_search:index_doc(?ES_INDEX,
												   "vsdatapoint", Msg) of
						{error, Reason} -> {error, Reason};
						{ok, _} ->
							%% Publish the calculated value
							ChannelOut = element(3, Net),
							send(ChannelOut, VStreamExchange,
								 list_to_binary(Msg)),
							loop(VStreamId, NewDataPoints, VStreamExchange,
								 Net, Function, false)
					end;
				_ ->
					loop(VStreamId, DataPoints, VStreamExchange,
						 Net, Function, Calculate)
			end;
		quit ->
			{Connection, ChannelIn, ChannelOut} = Net,
			amqp_channel:close(ChannelIn),
			amqp_channel:close(ChannelOut),
			amqp_connection:close(Connection),
			ok;
		_ -> loop(VStreamId, DataPoints, VStreamExchange,
				  Net, Function, Calculate)
	end.













%% ====================================================================
%% Helper functions
%% ====================================================================




%% @doc
%% Function: send/3
%% Purpose: Used to publish a message into specified exchange in the pub/sub
%%          system on the specified channel.
%% Args: Channel - The channel on which we publish,
%%       Exchange - The exchange we publish to,
%%       Message - The message that we want to publish.
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
%%          virtual streams, i.e [{stream, Id}, {vstream, Id}].
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
%%          virtual streams, i.e [{stream, Id}, {vstream, Id}].
%% Args: List - A list of streams, i.e [{stream, Id}, {vstream, Id}],
%%       Exchanges - The list to store the created exchanges in.
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
%%       InputExchanges - The list of exchanges to subscribe to.
%% Returns: ok
%% Side-effects: Creates one queue in RabbitMQ for each exchange and binds it
%%               to the exchange.
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
-spec get_first_value_for_streams(List :: [{Type :: atom(), Id :: string()}])
	-> [] | [{Id :: string(), Value :: json() | undefined}].
get_first_value_for_streams([]) -> [];
get_first_value_for_streams([{Type, Id} | T]) ->
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
				[] -> [{Id, undefined} | get_first_value_for_streams(T)];
				[Json] ->
					Value = lib_json:get_field(Json, "_source"),
					[{Id, Value} | get_first_value_for_streams(T)]
			end;
		{error, _} -> get_first_value_for_streams(T)
	end.







%% @doc
%% Function: apply_function()/3
%% Purpose: Applies the specified predefined function
%%          with the list as its argument.
%% Args: VStreamId - The virtual stream id for which
%%		 			 we are doing the calculation,
%%		 Function - Specifier of a predefined function,
%%       List - List of datapoints for streams and virtual streams.
%% Returns: Value of the predefined function.
%% Side effects: Applies a predefined function
%% @end 
-spec apply_function(VStreamId :: binary(),
					 Function :: atom(),
					 List :: [{Id :: string(),
							   DataPoint :: json() | undefined}]) -> number().
%% TODO - Make the predefined functions
apply_function(_VStreamId, _Function, []) -> 0;
apply_function(VStreamId, Function, DataPoints) ->
	DataList =
		lists:foldr(fun({_, Data}, Acc) ->
							case Data of
								undefined -> Acc;
								_ -> [Data|Acc]
							end
					end,
					[], DataPoints),
	vs_func_lib:Function([DataList], VStreamId).

	
	



%% HOW TO SEND TO A STREAMPROCESS
%                        StreamId = "1",
%                        ResourceId = "1",
%                        Exchange = list_to_binary("resources."++ResourceId),
%
%                        {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "localhost"}),
%                        {ok, Channel} = amqp_connection:open_channel(Connection),
%
%                        amqp_channel:call(Channel,
%                                        #'exchange.declare'{exchange = Exchange,
%                                                        type = <<"fanout">>}),
%
%                        PID = spawn(streamProcess, create, [StreamId, ResourceId]),
%                        io:format("Node Created: ~p~n", [PID]),
%
%                        %% Needed for the RabbitMQ to have time to set up the system.
%                        timer:sleep(1000),
%
%                        amqp_channel:cast(Channel,
%                                         #'basic.publish'{exchange = Exchange},
%                                         #amqp_msg{payload = term_to_binary({post, DatapointJson})}),
%
%                        ok = amqp_channel:close(Channel),
%                        ok = amqp_connection:close(Connection),