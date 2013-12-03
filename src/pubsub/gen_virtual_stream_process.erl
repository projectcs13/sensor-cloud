%% @author Anders Steinrud, Gabriel Tholsgård <gath5951@student.uu.se>
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == gen_virtual_stream_process ==
%% Represents a gen_server of a virtual stream process which subscribes to
%% data points from the pub/sub system for specified streams/virtual streams
%% and publish it back into the pub/sub system for the represented virtual
%% stream after applying the specified function with the data points as
%% arguments. 
%%
%% @end

-module(gen_virtual_stream_process).
-behaviour(gen_server).

-include_lib("erlastic_search.hrl").
-include_lib("amqp_client.hrl").
-include_lib("pubsub.hrl").
-include_lib("common.hrl").
-include_lib("json.hrl").
-include_lib("debug.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/3]).



%% start_link/3
%% ====================================================================
%% @doc
%% Function: start_link/3
%% Purpose: Initializes as a gen_server process.
%% Args: VStreamId - String representing the virtual streams ID,
%%		 InputIds - A list of streams/virtual streams to subscribe to,
%%		 Function - Name of function to use during calculation.
%% Return: {ok, Pid}.
%% Side effect: Starts a gen_server process.
%% @end
-spec start_link(VStreamId :: string(),
				 InputIds :: [StreamInfo], Function :: atom()) -> {ok, pid()}
				 when StreamInfo :: {Type, Id},
				 	  Type :: stream | vstream,
				 	  Id :: string().
%% ====================================================================
start_link(VStreamId, InputIds, Function) ->
	gen_server:start_link(?MODULE, [VStreamId, InputIds, Function], []).




%% ====================================================================
%% Behavioural functions 
%% ====================================================================



%% spec of record state:
%% {string(), [{atom(),string()}], binary(), {pid(), pid(), pid()}, atom()}
-record(state, {vstreamid, datapoints, exchange, network, function, log}).



%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
%% Args: Data - A list of length three.
%%              First element is the virtual stream id as a string.
%%              Second element is a list of streams to subscribe to in the
%%              format: [{Type, Id}]
%%				where Type is the atom stream or vstream and Id is a string.
%%				Third element is the function to apply as a atom,
%%				i.e sum, max, min, avg.
-spec init(Args :: Data) -> Result when
	Data :: iolist(),
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: record(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([VStreamId, InputIds, Function]) ->
	%% Trap exits from parent
	process_flag(trap_exit, true),
	
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
	DataPoints = get_latest_value_for_streams(InputIds),
	
	%% Create state
	State = #state{vstreamid=VStreamId,
				   datapoints = DataPoints,
				   exchange = VStreamExchange,
				   network = {Connection, ChannelIn, ChannelOut},
				   function = Function,
				   log = []},
    {ok, State}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
%% Not used by our server
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
%% Not used by our server
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: record()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: record(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({#'basic.deliver'{}, #amqp_msg{payload = Body}},
			State = #state{vstreamid = VStreamId, datapoints = DataPoints,
						   exchange = VStreamExchange, network = Net,
						   function = Function}) ->
	Data = binary_to_list(Body),
	case erlson:is_json_string(Data) of
		%% New value from the source as a Json
		true ->
			Id = binary_to_list(lib_json:get_field(Data, "stream_id")),
			%% TODO Check whether it is a virtual stream or a stream before
			%%      replacing the key, since they can have the same key.
			NewDataPoints = lists:keyreplace(Id, 1, DataPoints, {Id, Data}),
			
			%% Apply function to the new values
			[Value] = apply_function(list_to_binary(VStreamId),
									 Function, NewDataPoints),

			case Value of
				none -> {noreply, State};
				_ ->
					%% Create timestamp to avoid issues with
					%% asynchronus data points.
					Timestamp = ?TIME_NOW(erlang:localtime()),
			
					%% Create new datapoint message
					Msg = lib_json:replace_field(Value, "timestamp",
												 list_to_binary(Timestamp)),
			
					%% Store value in ES
					Log = case erlastic_search:index_doc(?ES_INDEX,
														 "vsdatapoint", Msg) of
						  	{error, Reason} ->
						  		erlang:display({error, Reason}),
						  		%% TODO Persistent storage using file?
								[Msg | State#state.log];
							{ok, _} ->
								%% Try storing the log
								store_log_in_es(State#state.log)
						  end,
					%% Publish the calculated value
					ChannelOut = element(3, Net),
					send(ChannelOut, VStreamExchange, list_to_binary(Msg)),
					{noreply, State#state{datapoints = NewDataPoints,
										  log = Log}}
			end;
		_ ->
			{noreply, State}
	end;

handle_info(quit, State) ->
	{stop, normal, State};
			
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================

terminate(_Reason, #state{network = Net}) when Net /= undefined ->
	{Connection, ChannelIn, ChannelOut} = Net,
	amqp_channel:close(ChannelIn),
	amqp_channel:close(ChannelOut),
	amqp_connection:close(Connection),
	ok;

terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.









%% ====================================================================
%% Internal functions
%% ====================================================================




%% send/3
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
%% ====================================================================
send(Channel, Exchange, Message) ->
	amqp_channel:cast(Channel,
					  #'basic.publish'{exchange = Exchange},
					  #amqp_msg{payload = Message}).







%% store_log_in_es/1
%% ====================================================================
%% @doc
%% Function: store_log_in_es/1
%% Purpose: Used to store data points into Elastic Search that previously
%%			could not be stored.
%% Args: Log - List of data points to store.
%% Returns: [] | NewLog where NewLog contain all data points that still could
%%			not be stored.
%% Side effects: Stores data points into Elastic Search.
%%
%% @end
-spec store_log_in_es([Log :: json()]) -> [] | [NewLog :: json()].
%% ====================================================================
store_log_in_es([]) -> [];
store_log_in_es([Msg | Log]) ->
	case erlastic_search:index_doc(?ES_INDEX, "vsdatapoint", Msg) of
		{error, Reason} ->
			erlang:display({error, Reason}),
			[Msg | Log];
		{ok, _} ->
			store_log_in_es(Log)
	end.










%% create_input_exchanges/1
%% ====================================================================
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
%% ====================================================================
create_input_exchanges(List) ->
	create_input_exchanges(List, []).

%% create_input_exchanges/2
%% ====================================================================
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
%% ====================================================================
create_input_exchanges([], Exchanges) -> Exchanges;
create_input_exchanges([{Type, Id} | InputIds], Exchanges) when
  Type =:= stream; Type =:= vstream ->
	Exchange = list_to_binary(atom_to_list(Type) ++ "s." ++ Id),
	create_input_exchanges(InputIds, [Exchange | Exchanges]).







%% subscribe/2
%% ====================================================================
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
%% ====================================================================
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






%% get_latest_value_for_streams/1
%% ====================================================================
%% @doc
%% Function: get_latest_value_for_streams/1
%% Purpose: Used to get the latest value for each stream/virtual stream
%% in the provided list.
%% Args: List - The list of streams and virtual streams.
%% Returns: [] | [{Id, Value}].
%% @end
-spec get_latest_value_for_streams(List :: [{Type :: atom(), Id :: string()}])
	-> [] | [{Id :: string(), Value :: json() | undefined}].
%% ====================================================================
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





%% apply_function/3
%% ====================================================================
%% @doc
%% Function: apply_function/3
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
							   DataPoint :: json() | undefined}]) -> [number()].
%% ====================================================================
apply_function(_VStreamId, _Function, []) -> [none];
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

