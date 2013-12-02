%% @author Tholsg�rd Gabriel, Li Hao
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == poller ==
%% this module implements the functionalities of the poller, which communicates with the external
%% resources
%% @end
-module(poller).
-behaviour(gen_server).
-include("parser.hrl").
-include("state.hrl").
-include("common.hrl").
-include_lib("amqp_client.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, init/1, handle_call/3, handle_info/2, terminate/2]).

%% @doc
%% Function: start_link/1
%% Purpose: start function used to start the poller, will call init/1 function later
%% Parameter: State--type of state record(), define the necessary information of the poller.
%% Returns: {ok, Pid} | {error, ErrMsg}
%% @end
-spec start_link(State :: record()) -> {ok, pid()} | {error, term()}.
start_link(State)->
	gen_server:start_link(?MODULE, State, []).

%% @doc
%% Function: init/1
%% Purpose: init function used to initialize this poller gen_server.
%% Returns: {ok, State}
%% Side Effects: start inets and start the ssl, declare a new RabbitMQ exchange
%% @end
-spec init(State :: record()) -> {ok, record()}.
init(State)->
	application:start(inets),
	ssl:start(),
	
	%% create exchange for rabbitMQ
	StreamExchange = list_to_binary("streams."++State#state.stream_id),
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{host = "localhost"}),
	{ok, ChannelOut} = amqp_connection:open_channel(Connection),
	amqp_channel:call(ChannelOut, #'exchange.declare'{exchange = StreamExchange, type = <<"fanout">>}),
	
	{ok, #state{stream_id=State#state.stream_id, uri=State#state.uri, parser=State#state.parser, exchange=StreamExchange, channel=ChannelOut, connection=Connection}}.

%% @doc
%% Function: handle_call/2
%% Purpose: handle synchronous call of gen_server, could be called via: gen_server:call(pid(), {rebuild})
%% Parameter: Request   -- the request provided by gen_server:call() or gen_server:multi_call()
%%            _Form     -- the tuple {Pid, Tag}, in which the Pid is the pid of the cliend, and the Tag is a unique tag
%%            State     -- the current state of the gen_server, which may contain some status information.
%% Returns: {reply, (returned message), (new state of gen_server)}
%%          (returned message) could be any thing you want to return to the client.
%% @end
-spec handle_call(Request :: tuple(), _Form :: tuple(), State :: record()) -> {reply, {update, StreamId :: string(), FinalUri :: string(), NewFreq :: integer()}, State :: record()}
																			 |{reply, {error, Reason :: string()}, State :: record()}
																			 |{reply, {info, State :: record()}, State :: record()}.
handle_call({rebuild}, _Form, State)->
	StreamId = State#state.stream_id,
	Url = State#state.uri,

	case erlastic_search:get_doc(?ES_INDEX, "stream", StreamId) of 
		{error,Reason} -> 
			erlang:display("Failed to retrieve the stream according to stream`s id"),
			erlang:display("The error reason: "++Reason),
			
			{reply, {error, Reason}, State};
		{ok,JsonStruct} ->
		    FinalJson = lib_json:get_and_add_id(JsonStruct),
			
			Parser = poll_help:get_parser_by_id(StreamId), %% return a list of parsers [#parser, ...]
			case Parser of
				{error, ErrMsg} ->
					{reply, {error, ErrMsg}, State};
				_ ->
					NewUri = lib_json:get_field(FinalJson, "uri"),
					NewFreq = lib_json:get_field(FinalJson, "polling_freq"),
					case is_binary(NewUri) of
						false->
							FinalUri = NewUri;
						_ ->
							FinalUri = binary_to_list(NewUri)
					end,			
					%% notify the supervisor to refresh its records
					{reply, {update, StreamId, FinalUri, NewFreq}, #state{stream_id=StreamId, uri=FinalUri, parser=Parser, exchange=State#state.exchange, channel=State#state.channel, connection=State#state.connection}}
			end
	end;
handle_call({check_info}, _Form, State)->
	%% return the information of poller
	{reply, {info, State}, State}.
	
%% @doc
%% Function: handle_info/2
%% Purpose: handle messages processing of the gen_server, could be called via: pid()!{probe}
%% Parameter: {probe} -- the message sent from the client
%%            State   -- contains some status information of the gen_server
%% Returns: {noreply, NewState}
%% @end
-spec handle_info({probe}, State :: record()) -> {noreply, record()}.
handle_info({probe}, State)->
	StreamId = State#state.stream_id,
	Parser = State#state.parser,
	Uri = State#state.uri,
	
	%%communicate with external resources
	%%http://userprimary.net/posts/2009/04/04/exploring-erlangs-http-client/
	
	case httpc:request(get, {Uri, [{"User-Agent", (?UA++StreamId)}]}, [], []) of
		{ok, {{_HttpVer, Code, _Msg}, Headers, Body}}->
			case Code==200 of
				true->
					%% get the time from the http response header
					TimeList = case lists:keyfind("date", 1, Headers) of
									{"date", Date}->
										string:tokens(Date, " ");
									_->
										[]
								end,
					
					FinalData = case Parser#parser.input_type of
									"application/json" ->
										parser:parseJson(Parser, Body, make_stamp(TimeList));
									"plain/text" ->
										parser:parseText(Parser, Body, make_stamp(TimeList));
									_ ->
										%% the input type of json is wrong
										erlang:display("the data type user provided is not correct!!")
								end,
					case FinalData == true of
						false->
							amqp_channel:cast(State#state.channel,
					  							#'basic.publish'{exchange = State#state.exchange},
					  							#amqp_msg{payload = list_to_binary(FinalData)});
						_->
							continue
					end;

				_ ->
					%%polling fails
					erlang:display("polling failed")
			end,
			{noreply, State};
		{error, Reason}->
			erlang:display("failed to poll external resource, "++Reason),
			{noreply, State};
		_ ->
			erlang:display("failed to poll external resource"),
			{noreply, State}
	end.

%% @doc
%% Function: terminate/2
%% Purpose: handles what is going to do when the poller is terminated
%% Parameter: _T    -- the reason denoting the stop reason  
%%            State -- contains the status information of the gen_server
%% Returns: ok | {error, Reason}
%% @end
-spec terminate(_Reason :: term(), State :: record()) -> ok | {error, string()}.
terminate(_Reason, State)->
	%% Close the channel
    amqp_channel:close(State#state.channel),
    %% Close the connection
    amqp_connection:close(State#state.connection),
	Uri = State#state.uri,
	erlang:display("the poller for "++Uri++" stops working!"),
	application:stop(inets).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Function: make_timestamp/1
%% Purpose: transform the time item list to timestamp string
%% Example: ["Thu,","21","Nov","2013","09:32:42","GMT"] => "2013:11:21T09:32:42" 
%% Returns: string()
%% @end
-spec make_stamp(list()) -> string().
make_stamp(TimeList)->
	Day = lists:nth(2, TimeList),
	Month = case lists:nth(3, TimeList) of
				"Jan"->"01";
				"Feb"->"02";
				"Mar"->"03";
				"Apr"->"04";
				"May"->"05";
				"Jun"->"06";
				"Jul"->"07";
				"Aug"->"08";
				"Sep"->"09";
				"Oct"->"10";
				"Nov"->"11";
				"Dec"->"12"
			end,
	Year = lists:nth(4, TimeList),
	Time = lists:nth(5, TimeList),
	Year++"-"++Month++"-"++Day++"T"++Time++".000".
