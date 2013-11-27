%% @author Tholsgård Gabriel, Li Hao
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
%% Returns: {ok, Pid} | {error, ErrMsg}
%% @end
-spec start_link(tuple()) -> tuple() | atom().
start_link(State)->
	gen_server:start_link(?MODULE, State, []).

%% @doc
%% Function: init/1
%% Purpose: init function used to initialize this poller gen_server.
%% Returns: {ok, State}
%% Side Effects: start inets and start the ssl
%% @end
-spec init(tuple()) -> tuple().
init(State)->
	application:start(inets),
	ssl:start(),
	
	%% create exchange for rabbitMQ
	StreamExchange = list_to_binary("streams."++State#state.stream_id),
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{host = "localhost"}),
	{ok, ChannelOut} = amqp_connection:open_channel(Connection),
	amqp_channel:call(ChannelOut, #'exchange.declare'{exchange = StreamExchange, type = <<"fanout">>}),
	
	State#state{exchange = StreamExchange, channel = ChannelOut},
	
	{ok, #state{stream_id=State#state.stream_id, uri=State#state.uri, parser=State#state.parser, exchange=StreamExchange, channel=ChannelOut}}.

%% @doc
%% Function: handle_call/2
%% Purpose: handle synchronous call of gen_server, could be called via: gen_server:call(pid(), {rebuild})
%% Returns: {reply, (returned message), (new state of gen_server)}
%% @end
-spec handle_call(any(), tuple(), tuple()) -> {reply, any(), tuple()}.
handle_call({rebuild}, _Form, State)->
	StreamId = State#state.stream_id,
	Url = State#state.uri,
	%%to rebuild the poller 
	%%extract the uri from the datastore according to stream id
	%%get the paser from the datastore

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
					{reply, {update, StreamId, FinalUri, NewFreq}, #state{stream_id=StreamId, uri=FinalUri, parser=Parser, exchange=State#state.exchange, channel=State#state.channel}}
			end
	end;
handle_call({check_info}, _Form, State)->
	%% return the information of poller
	{reply, {info, State}, State}.
	
%% @doc
%% Function: handle_info/2
%% Purpose: handle messages processing of the gen_server, could be called via: pid()!{probe}
%% Returns: {noreply, NewState}
%% @end
-spec handle_info(any(), tuple()) -> {noreply, tuple()}.
handle_info({probe}, State)->
	StreamId = State#state.stream_id,
	Parser = State#state.parser,
	Uri = State#state.uri,
	
	%%communicate with external resources
	%%http://userprimary.net/posts/2009/04/04/exploring-erlangs-http-client/
	
	case httpc:request(get, {Uri, [{"User-Agent", (?UA++StreamId)}]}, [], []) of
		{ok, {{HttpVer, Code, Msg}, Headers, Body}}->
			case Code==200 of
				true->
					
					TimeList = case lists:keyfind("date", 1, Headers) of
									{"date", Date}->
										string:tokens(Date, " ");
									_->
										[]
								end,
					
					FinalData = case check_header(Headers) of
									"application/json" ->
										parser:parseJson(Parser, Body, TimeList);
									"plain/text" ->
										parser:parseText(Parser, Body, TimeList);
									_ ->
										%%the other options
										erlang:display("other content type")
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
%% Returns: ok | {error, Reason}
%% @end
-spec terminate(tuple(), tuple()) -> atom | tuple().
terminate(_T, State)->
	Uri = State#state.uri,
	erlang:display("the poller for "++Uri++" stops working!"),
	application:stop(inets).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Function: check_header/1
%% Purpose: used to return the content-type of the response from the response`s header.
%% Returns: "no content type" | content-type
%% @end
-spec check_header(list(string())) -> string().
check_header([]) -> "no content type";
check_header([Tuple|Tail]) ->
	case Tuple of
		{"content-type", Res} -> Res;
		_ -> check_header(Tail)
	end.



