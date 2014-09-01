%% @author Tholsgård Gabriel, Li Hao
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == polling_system ==
%% This module contains all functionalities needed to poll from external resources.
%% We are going to generate one supervisor process and a few poller processes.
%% @end

-module(polling_system).
-behaviour(gen_server).
-include("poller.hrl").
-include("state.hrl").
-include("field_restrictions.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

%% @doc
%% Function: start_link/0
%% Purpose: start function used to generate the supervisor process, polling_monitor, and create all pollers.
%% Returns: {ok, Pid} | ignore | {error, ErrMsg} | {error, {already_started, Pid}}
%% Side effects: spawn supervisor process and register it by name polling_supervisor. spawn polling_monitor and fetch data from elasticsearch to create pollers.
%% @end
-spec start_link() -> {ok, pid()} | ignore | {error,{already_started, pid()}} | {error, term()}.
start_link()->
	
	case whereis(polling_supervisor) of
		undefined->			
			gen_server:start_link({local, polling_supervisor}, ?MODULE, [], []),
			polling_monitor:start_link(),
			case whereis(polling_supervisor) of
				undefined->
					erlang:display("polling supervisor has not been started");
				_ ->
					gen_server:call(polling_supervisor, create_pollers)
			end;
		_ ->
			%%gen_event could be used here to handle the error conditions!
			{error, "the supervisor has already been started"}
	end.

%% @doc
%% Function: init/1
%% Purpose: init function called by gen_server:start_link() method, used to initialize the state of gen_server
%% Returns: {ok, State}
%% @end
-spec init(any()) -> {ok, list()}.
init(_)->
	{ok, []}.

%% @doc
%% Function: handle_info/2
%% Purpose: handle the messages` processing of the gen_server, accepts two parameters: message and the old state of gen_server.
%%			could be called by: polling_supervisor!{message}
%%			currently only for implementing the handle_info/2 interface of gen_server, not quite useful.
%% Returns: {noreply, State}
%% @end
-spec handle_info(Request :: term(), _State :: [record()]) -> {noreply, list()}.
handle_info({print, Message}, _State)->
	erlang:display(Message),
	{noreply, _State}.

%% @doc
%% Function: handle_cast/2
%% Purpose: handle asynchronous call of gen_server, accepts two parameters: message of the call and the old state of gen_server.
%%			can be called via: gen_server:cast(polling_supervisor, {rebuild_poller, (stream_id)}).
%% Parameters: Request 	   -- request sent from the client, could be any type of request.
%%             PollersInfo -- the state of the gen_server, a list which contains all the information of the pollers. 
%% Returns: {noreply, NewState}
%% Side effects: create new poller and send message to specific poller.
%% @end
-spec handle_cast(Request :: term(), PollersInfo :: list()) -> {noreply, NewState :: list()}.
handle_cast({create_poller, #pollerInfo{stream_id = StreamId, name = StreamName, uri = Uri, frequency = Frequency, data_type = DataType, parser = Parser}}, PollersInfo)->
	case {is_list(StreamId), is_list(Uri), is_integer(Frequency), Frequency>0} of
		{true, true, true, true}->
			{ok, Pid}=supervisor:start_child(polling_monitor, [#state{stream_id=StreamId, uri=Uri, parser=Parser, data_type=DataType}]),
			%%use timer library to create scheduler for this poller
			timer:start(),
			{ok, TRef} = timer:send_interval((Frequency*1000), Pid, {probe}),
			Record = #pollerInfo{stream_id=StreamId, name=StreamName, uri=Uri, frequency=Frequency, parser=Parser, data_type=DataType, pid=Pid, timer_ref=TRef},
			{noreply, [Record|PollersInfo]};
		{false, _, _, _}->erlang:display("create_poller: please provide a string stream id!"),
					      {noreply, PollersInfo};
		{_, false, _, _}->erlang:display("create_poller: please provide a string uri!"),
					      {noreply, PollersInfo};
		{_, _, false, _}->erlang:display("create_poller: please provide a integer frequency!"),
					   	  {noreply, PollersInfo};
		{_, _, _, false}->erlang:display("create_poller: please provide a positive frequency value!"),
						  {noreply, PollersInfo}
	end;
handle_cast({terminate, StreamId}, PollersInfo)->
	Poller = find_poller_by_id(StreamId, PollersInfo),
	case Poller of
		{error, ErrMsg}->
			erlang:display(ErrMsg),
			{noreply, PollersInfo};
		_ ->
			supervisor:terminate_child(polling_monitor, Poller#pollerInfo.pid),
			supervisor:delete_child(polling_monitor, Poller#pollerInfo.pid),
			erlastic_search:delete_doc(?INDEX,"pollinghistory", StreamId),
			{noreply, delete_info(PollersInfo, StreamId)}
	end.
	
%% @doc
%% Function: handle_call/2
%% Purpose: the synchronized call used to fetch all data from the elasticsearch and create poller for each stream.
%% Parameters: Request	-- the request sent from the client, actually could be any erlang type.
%%			   _Form	-- {pid(), Tag}, a tuple which contains the client`s pid and a unique tag.
%%			   State	-- the state of the gen_server, here implemented as a list of all pollers` information.
%% Returns: {reply, (reply_message), (new_state_of_gen_server)}
%% Side effects: creates pollers for specific stream
%% @end
-spec handle_call(Request :: term(), _Form :: tuple(), State :: list()) -> {reply, ReturnedMessage :: term(), State :: list()}.
handle_call(create_pollers, _Form, State)->
	%%extract all the streams data from the database
	%%and create poller for each stream.
	
	PollerList = poll_help:json_to_record_streams(poll_help:get_streams_using_polling()),

	create_poller_for_each(PollerList),
	{reply, ok, State};
handle_call({rebuild, StreamId}, _Form, PollersInfo)->
	
	Poller = find_poller_by_id(StreamId, PollersInfo),
	case Poller of
		{error, ErrMessage} -> 
			erlang:display("the error in finding poller: " ++ ErrMessage),
			NewPollersInfo = PollersInfo;
		_ ->
			{update, StreamId, NewUri, NewFreq, NewDataType, NewParser} = gen_server:call(Poller#pollerInfo.pid, {rebuild}),
			{ok,  cancel} = timer:cancel(Poller#pollerInfo.timer_ref),
			{ok, NewTRef} = timer:send_interval((NewFreq*1000), Poller#pollerInfo.pid, {probe}), 
			NewPollersInfo = update_info(PollersInfo, StreamId, NewUri, NewTRef, NewDataType, NewParser)
	end,
	{reply, ok, NewPollersInfo}.

%% @doc
%% Function: terminate/2
%% Purpose: controls what happen when this pulling supervisor stops working.
%% Parameter:	_Reason -- normal | shutdown | {shutdown,term()} | term(), the reason why this gen_server is terminated
%%				_State  -- the state of the gen_server, actually is a list of all pollers` information.
%% Returns: ok
%% @end
-spec terminate(_Reason :: term(), _State :: list()) -> ok.
terminate(_Reason, _State)->
	erlang:display("polling supervisor stops working!").

%% @doc
%% Function: code_change/3
%% Purpose: this funciton will called when the gen_server should update its internal state during a release upgrade/downgrade.
%% Parameter:	_OldVsn -- In the case of upgrade, the _OldVsn is the Vsn, in the case of downgrade, the _OldVsn is {down, Vsn}.  
%%                         Vsn is defined by the vsn attribute(s) of the old version of the callback module Module. If no such attribute is defined, the
%%                         version is the checksum of the BEAM file. 
%%				State   -- The internal state of the gen_server. 
%%				_Extra  -- is passed as-is from the {advanced,Extra} part of the update instruction.
%% Returns: {ok, State}
%% @end
-spec code_change(_OldVsn :: term()|{down, term()}, State :: list(), _Extra :: term()) -> {ok, list()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
			
%% @doc
%% Function: create_poller_for_each/1
%% Purpose: make asynchronous call to polling_supervisor to create poller for each item in polling information list.
%% Returns: ok
%% @end
-spec create_poller_for_each(PollersInfoList :: [record()]) -> ok.
create_poller_for_each([])->ok;
create_poller_for_each([PollerInfo|PollerInfoList])->
	gen_server:cast(polling_supervisor, {create_poller, PollerInfo}),
	create_poller_for_each(PollerInfoList).

%% @doc
%% Function: find_poller_by_id/2
%% Purpose: find one specific poller record in the pollers` information list.
%% Returns: #pollerInfo | {error, ErrMsg}
%% @end
-spec find_poller_by_id(StreamId :: string(), PollerList :: [record()]) -> record() | {error, string()}.
find_poller_by_id(_StreamId, []) -> {error, "id doesn`t exist"};
find_poller_by_id(StreamId, [Poller|Tail]) ->
	case Poller#pollerInfo.stream_id == StreamId of
		true -> Poller;
		_ -> find_poller_by_id(StreamId, Tail)
	end.

%% @doc
%% Function: delete_info/2
%% Purpose: delete a stream record with the stream`s id
%% Returns: NewPollersRecordList | []
%% @end
-spec delete_info(PollersList :: [record()], StreamId :: string()) -> [record()].
delete_info([], _)->[];
delete_info([Poller|Tail], StreamId)->
	case Poller#pollerInfo.stream_id == StreamId of
		true->
			Tail;
		_->
			[Poller|delete_info(Tail, StreamId)]
	end.

%% @doc
%% Function: update_info/6
%% Purpose: after poller done its rebuild, supervisor uses this function to update its pollers` info store.
%% Returns: NewPollersRecordList | []
%% @end
-spec update_info(PollersList :: [record()], StreamId :: string(), NewUri :: string(), NewTRef :: pid(), NewDataType :: string(), NewParser :: string()) -> [record()].
update_info([], _, _, _, _, _) -> [];
update_info([Poller|Tail], StreamId, NewUri, NewTRef, NewDataType, NewParser) ->
	case Poller#pollerInfo.stream_id == StreamId of
		true->
			[Poller#pollerInfo{uri = NewUri, timer_ref = NewTRef, data_type = NewDataType, parser = NewParser} | Tail];
		_ ->
			[Poller | update_info(Tail, StreamId, NewUri, NewTRef, NewDataType, NewParser)]
	end.
