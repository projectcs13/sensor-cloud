%% @author Tholsgård Gabriel, Li Hao
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == pollingSystem ==
%% This module contains all functionalities needed to poll from external resources.
%% We are going to generate one supervisor process and a few poller processes.
%% @end

-module(pollingSystem).
-behaviour(gen_server).
-include("poller.hrl").
-include("state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2]).

%% @doc
%% Function: start_link/0
%% Purpose: init function used to generate the supervisor process, polling_monitor, and create all pollers.
%% Returns: ok | {error, ErrMsg}
%% Side effects: spawn supervisor process and register it by name supervisor. spawn pollingBoss supervisor and register it by its name.
%% @end
-spec start_link() -> atom() | tuple().
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
-spec init(any()) -> tuple().
init(_)->
	{ok, []}.

%% @doc
%% Function: handle_info/2
%% Purpose: handle the messages` processing of the gen_server, accepts two parameters: message and the old state of gen_server.
%%			could be called by: polling_supervisor!{message}
%% Returns: {noreply, NewState}
%% Side effects: send message to specific poller and update the state of gen_server.
%% @end
-spec handle_info(any(), any()) -> tuple().
handle_info({print, Message}, State)->
	erlang:display(Message),
	{noreply, State}.

%% @doc
%% Function: handle_cast/2
%% Purpose: handle asynchronous call of gen_server, accepts two parameters: message of the call and the old state of gen_server.
%%			can be called via: gen_server:cast(polling_supervisor, {rebuild_poller, (stream_id)}).
%% Returns: {noreply, NewState}
%% Side effects: create new poller and send message to specific poller.
%% @end
-spec handle_cast(any(), any()) -> tuple().
handle_cast({create_poller, #pollerInfo{stream_id = StreamId, name = StreamName, uri = Uri, frequency = Frequency}}, PollersInfo)->
	%% erlang:display("receive cast: {create_poller, pollerInfo}"),
	
	Parser = poll_help:get_parser_by_id(StreamId),
	
	{ok, Pid}=supervisor:start_child(polling_monitor, [#state{stream_id=StreamId, uri=Uri, parser=Parser}]),
	Record = #pollerInfo{stream_id=StreamId, name=StreamName, uri=Uri, frequency=Frequency, pid=Pid},
	%%use timer library to create scheduler for this poller
	timer:start(),
	timer:send_interval(Frequency, Pid, {probe}),
	{noreply, [Record|PollersInfo]};
handle_cast({terminate, StreamId}, PollersInfo)->
	%% erlang:display("receive cast: {terminate, ResourceId}"),
	Poller = find_poller_by_id(StreamId, PollersInfo),
	case Poller of
		{error, ErrMsg}->
			erlang:display(ErrMsg),
			{noreply, PollersInfo};
		_ ->
			supervisor:terminate_child(polling_monitor, Poller#pollerInfo.pid),
			supervisor:delete_child(polling_monitor, Poller#pollerInfo.pid),
			{noreply, delete_info(PollersInfo, StreamId)}
	end;
handle_cast({rebuild, StreamId}, PollersInfo)->
	%% erlang:display("receive cast: {rebuild, ResourceId}"),
	
	Poller = find_poller_by_id(StreamId, PollersInfo),
	case Poller of
		{error, ErrMessage} -> 
			erlang:display("the error in finding poller: " ++ ErrMessage),
			NewPollersInfo = PollersInfo;
		_ ->
			{update, StreamId, NewUri} = gen_server:call(Poller#pollerInfo.pid, {rebuild}),
			%%change the url of the poller information stored
			NewPollersInfo = update_info(PollersInfo, StreamId, NewUri)
	end,
	{noreply, NewPollersInfo};
handle_cast({add_new_poller, Poller}, PollersInfo)->
	%% erlang:display("receive cast: {add_new_poller, Poller}"),
	{noreply, [Poller|PollersInfo]};
handle_cast({update, StreamId, NewUri}, PollersInfo)->
	%% erlang:display("receive cast: {update, ResourceId, NewUrl}"),
	{noreply, update_info(PollersInfo, StreamId, NewUri)}.
	
%% @doc
%% Function: handle_call/2
%% Purpose: init function used to generate the supervisor process, polling monitor, and create all pollers.
%% Returns: {reply, (reply_message), (new_state_of_gen_server)}
%% Side effects: creates pollers for specific stream
%% @end
-spec handle_call(atom(), tuple(), any()) -> tuple().
handle_call(create_pollers, _Form, State)->
	%% erlang:display("receive request: create_pollers"),
	%%extract all the streams data from the database
	%%and store the information into specific data structure
	
	PollerList = poll_help:json_to_record_streams(poll_help:get_streams_using_polling()),

	create_poller_for_each(PollerList),
	{reply, ok, State}.

%% @doc
%% Function: terminate/2
%% Purpose: controls what happen when this pulling supervisor stops working!.
%% Returns: ok
%% @end
-spec terminate(tuple(), any()) -> atom().
terminate(_T, _State)->
	erlang:display("polling supervisor stops working!").


%% ====================================================================
%% Internal functions
%% ====================================================================
			
%% @doc
%% Function: create_poller_for_each/1
%% Purpose: make asynchronous call to polling_supervisor to create poller for each item in polling information list.
%% Returns: ok
%% @end
-spec create_poller_for_each(list(tuple())) -> atom().
create_poller_for_each([])->ok;
create_poller_for_each([PollerInfo|PollerInfoList])->
	gen_server:cast(polling_supervisor, {create_poller, PollerInfo}),
	create_poller_for_each(PollerInfoList).

%% @doc
%% Function: find_poller_by_id/2
%% Purpose: find one specific poller record in the pollers` information list.
%% Returns: #pollerInfo | {error, ErrMsg}
%% @end
-spec find_poller_by_id(integer(), list(tuple())) -> term() | {error, string()}.
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
-spec delete_info(list(tuple()), integer()) -> list().
delete_info([], _)->[];
delete_info([Poller|Tail], StreamId)->
	case Poller#pollerInfo.stream_id == StreamId of
		true->
			Tail;
		_->
			[Poller|delete_info(Tail, StreamId)]
	end.

%% @doc
%% Function: update_info/3
%% Purpose: after poller done its rebuild, supervisor uses this function to update its pollers` info store.
%% Returns: NewPollersRecordList | []
%% @end
-spec update_info(list(tuple()), integer(), string()) -> list().
update_info([], _, _) -> [];
update_info([Poller|Tail], StreamId, NewUri) ->
	case Poller#pollerInfo.stream_id == StreamId of
		true->
			[Poller#pollerInfo{uri = NewUri} | Tail];
		_ ->
			[Poller | update_info(Tail, StreamId, NewUri)]
	end.

