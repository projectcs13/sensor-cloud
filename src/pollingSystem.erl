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

%%this function only does some initialization of the testing data
test()->
    inets:start(),
    %%insert a new resource
    httpc:request(post, {"http://localhost:9200/sensorcloud/resource", [], "application/json", "{\"user_id\":1, \"name\":\"testing\", \"tags\":\"\", \"description\":\"the temperature in uppsala\", \"type\":\"application/json\", \"manufacturer\":\"volvo\", \"streams\":\"1\", \"uri\":\"http://130.238.15.199:8000/cgi-bin/resource.py\", \"polling_freq\":1000, \"creation_date\":\"\"} "}, [], []),
    
    %%insert two new parsers
    httpc:request(post, {"http://localhost:9200/sensorcloud/parser", [], "application/json", "{\"resource_id\":1, \"stream_id\":17, \"input_parser\":\"streams/temperature/value\", \"input_type\":\"application/json\" }"}, [], []),
    httpc:request(post, {"http://localhost:9200/sensorcloud/parser", [], "application/json", "{\"resource_id\":1, \"stream_id\":17, \"input_parser\":\"streams/humidity/value\", \"input_type\":\"application/json\" }"}, [], []),
    
    application:stop(inets).

%% @doc
%% Function: start_link/0
%% Purpose: init function used to generate the supervisor process, polling_monitor, and create all pollers.
%% Returns: ok | {error, ErrMsg}
%% Side effects: spawn supervisor process and register it by name supervisor. spawn pollingBoss supervisor and register it by its name.
%% @end
start_link()->
	%%for testing
	erlastic_search_app:start(),
	timer:sleep(1000),
	%%test(),
	%%testing ends
	
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
init(_)->
	{ok, []}.

%% @doc
%% Function: handle_info/2
%% Purpose: handle the messages` processing of the gen_server, accepts two parameters: message and the old state of gen_server.
%%			could be called by: polling_supervisor!{message}
%% Returns: {noreply, NewState}
%% Side effects: send message to specific poller and update the state of gen_server.
%% @end
handle_info({addNewPoller, Poller}, PollersInfo)->
	erlang:display("receive info: {addNewPoller, Poller}"),
	{noreply, [Poller|PollersInfo]};
handle_info({update, ResourceId, NewUrl}, PollersInfo)->
	erlang:display("receive info: {update, ResourceId, NewUrl}"),
	{noreply, updateInfo(PollersInfo, ResourceId, NewUrl)};
handle_info({rebuild, ResourceId}, PollersInfo)->
	erlang:display("receive info: {rebuild, ResourceId}"),
	%%find pid from the records list according to resource id
	Poller = find_poller_by_id(ResourceId, PollersInfo),
	%%and sends rebuild function to the poller
	case Poller of
		{error, ErrMessage} -> 
			erlang:display(ErrMessage);
		_ ->
			gen_server:call(Poller#pollerInfo.pid, {rebuild})
	end,
	{noreply, PollersInfo}.

%% @doc
%% Function: handle_cast/2
%% Purpose: handle asynchronous call of gen_server, accepts two parameters: message of the call and the old state of gen_server.
%%			can be called via: gen_server:cast(polling_supervisor, {rebuild_poller, (resource_id)}).
%% Returns: {noreply, NewState}
%% Side effects: create new poller and send message to specific poller.
%% @end
handle_cast({create_poller, #pollerInfo{resourceid = ResourceId, name = ResourceName, url = Url, frequency = Frequency}}, State)->
	erlang:display("receive cast: {create_poller, pollerInfo}"),
	
	%% TODO create erlastic function to get all parsers for a specific resource
	Parsers = parser:getParsersById(ResourceId),
	%%generate a new poller to the pollingBoss

	%%only for testing
	case whereis(polling_monitor) of
		undefined->
			erlang:display("polling monitor is not running");
		_ ->
			continue
	end,
	
	{ok, Pid}=supervisor:start_child(polling_monitor, [#state{resourceid=ResourceId, url=Url, parserslist=Parsers}]),
	Record = #pollerInfo{resourceid=ResourceId, name=ResourceName, url=Url, frequency=Frequency, pid=Pid},
	%%add this poller into the pollers infor storage of the server_loop
	%%polling_supervisor!{addNewPoller, Record},
	%%use timer library to create scheduler for this poller
	timer:start(),
	timer:send_interval(Frequency, Pid, {probe}),
	{noreply, [Record|State]};
handle_cast({rebuild_poller, ResourceId}, State)->
	erlang:display("receive cast: {rebuild_poller, ResourceId}"),
	
	polling_supervisor!{rebuild, ResourceId},
	{noreply, State}.
	
%% @doc
%% Function: handle_call/2
%% Purpose: init function used to generate the supervisor process, pollingBoss supervisor, and create all pollers.
%% Returns: {reply, (reply_message), (new_state_of_gen_server)}
%% Side effects: creates pollers for specific resource
%% @end
handle_call(create_pollers, _Form, State)->
	erlang:display("receive request: create_pollers"),
	%%extract all the resources data from the database
	%%and store the information into specific data structure
	
	%%create pollers for each resource and add them into the server_loop.\
				%% [{#pollerInfo{...}}, {#pollerInfo{...}}]
	%% TODO create erlastic function for getting all resources using polling.
	
	%%PollerList = streams:get_all_pollers(),
	PollerList = [#pollerInfo{resourceid=1, name="temperature", url="http://130.238.15.199:8000/cgi-bin/resource.py", frequency=13}],
	
	%%lists:foreach(create_poller, PollerList),
	create_poller_for_each(PollerList),
	%%return the list of pollers` information list
	{reply, ok, State}.

%% @doc
%% Function: terminate/2
%% Purpose: controls what happen when this pulling supervisor stops working!.
%% Returns: ok
%% @end
terminate(_T, State)->
	erlang:display("polling supervisor stops working!").


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Function: create_poller_for_each/1
%% Purpose: make asynchronous call to polling_supervisor to create poller for each item in polling information list.
%% Returns: ok
%% @end
create_poller_for_each([])->ok;
create_poller_for_each([PollerInfo|PollerInfoList])->
	gen_server:cast(polling_supervisor, {create_poller, PollerInfo}),
	create_poller_for_each(PollerInfoList).

%% @doc
%% Function: find_poller_by_id/2
%% Purpose: find one specific poller record in the pollers` information list.
%% Returns: #pollerInfo | {error, ErrMsg}
%% @end
-spec find_poller_by_id(integer(), list()) -> term() | {error, string()}.
find_poller_by_id(_ResourceId, []) -> {error, "id doesn`t exist"};
find_poller_by_id(ResourceId, [Poller|Tail]) ->
	case Poller#pollerInfo.resourceid == ResourceId of
		true -> Poller;
		_ -> find_poller_by_id(ResourceId, Tail)
	end.

%% @doc
%% Function: create_pollers/0
%% Purpose: function used to extract all pollers from the datastore and create every poller for each of them.
%% Returns: ok
%% @end
%%-spec create_pollers() -> ok.
%%create_pollers()->
	%%extract all the resources data from the database
	%%and store the information into specific data structure
	
	%%create pollers for each resource and add them into the server_loop.\
				%% [{#pollerInfo{...}}, {#pollerInfo{...}}]
	%% TODO create erlastic function for getting all resources using polling.
%%	PollerList = streams:get_all_pollers(),
%%	lists:foreach(create_poller, PollerList)
	%%return the list of pollers` information list
%%	.


%% @doc
%% Function: updateInfo/3
%% Purpose: after poller done its rebuild, supervisor uses this function to update its pollers` info store.
%% Returns: NewPollersRecordList | []
%% @end
-spec updateInfo(list(), integer(), string()) -> list().
updateInfo([], _, _) -> [];
updateInfo([Poller|Tail], ResourceId, NewUrl) ->
	case Poller#pollerInfo.resourceid == ResourceId of
		true->
			[Poller#pollerInfo{url = NewUrl} | Tail];
		_ ->
			[Poller | updateInfo(Tail, ResourceId, NewUrl)]
	end.


%% @doc
%% Function: server_loop/1
%% Purpose: the main loop of supervisor to process the coming messages
%% @end
%%-spec server_loop(list()) -> none().
%%server_loop(PollersInfo)->
%%	receive
%%		{addNewPoller, Poller}->
%%			server_loop([Poller|PollersInfo]);
%%		{update, ResourceId, NewUrl}->
%%			server_loop( updateInfo(PollersInfo, ResourceId, NewUrl) );
%%		{rebuild, ResourceId}->
%%			%%find pid from the records list according to resource id
%%			Poller = find_poller_by_id(ResourceId, PollersInfo),
%%			%%and sends rebuild function to the poller
%%			case Poller of
%%				{error, ErrMessage} -> 
%%					erlang:display(ErrMessage);
%%				_ ->
%%					Poller#pollerInfo.pid ! {rebuild}
%%			end,
%%			server_loop(PollersInfo)
%%	end.

