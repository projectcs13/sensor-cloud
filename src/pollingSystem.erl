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
-include("poller.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, rebuild_poller/1, create_poller/1]).

%% @doc
%% Function: start/0
%% Purpose: init function used to generate the supervisor process and create all pollers.
%% Returns: ok | {error, ErrMsg}
%% Side effects: spawn supervisor process and register it by name supervisor
%% @end
-spec start() -> ok | {error, term()}.
start()->
	case whereis(supervisor) of
		undefined->
			register(supervisor, spawn(server_loop, [[]])),
			create_pollers();
		_ -> 
			{error, "the supervisor has already been started"}
	end.

%% @doc
%% Function: create_poller/1
%% Purpose: create one poller according the poller`s information provided.
%% Returns: {ok, TRef} | {error, ErrMsg}
%% Side effects: make a timer for scheduling, the TRef is the reference to this timer.
%% @end
-spec create_poller(record()) -> {ok, term()} | {error, term()}.
create_poller(#pollerInfo{resourceid = ResourceId, name = ResourceName, url = Url, frequency = Frequency})->
	%% TODO create erlastic function to get all parsers for a specific resource
	Parsers = parser:get_parsers_by_id(ResourceId), %% Non-Existing
	Pid = spawn(poller, poller_loop, [ResourceId, Url, Parsers]),
	Record = #pollerInfo{resourceid=ResourceId, name=ResourceName, url=Url, frequency=Frequency, pid=Pid},
	%%add this poller into the pollers infor storage of the server_loop
	supervisor!{addNewPoller, Record},
	%%use timer library to create scheduler for this poller
	timer:start(),
	timer:send_interval(Frequency, Pid, {probe}).


%% @doc
%% Function: rebuild_poller/1
%% Purpose: the function used to notify the supervisor to rebuild the poller, acccording to the resource`s id.
%% Returns: ok.
%% Side effects: the poller which is specified by resource id will be rebuilt
%% @end
-spec rebuild_poller(string()) -> ok.
rebuild_poller(ResourceId)->
	supervisor!{rebuild, ResourceId},
	ok.


%% ====================================================================
%% Internal functions
%% ====================================================================

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
-spec create_pollers() -> ok.
create_pollers()->
	%%extract all the resources data from the database
	%%and store the information into specific data structure
	
	%%create pollers for each resource and add them into the server_loop.\
				%% [{#pollerInfo{...}}, {#pollerInfo{...}}]
	%% TODO create erlastic function for getting all resources using polling.
	PollerList = streams:get_all_pollers(),
	lists:foreach(create_poller, PollerList)
	%%return the list of pollers` information list
	.


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
-spec server_loop(list()) -> none().
server_loop(PollersInfo)->
	receive
		{addNewPoller, Poller}->
			server_loop([Poller|PollersInfo]);
		{update, ResourceId, NewUrl}->
			server_loop( updateInfo(PollersInfo, ResourceId, NewUrl) );
		{rebuild, ResourceId}->
			%%find pid from the records list according to resource id
			Poller = find_poller_by_id(ResourceId, PollersInfo),
			%%and sends rebuild function to the poller
			case Poller of
				{error, ErrMessage} -> 
					erlang:display(ErrMessage);
				_ ->
					Poller#pollerInfo.pid ! {rebuild}
			end,
			server_loop(PollersInfo)
	end.

