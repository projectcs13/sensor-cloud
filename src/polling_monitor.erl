%% @author Li Hao

%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == polling_monitor ==
%% this module implements a supervisor of pollers, when one poller crashes for some reason, this monitor could restart it 
%% automaticaly.
%% @end

%% more information about supervisor framework
%% could be seen here: http://learnyousomeerlang.com/supervisors
%%					   http://www.erlang.org/doc/man/supervisor.html

-module(polling_monitor).
-include("state.hrl").
-behaviour(supervisor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, init/1]).

%% @doc
%% Function: start_link/0
%% Purpose: start function used to generate the polling_monitor process, and will call init/1 function to initialize.
%% Returns: {already_started, pid()} | {shutdown, term()} | {ok, pid()}
%% @end
-spec start_link() -> {ok, pid()} | {already_started, pid()} | {shutdown, term()}.
start_link()->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc
%% Function: init/1
%% Purpose: init function used to initialize the polling_monitor, will called by supervisor:start_link()
%% Returns: {ok, {configuration of the supervisor, specifications of the children}}
%% @end
-spec init(term()) -> {ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore when
		  RestartStrategy :: term(),
		  MaxR :: integer(),
		  MaxT :: integer(),
		  ChildSpec :: tuple().
init(_)->
	{ok, {{simple_one_for_one, 5, 60},
		  [{poller, {poller, start_link, []}, transient, 1000, worker, [poller]}]
		 }}.