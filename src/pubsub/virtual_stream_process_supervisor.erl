%% @author Gabriel Tholsgård <gath5951@student.uu.se>
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == virtual_stream_process_supervisor ==
%% This module represents a virtual stream process supervisor which
%% initializes all virtual stream processes and puts them under its
%% supervision. 
%%
%% @end

-module(virtual_stream_process_supervisor).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).




%% start_link/0
%% ====================================================================
%% @doc
%% Function: start_link/0
%% Purpose: Initializes as a supervisor process for virtual streams.
%% Return: {ok, Pid} | ignore | {error, {already_started, Pid}} |
%%		   {error, {shutdown, Reason}} | {error, Reason}.
%% Side effect: Starts a supervisor process for virtual streams.
%% @end
-spec start_link() -> Startlink_ret when
	Startlink_ret :: {ok, pid()}
	               | ignore
	               | {error, Startlink_err},
	Startlink_err :: {already_started, pid()}
    	           | {shutdown, term()}
        	       | term().
%% ====================================================================
start_link() ->
	supervisor:start_link({local, vstream_sup}, ?MODULE, []).





%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    {ok,{{simple_one_for_one,5,60},
    	[{virtual_stream_process,
    	  {gen_virtual_stream_process, start_link, []},
    	  transient, brutal_kill, worker, [gen_virtual_stream_process]}
		]}}.

%% SupervisionPolicy: {simple_one_for_one, 5, 60}  -- simple_one_for_one because it can create children dynamically but kills are harder.
%% ChildSpec: {VStreamId, {gen_virtual_stream_process, start_link, []}, transient, brutal_kill, worker, [gen_virtual_stream_process]}.

%% ====================================================================
%% Internal functions
%% ====================================================================


