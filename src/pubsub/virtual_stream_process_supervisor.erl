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
-export([]).



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
	%% TODO Get all virtual streams in a list
    AChild = {'AName',{'AModule',start_link,[]},
	      permanent,2000,worker,['AModule']},
    {ok,{{simple_one_for_one,5,60}, [AChild]}}.

%% SupervisionPolicy: {simple_one_for_one, 5, 60}  -- simple_one_for_one because it can create children dynamically but kills are harder.
%% ChildSpec: {VStreamId, {virtual_stream_process, start_link, []}, transient, brutal_kill, worker, [virtual_stream_process]}.

%% ====================================================================
%% Internal functions
%% ====================================================================


