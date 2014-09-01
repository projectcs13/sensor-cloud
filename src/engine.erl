%% @author Georgios Koutsoumpakis, Li Hao
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]

%% @doc engine startup code

-module(engine).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    ensure_started(as1),
    ensure_started(inets),
    ensure_started(ibrowse),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(xmerl),
    ensure_started(compiler),
    ensure_started(syntax_tools),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    engine_sup:start_link().

%% @spec start() -> ok
%% @doc Start the engine server.
start() ->
    ensure_started(crypto),
    ensure_started(asn1),
    ensure_started(inets),
    ensure_started(ibrowse),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(xmerl),
    ensure_started(compiler),
    ensure_started(syntax_tools),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    % analyse:start(),    % possibly temporary solution for ericsson demo
    application:start(engine).

%% @spec stop() -> ok
%% @doc Stop the engine server
stop() ->
    Res = application:stop(engine),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(syntax_tools),
    application:stop(compiler),
    application:stop(xmerl),
    application:stop(ssl),
    application:stop(asn1),
    application:stop(crypto),
    application:stop(public_key),
    application:stop(ibrowse),
    application:stop(inets),
    case whereis(polling_monitor) of
        undefined -> ok;
        P_M_Pid ->
            exit(P_M_Pid, "stop")
    end,
    case whereis(polling_supervisor) of
        undefined -> ok;
        P_S_Pid ->
            exit(P_S_Pid, "stop")
    end,
    case whereis(vstream_sup) of
        undefined -> ok;
        V_S_Pid ->
            exit(V_S_Pid, "stop")
    end,
    Res.
