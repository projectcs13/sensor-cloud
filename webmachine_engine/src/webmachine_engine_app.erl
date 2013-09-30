%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the webmachine_engine application.

-module(webmachine_engine_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for webmachine_engine.
start(_Type, _StartArgs) ->
    webmachine_engine_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for webmachine_engine.
stop(_State) ->
    ok.
