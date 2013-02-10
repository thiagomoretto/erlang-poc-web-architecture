-module(core).
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
  start_common(),
  core_sup:start_link().

%% @spec start() -> ok
%% @doc Start the core server.
start() ->
  start_common(),
  application:start(core).

%% @spec stop() -> ok
%% @doc Stop the core server.
stop() ->
  Res = application:stop(core),
  application:stop(crypto),
  application:stop(pooler),
  application:stop(webmachine),
  application:stop(mochiweb),
  application:stop(crypto),
  application:stop(inets),
  Res.

%%
%% @private
start_common() ->
  ensure_started(crypto),
  ensure_started(pooler),
  ensure_started(inets),
  ensure_started(crypto),
  ensure_started(mochiweb),
  application:set_env(webmachine, 
                      webmachine_logger_module, 
                      webmachine_logger),
  ensure_started(webmachine).