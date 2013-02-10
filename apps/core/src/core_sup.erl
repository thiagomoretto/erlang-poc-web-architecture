-module(core_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, upgrade/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
  {ok, {_, Specs}} = init([]),
  Old = sets:from_list(
          [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
  New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
  Kill = sets:subtract(Old, New),

  sets:fold(fun (Id, ok) ->
                    supervisor:terminate_child(?MODULE, Id),
                    supervisor:delete_child(?MODULE, Id),
                    ok
            end, ok, Kill),
  [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
  ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  % Web process that exposes Rest/API interface
  WebConfig = core_conf:get_section(web),
  WebServer = {webmachine_mochiweb,
    {webmachine_mochiweb, start, [WebConfig]},
    permanent, 5000, worker, dynamic},

  % Internal gen_server worker to handle api calls
  InternalServer = ?CHILD(core_server, worker),
  Processes = [ InternalServer, WebServer ],
  {ok, { {one_for_one, 5, 10}, Processes} }.

