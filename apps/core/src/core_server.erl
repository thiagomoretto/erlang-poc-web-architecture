-module(core_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_resource/2, save_resource/2, update_resource/3,delete_resource/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% 
%% OTP API function to get a resource based on the key.
get_resource(ResourcePath, ResourceKey) ->
  gen_server:call(?SERVER, {get_resource, ResourcePath, ResourceKey}, infinity).

%% 
%% OTP API function to get a resource based on the key.
delete_resource(ResourcePath, ResourceKey) ->
  gen_server:call(?SERVER, {delete_resource, ResourcePath, ResourceKey}, infinity).

%% 
%% OTP API function to save a new resource.
save_resource(ResourcePath, ResourceData) ->
  gen_server:call(?SERVER, {save_resource, ResourcePath, ResourceData}, infinity).

%% 
%% OTP API function to update a resource based on the key.
update_resource(ResourcePath, ResourceKey, ResourceData) ->
  gen_server:call(?SERVER, {update_resource, ResourcePath, ResourceKey, ResourceData}, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

%%
%% Handling "get_resource" event from API
handle_call({get_resource, ResourcePath, ResourceKey}, _From, State) ->
  ResourceJson = pooler:use_member(
    fun(RiakPid) ->
      core_data:fetch(RiakPid, ResourcePath, ResourceKey) end),
  {reply, ResourceJson, State};

handle_call({delete_resource, ResourcePath, ResourceKey}, _From, State) ->
  ResourceJson = pooler:use_member(
    fun(RiakPid) ->
      core_data:delete(RiakPid, ResourcePath, ResourceKey) end),
  {reply, ResourceJson, State};

handle_call({save_resource, ResourcePath, ResourceData}, _From, State) ->
  SavedResource = pooler:use_member(
    fun(RiakPid) ->
      core_data:save(RiakPid, ResourcePath, ResourceData) end),
  {reply, SavedResource, State};

handle_call({update_resource, ResourcePath, ResourceKey, ResourceData}, _From, State) ->
  SavedResource = pooler:use_member(
    fun(RiakPid) ->
      core_data:update(RiakPid, ResourcePath, ResourceKey, ResourceData) end),
  {reply, SavedResource, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

