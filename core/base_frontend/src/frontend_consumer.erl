-module(frontend_consumer).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_resource/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  InitialState = frontend_conf:get_section(consumer),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ InitialState ], []).

%% 
%% OTP API function to get a resource based on the key.
get_resource(ResourcePath, ResourceKey) ->
  gen_server:call(?SERVER, {get_resource, ResourcePath, ResourceKey}, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ InitialState ]) ->
  {ok, InitialState}.

%%
%% Handling "save_resource" event from API
handle_call({save_resource, _Resource}, _From, State) ->
  % make a http post
  {reply, <<"To be done!">>, State};

%%
%% Handling "get_resource" event from API
handle_call({get_resource, ResourcePath, ResourceKey}, _From, State) ->
  ApiBaseUrl = proplists:get_value(api_base_url, State),
  Url = string:concat(
          string:concat(string:concat(ApiBaseUrl, ResourcePath), "/"), 
            ResourceKey),
  io:format("[debug] Url: ~p~n", [Url]),
  Headers = [],
  {ok, {Status, Header, Body}} = httpc:request(get, {Url, Headers}, [], []),
  io:format("[debug] Result: status=~p headers=~p body=~p~n", [Status, Header, Body]),
  case Status of
    {_, 200, _} ->
      {reply, {resource, Body}, State};
    {_, 404, _} ->
      {reply, notfound, State};
    _ ->
      {reply, error, State}
  end;
  
%%
%% Catch all handle
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

