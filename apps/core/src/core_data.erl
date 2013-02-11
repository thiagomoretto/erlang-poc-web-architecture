-module(core_data).
-export([fetch_as_proplist/3, fetch_as_json/3, save/3, update/4, delete/3]).

%% 
%% API Exported
fetch_as_proplist(RiakPid, ResourceName, Key) ->
  {ok, RiakObj} = core_riak:fetch(RiakPid, ResourceName, Key),
  ResourceJson = core_riak:get_value(RiakObj),
  {resource, from_json_internal(ResourceJson)}.

fetch_as_json(RiakPid, ResourceName, Key) ->
  case core_riak:fetch(RiakPid, ResourceName, Key) of
    {ok, RiakObj} ->
      core_riak:fetch(RiakPid, ResourceName, Key),
      ResourceJson = core_riak:get_value(RiakObj),
      {resource, ResourceJson};
    {error, notfound} ->
      {error, notfound}
  end.

save(RiakPid, ResourceName, Resource={resource, ResourceData}) ->
  NewKey = core_riak:new_key(),
  NewResourceData = [{key, NewKey} | ResourceData],
  
  io:format("[debug] new resource data: ~p json: ~p~n", 
      [ NewResourceData, to_json_internal(NewResourceData) ]),

  RiakObj = core_riak:create(ResourceName, NewKey, to_json_internal(NewResourceData)),
  ok = core_riak:save(RiakPid, RiakObj),
  {resource, NewResourceData}.

%  case proplists:get_value(key, ResourceData, undefined) of
update(RiakPid, ResourceName, ResourceKey, Resource={resource, ResourceData}) ->
  {ok, RiakObj} = core_riak:fetch(RiakPid, ResourceName, ResourceKey),
  io:format("[debug] resource data: ~p json: ~p~n", 
    [ ResourceData, to_json_internal(ResourceData) ]),
  NewRiakObj = core_riak:update(RiakObj, to_json_internal(ResourceData)),
  io:format("[debug] new riak obj ~p~n", [NewRiakObj]),
  ok = core_riak:save(RiakPid, NewRiakObj),
  {resource, ResourceData}.

delete(RiakPid, ResourceName, ResourceKey) when is_list(ResourceKey) ->
  core_riak:delete(RiakPid, ResourceName, list_to_binary(ResourceKey));
delete(RiakPid, ResourceName, ResourceKey) when is_binary(ResourceKey) ->
  core_riak:delete(RiakPid, ResourceName, ResourceKey).

%%
%% helpers used internally
to_json_internal(ResourceData) ->
  common_json:to_json(ResourceData, fun is_string/1).

from_json_internal(ResourceJson) ->
  common_json:from_json(ResourceJson, fun is_string/1).

is_string(_) -> 
  true.