-module(core_data).
-export([fetch/3, save/3, update/4, delete/3]).

%% 
%% API Exported
fetch(RiakPid, ResourceName, Key) ->
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
  Json = resource_caller:call(ResourceName, to_json, [ NewResourceData ]),
  RiakObj = core_riak:create(ResourceName, NewKey, Json),
  ok = core_riak:save(RiakPid, RiakObj),
  {resource, NewResourceData}.

update(RiakPid, ResourceName, ResourceKey, Resource={resource, ResourceData}) ->
  {ok, RiakObj} = core_riak:fetch(RiakPid, ResourceName, ResourceKey),
  Json = resource_caller:call(ResourceName, to_json, [ ResourceData ]),
  NewRiakObj = core_riak:update(RiakObj, Json),
  ok = core_riak:save(RiakPid, NewRiakObj),
  {resource, ResourceData}.

delete(RiakPid, ResourceName, ResourceKey) when is_list(ResourceKey) ->
  core_riak:delete(RiakPid, ResourceName, list_to_binary(ResourceKey));
delete(RiakPid, ResourceName, ResourceKey) when is_binary(ResourceKey) ->
  core_riak:delete(RiakPid, ResourceName, ResourceKey).
