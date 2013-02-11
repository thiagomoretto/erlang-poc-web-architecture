-module(core_data).
-export([fetch_as_proplist/3, fetch_as_json/3, save/4]).

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

save(RiakPid, ResourceName, Key, Resource={resource, ResourceData}) ->
  case proplists:get_value(key, ResourceData, undefined) of
    undefined ->
      NewResourceData = [{key, Key} | ResourceData],
      RiakObj = core_riak:create(ResourceName, Key, to_json_internal(NewResourceData)),
      ok = core_riak:save(RiakPid, RiakObj),
      {resource, NewResourceData};
    ExistingKey ->
      RiakObj = core_riak:fetch(RiakPid, ResourceName, ExistingKey),
      NewRiakObj = core_riak:update(RiakObj, to_json_internal(ResourceData)),
      ok = core_riak:save(RiakPid, NewRiakObj),
      {resource, Resource}
  end.

%%
%% helpers used internally
to_json_internal(ResourceData) ->
  common_json:to_json(ResourceData, fun is_string/1).

from_json_internal(ResourceJson) ->
  common_json:from_json(ResourceJson, fun is_string/1).

is_string(_) -> 
  true.