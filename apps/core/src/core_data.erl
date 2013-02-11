-module(core_data).
-export([fetch_as_proplist/3, fetch_as_json/3]).

%% 
%% API Exported
fetch_as_proplist(RiakPid, ResourceName, Key) ->
  {ok, RiakObj} = core_riak:fetch(RiakPid, ResourceName, Key),
  ResourceJson = core_riak:get_value(RiakObj),
  {resource, from_json_internal(ResourceJson)}.

fetch_as_json(RiakPid, ResourceName, Key) ->
  {ok, RiakObj} = core_riak:fetch(RiakPid, ResourceName, Key),
  ResourceJson = core_riak:get_value(RiakObj),
  {resource, ResourceJson}.

%%
%% helpers used internally
to_json_internal(ResourceData) ->
  common_json:to_json(ResourceData, fun is_string/1).

from_json_internal(ResourceJson) ->
  common_json:from_json(ResourceJson, fun is_string/1).

is_string(_) -> 
  true.