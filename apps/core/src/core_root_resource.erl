-module(core_root_resource).
-export([init/1, content_types_provided/2, resource_exists/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
  {ok, undefined}.

content_types_provided(ReqData, State) ->
  {[{"application/json", to_json}], ReqData, State}.

resource_exists(ReqData, State) -> 
  {true, ReqData, State}.

to_json(ReqData, State) ->
  PathInfo = wrq:path_info(ReqData),
  {ok, ResourceName} = dict:find(resource, PathInfo),
  {ok, ResourceKey} = dict:find(key, PathInfo),
  {resource, ResourceJson} = core_server:get_resource(ResourceName, ResourceKey),
  {ResourceJson, ReqData, State}.