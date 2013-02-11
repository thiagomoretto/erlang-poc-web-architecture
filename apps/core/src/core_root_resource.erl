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
  {mochijson:encode({struct, [{name, "foobar"}]}), ReqData, State}.
