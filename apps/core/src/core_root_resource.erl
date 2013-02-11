-module(core_root_resource).
-export([init/1, allowed_methods/2, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
  {ok, undefined}.

allowed_methods(ReqData, State) ->
  {['GET', 'HEAD'], ReqData, State}.

content_types_provided(ReqData, State) ->
  {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
  {<<"OK">>, ReqData, State}.