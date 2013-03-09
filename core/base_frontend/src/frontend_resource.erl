-module(frontend_resource).
-export([init/1, to_html/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

resource_exists(ReqData, State) -> 
  PathInfo = wrq:path_info(ReqData),
  {ok, Template} = dict:find(template, PathInfo),
  %% TODO: Fix that
  TemplateExists = erlang:function_exported(list_to_atom(string:concat(Template, "_dtl")), render, 1),
  {true, ReqData, State}.

to_html(ReqData, State) ->
  PathInfo = wrq:path_info(ReqData),
  {ok, ResourcePath} = dict:find(path, PathInfo),
  {ok, ResourceKey} = dict:find(key, PathInfo),
  {ok, Template} = dict:find(template, PathInfo),
  Resource = frontend_consumer:get_resource(ResourcePath, ResourceKey),
  render_resource(ReqData, State, Template, ResourcePath, Resource).

%%
%% Render resource
render_resource(ReqData, State, Template, ResourcePath, {resource, ResourceData}) ->
  Params = [{ erlang:list_to_atom(ResourcePath), from_json_internal(ResourceData) }],
  {ok, Content} = erlang:apply(list_to_atom(string:concat(Template, "_dtl")), render, [Params]),
  {Content, ReqData, State};

render_resource(ReqData, State, Template, ResourcePath, _) ->
  Params = [],
  {ok, Content} = erlang:apply(list_to_atom(string:concat(Template, "_dtl")), render, [Params]),
  {Content, ReqData, State}. 

%% 
%% Internal utility functions
from_json_internal(Json) ->
  common_json:from_json(Json, fun is_string/1).

is_string(_) -> 
  true.