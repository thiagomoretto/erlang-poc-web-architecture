-module(core_root_resource).
-export([init/1, content_types_provided/2, resource_exists/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
  {ok, undefined}.

content_types_provided(ReqData, State) ->
  {[{"application/json", to_json}], ReqData, State}.

resource_exists(ReqData, State) -> 
  PathInfo = wrq:path_info(ReqData),
  {ok, ResourceName} = dict:find(resource, PathInfo),
  {ok, ResourceKey} = dict:find(key, PathInfo),
  case erlang:apply(list_to_atom(string:concat(ResourceName, "_resource")), get, [ResourceKey]) of
    {resource, Resource} ->
      {true, ReqData, {resource, Resource}};
    {error, notfound} ->
      io:format("Not found ~p@~p~n", [ResourceKey, ResourceName]),
      {false, ReqData, State};
    _ ->
      io:format("Unknown error at ~p@~p~n", [ResourceKey, ResourceName]),
      {false, ReqData, State}
  end.

to_json(ReqData, State={resource, Resource}) ->
  {Resource, ReqData, State};

to_json(ReqData, State) ->
  {<<"error">>, ReqData, State}.