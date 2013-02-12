-module(core_resource).
-export([init/1, allowed_methods/2, post_is_create/2, create_path/2, allow_missing_post/2,
         delete_resource/2, content_types_provided/2, content_types_accepted/2,
         resource_exists/2, to_json/2, from_json/2, process_form/2, generate_etag/2]).

-include_lib("webmachine/include/webmachine.hrl").

%% Webmachine "delegates" 
%%
init([]) -> 
  {ok, undefined}.

allowed_methods(ReqData, State) ->
  {['GET', 'PUT', 'POST', 'DELETE', 'HEAD'], ReqData, State}.

content_types_provided(ReqData, State) ->
  {[{"application/json", to_json}], ReqData, State}.

content_types_accepted(ReqData, State) ->
  {[
    {"application/json", from_json},
    {"application/x-www-form-urlencoded", process_form}
  ], ReqData, State}.

resource_exists(ReqData, State) -> 
  resource_exists(wrq:method(ReqData), ReqData, State).
  
resource_exists('GET', ReqData, State) -> 
  PathInfo = wrq:path_info(ReqData),
  {ok, ResourceName} = dict:find(resource, PathInfo),
  {ok, ResourceKey}  = dict:find(key, PathInfo),
  case get_by_key(ResourceName, ResourceKey) of
    {resource, Resource} ->
      {true, ReqData, {resource, Resource}};
    {error, notfound} ->
      io:format("Not found ~p@~p~n", [ResourceKey, ResourceName]),
      {false, ReqData, State};
    _ ->
      io:format("Unknown error at ~p@~p~n", [ResourceKey, ResourceName]),
      {false, ReqData, State}
  end;

resource_exists(_, ReqData, State) ->
  {true, ReqData, State}.

post_is_create(ReqData, State) ->
  {true, ReqData, State}.
 
create_path(ReqData, State) ->
  Path = integer_to_list(1),
  {Path, ReqData, State}.

allow_missing_post(ReqData, State) ->
  {true, ReqData, State}.

delete_resource(ReqData, State) ->
  ResourceKey   = wrq:path_info(key, ReqData),
  ResourceName  = wrq:path_info(resource, ReqData),
  case delete_by_key(ResourceName, ResourceKey) of
    ok ->
      NewReqData = wrq:set_resp_header("Content-type", "application/json", 
                   wrq:set_resp_body(<<"OK">>, ReqData)),
      {true, NewReqData, State};
    _ ->
      {false, ReqData, State}
  end.

to_json(ReqData, State={resource, Resource}) ->
  {Resource, ReqData, State};

to_json(ReqData, State) ->
  {<<"error">>, ReqData, State}.

process_form(ReqData, State) ->
  process_form(wrq:method(ReqData), ReqData, State).

process_form('POST', ReqData, State) ->
  ResourceName  = wrq:path_info(resource, ReqData),
  ResourceProps = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
  case save_new(ResourceName, ResourceProps) of
    {resource, NewResourceData} ->
      %NewKey = proplists:get_value(key, NewResourceData, undefined),
      %R = wrq:set_resp_header("Location", NewKey, ReqData),
      NewReqData = wrq:set_resp_header("Content-type", "application/json", 
                   wrq:set_resp_body(common_json:to_json(NewResourceData), ReqData)),
      {true, NewReqData, State};
    _ ->
      {false, ReqData, State}
  end;

process_form('PUT', ReqData, State) ->
  ResourceKey   = wrq:path_info(key, ReqData),
  ResourceName  = wrq:path_info(resource, ReqData),
  ResourceProps = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
  ResourceData  = [{key, ResourceKey} | ResourceProps],
  case update_by_key(ResourceName, ResourceKey, ResourceData) of
    {resource, NewResourceData} ->
      %R = wrq:set_resp_header("Location", ResourceKey, ReqData),
      NewReqData = wrq:set_resp_header("Content-type", "application/json", 
                   wrq:set_resp_body(common_json:to_json(NewResourceData), ReqData)),
      {true, NewReqData, State};
    _ ->
      {false, ReqData, State}
  end;

process_form(_, ReqData, State) ->
  {false, ReqData, State}.

from_json(ReqData, State) ->
  {false, ReqData, State}.

generate_etag(ReqData, State) ->
  {resource, Data} = State,
  {mochihex:to_hex(crypto:md5(Data)), ReqData, State}.

% encodings_provided(ReqData, State) ->
%   {[{"gzip", fun(X) -> zlib:gzip(X) end}], ReqData, State}.

%% Handle calls to "core_data" implementation
%%
get_by_key(ResourceName, ResourceKey) ->
  erlang:apply(list_to_atom(string:concat(ResourceName, "_resource")), get, [ResourceKey]).

delete_by_key(ResourceName, ResourceKey) ->
  erlang:apply(list_to_atom(string:concat(ResourceName, "_resource")), delete, [ResourceKey]).

% ResourceData -> proplists
update_by_key(ResourceName, ResourceKey, ResourceData) ->
  erlang:apply(list_to_atom(string:concat(ResourceName, "_resource")), update, 
      [ResourceKey, {resource, ResourceData}]).

save_new(ResourceName, ResourceData) ->
  erlang:apply(list_to_atom(string:concat(ResourceName, "_resource")), save, 
      [{resource, ResourceData}]).
