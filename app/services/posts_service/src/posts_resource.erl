-module(posts_resource).
-record(post, {title, text}).
-define(RESOURCE, "posts").

% Basic Operations
-export([get/1, save/1, update/2, delete/1]).

% (de)serialization
-export([to_json/1, from_json/1]).

get(Key) ->
  core_server:get_resource(?RESOURCE, Key).

delete(Key) ->
  core_server:delete_resource(?RESOURCE, Key).

save(Resource) ->
  core_server:save_resource(?RESOURCE, Resource).

update(Key, Resource) ->
  core_server:update_resource(?RESOURCE, Key, Resource).

to_json(Data) ->
  common_json:to_json(Data, fun is_string/1).

from_json(Json) ->
  common_json:from_json(Json, fun is_string/1).

is_string(_) ->
  true.