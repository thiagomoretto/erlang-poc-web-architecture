-module(product_resource).
-export([get/1, save/1, update/2, delete/1]).

%%
%% get(Key) -> {resource, ResourceJson}|{error, notfound}
get(Key) ->
  core_server:get_resource("product", Key).

delete(Key) ->
  core_server:delete_resource("product", Key).

save(Resource) ->
  % apply some logic here and save
  core_server:save_resource("product", Resource).

update(Key, Resource) ->
  % apply some logic here and update
  core_server:update_resource("product", Key, Resource).