-module(product_resource).
-export([get/1, save/2]).

%%
%% get(Key) -> {resource, ResourceJson}|{error, notfound}
get(Key) ->
  core_server:get_resource("product", Key).

save(Key, Resource) ->
  core_server:save_resource("product", Resource).