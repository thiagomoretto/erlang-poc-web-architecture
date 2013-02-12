-module(resource_caller).
-export([call/3]).

call(ResName, FunctionName, Params) ->
  erlang:apply(
    list_to_atom(string:concat(ResName, "_resource")), FunctionName, Params).