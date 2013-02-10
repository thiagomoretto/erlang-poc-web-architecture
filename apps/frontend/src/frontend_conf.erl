-module(frontend_conf).

-export([get_section/1, get_section/2]).
-export([get_val/2, get_val/3]).

get_section(Name) ->
  get_section(Name, undefined).

get_section(Name, Default) ->
  case application:get_env(frontend, Name) of
    {ok, V} ->
      V;
    _ ->
      Default
  end.

get_val(SectionName, Name) ->
  get_val(SectionName, Name, undefined).

get_val(SectionName, Name, Default) ->
  case application:get_env(frontend, SectionName) of
    {ok, Section} ->
      proplists:get_value(Name, Section, Default);
    _ ->
      Default
  end.