-module(key).
-export([generate/0, between/3]).

generate() ->
  rand:uniform(1, 1000000).

between(_, From, To) when From =:= To ->
   true;
between(Key, From, To) when From < To ->
  Key > From andalso Key =< To;
between(Key, From, To) ->
  Key > From orelse Key =< To.
