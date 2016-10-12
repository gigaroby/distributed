-module(key_tests).
-include_lib("eunit/include/eunit.hrl").

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

between_test() ->
  ?assertEqual(key:between(22, 1, 1), true),
  ?assertEqual(key:between(22, 1, 2), false),
  ?assertEqual(key:between(22, 2, 1), true).
