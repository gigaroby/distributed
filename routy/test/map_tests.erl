-module(map_tests).
-include_lib("eunit/include/eunit.hrl").

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

% check_table(Fn, [{Input, Expected} | Rest]) -> 
%     ?assertEqual(Expected, apply(Fn, Input)), 
%     check_table(Fn, Rest);
% check_table(_, []) -> ok.
% 

assert_equal_no_order(L1, L2) ->
	?PRINT(L1), ?PRINT(L2),
	?assertEqual(lists:sort(L1), lists:sort(L2)).

map_test() ->
	M = map:new(),
	?assertEqual(map:all_nodes(M), []),
	M2 = map:update(london, [paris, warsaw], M),
	assert_equal_no_order(map:all_nodes(M2), [paris, warsaw, london]),
	?assertEqual(map:reachable(paris, M2), []),
	assert_equal_no_order(map:reachable(london, M2), [warsaw, paris]),
	M3 = map:update(london, [paris], M2),
	?assertEqual(map:reachable(london, M3), [paris]),
	assert_equal_no_order(map:all_nodes(M3), [london, paris]).

