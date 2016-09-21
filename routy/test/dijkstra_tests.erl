-module(dijkstra_tests).
-include_lib("eunit/include/eunit.hrl").

check_table(Fn, [{Input, Expected} | Rest]) -> 
    ?assertEqual(Expected, apply(Fn, Input)), 
    check_table(Fn, Rest);
check_table(_, []) -> ok.

update_test() ->
	Table = [
		{[london, 2, amsterdam, []], % input
		 []},						 % output
		{[london, 2, amsterdam, [{london, 2, paris}]],
		 [{london, 2, paris}]},
		{[london, 1, stockholm, [{berlin, 2, paris}, {london, 3, paris}]],
		 [{london,1,stockholm}, {berlin, 2, paris}]}
	],
	check_table(fun dijkstra:update/4, Table).

iterate_test() ->
	M = map:update(paris, [berlin], map:new()),
	Table = [
		{[[{paris, 0, paris}, {berlin, inf, unknown}], M, []], % inputs
		 [{berlin, paris},{paris,paris}]}					   % outputs
	],
	check_table(fun dijkstra:iterate/3, Table).

table_test() ->
	M = map:update(madrid, [berlin], map:update(paris, [rome, madrid], map:new())),
	Table = [
		{[[paris, madrid], M],										   % inputs
		 [{berlin,madrid},{rome,paris},{madrid,madrid},{paris,paris}]} % outputs
	],
	check_table(fun dijkstra:table/2, Table).
