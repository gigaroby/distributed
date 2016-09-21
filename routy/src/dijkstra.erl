-module(dijkstra).
-export([entry/2, replace/4, update/4, iterate/3, table/2]).

entry(_, []) ->
	0;
entry(Node, [{Dest, Distance, _} | Rest]) ->
	case Dest of
		Node -> Distance;
		_ -> entry(Node, Rest)
	end.

sort_entries({_, D1, _}, {_, D2, _}) when D1 =< D2 -> true;
sort_entries(_, _) -> false.

replace(Node, N, Gateway, Sorted) ->
	replace(Node, N, Gateway, Sorted, []).

replace(_, _, _, [], _) ->
	{error, notfound};
replace(Node, N, Gateway, [Current = {Dest, _, _} | Rest], Acc) ->
	case Dest of
		Node ->
			{ok, lists:sort(fun sort_entries/2, Acc ++ [{Node, N, Gateway} | Rest])};
		_ ->
			replace(Node, N, Gateway, Rest, [Current | Acc])
	end.

update(Node, N, Gateway, Sorted) ->
	case entry(Node, Sorted) of
		M when M > N ->
			{ok, New} = replace(Node, N, Gateway, Sorted), New;
		_ -> Sorted
	end.

iterate([], _, Table) -> Table;
iterate([{_, inf, _} | _], _, Table) -> Table;
iterate([{Node, N, Gw} | Rest], Map, Table) ->
	NewTable = [{Node, Gw} | Table],
	NewSorted = lists:foldl(
					fun(Neighbor, CurrentSorted) -> update(Neighbor, N+1, Gw, CurrentSorted) end,
					Rest,
					map:reachable(Node, Map)),
	iterate(NewSorted, Map, NewTable).

contains(_, []) -> false;
contains(Elem, [Current | _]) when Current =:= Elem -> true;
contains(Elem, [_ | Rest]) -> contains(Elem, Rest).

table(Gateways, Map) ->
	GwTable = [{X, 0, X} || X <- Gateways],
	TempTable = [{X, inf, unknown} || X <- map:all_nodes(Map), not contains(X, Gateways)],
	iterate(GwTable ++ TempTable, Map, []).
