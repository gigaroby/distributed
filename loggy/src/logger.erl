-module(logger).
-export([start/1, stop/1]).

start(Nodes) ->
	spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

init(Nodes) ->
	loop([], time:clock(Nodes)).


safe_to_print(Holdback, Clock) ->
	Sorted = lists:sort(fun({_, T1, _}, {_, T2, _}) -> time:leq(T1, T2) end, Holdback),
	lists:partition(fun({_, Time, _}) -> time:safe(Time, Clock) end, Sorted).


loop(Holdback, Clock) ->
	receive
		{log, From, Time, Msg} ->
			NewClock = time:update(From, Time, Clock),
			{Safe, NewHoldback} = safe_to_print([{From, Time, Msg} | Holdback], NewClock),
			[log(F, T, M) || {F, T, M} <- Safe],
			loop(NewHoldback, NewClock);
		stop ->
			ok
	end.

log(From, Time, Msg) ->
	io:format("log: ~w ~w ~p~n", [Time, From, Msg]),
	ok.
