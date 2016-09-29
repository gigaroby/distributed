-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() -> 0.

inc(_Name, T) -> 
	T+1.

merge(T1, T2) ->
	max(T1, T2) + 1.

leq(T1, T2) ->
	T1 =< T2.

clock(Nodes) ->
	dict:from_list([{Node, zero()} || Node <- Nodes]).

update(Node, Time, Clock) ->
	dict:store(Node, Time, Clock).

safe(Time, Clock) ->
	lists:all(fun({_, T}) -> leq(Time, T) end, dict:to_list(Clock)).
