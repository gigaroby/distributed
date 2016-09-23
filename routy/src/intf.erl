-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() -> [].

add(Name, Ref, Pid, Intf) -> [{Name, Ref, Pid} | Intf].

remove(Name, Intf) ->
	[X || X = {N, _, _} <- Intf, N =/= Name].

lookup(_, []) -> {error, notfound};
lookup(Name, [{N, _, Pid} | _]) when Name =:= N -> {ok, Pid};
lookup(Name, [_ | Rest]) -> lookup(Name, Rest).

ref(_, []) -> {error, notfound};
ref(Name, [{N, Ref, _} | _]) when Name =:= N -> {ok, Ref};
ref(Name, [_ | Rest]) -> ref(Name, Rest).

name(_, []) -> {error, notfound};
name(Ref, [{Name, R, _} | _]) when Ref =:= R -> {ok, Name};
name(Ref, [_ | Rest]) -> name(Ref, Rest).

list(Intf) -> [Name || {Name, _, _} <- Intf].

broadcast(Message, Intf) ->
	[Pid ! Message || {_, _, Pid} <- Intf],
	ok.
