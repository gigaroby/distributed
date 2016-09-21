-module(router).
-export([start/2, stop/1, init/1]).

start(Reg, Name) ->
	register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
	Node ! stop,
	unregister(Node).

init(Name) ->
	Intf = intf:new(),
	Map = map:new(),
	Table = dijkstra:table(Intf, Map),
	Hist = history:new(Name),
	router(Name, 0, Hist, Intf, Table, Map). 

router(Name, N, Hist, Intf, Table, Map) ->
	receive
		{add, Node, Pid} ->
			Ref = erlang:monitor(process, Pid),
			NewIntf = intf:add(Node, Ref, Pid, Intf),
			router(Name, N, Hist, NewIntf, Table, Map);
		{remove, Node} ->
			{ok, Ref} = intf:ref(Node, Intf),
			erlang:demonitor(Ref),
			NewIntf = intf:remove(Node, Intf),
			router(Name, N, Hist, NewIntf, Table, Map);
		{'DOWN', Ref, process, _, _} ->
			{ok, Down} = intf:name(Ref, Intf),
			io:format("~w: exit recived from ~w~n", [Name, Down]),
			NewIntf = intf:remove(Down, Intf),
			router(Name, N, Hist, NewIntf, Table, Map);
		{status, From} ->
			From ! {status, {Name, N, Hist, Intf, Table, Map}},
			router(Name, N, Hist, Intf, Table, Map);
		stop ->
			ok
	end.