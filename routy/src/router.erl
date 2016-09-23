-module(router).
-export([start/2, stop/1, init/1, get_status/1, setup_test/0]).

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

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

add_bi_link(Name1, Reg1, Name2, Reg2) ->
	Reg1 ! {add, Name2, Reg2},
	Reg2 ! {add, Name1, Reg1}.

setup_test() ->
	start(r1, paris),
	start(r2, london),
	start(r3, prague),
	start(r4, berlin),
	start(r5, stockholm),
	add_bi_link(paris, r1, london, r2),
	add_bi_link(paris, r1, berlin, r4),
	add_bi_link(london, r2, prague, r3),
	add_bi_link(london, r2, berlin, r4),
	add_bi_link(berlin, r4, prague, r3),
	add_bi_link(prague, r3, stockholm, r5),
	add_bi_link(london, r2, stockholm, r5).


router(Name, N, Hist, Intf, Table, Map) ->
	receive
		{add, Node, Pid} ->
			Ref = erlang:monitor(process, Pid),
			NewIntf = intf:add(Node, Ref, Pid, Intf),
			self() ! broadcast,
			self() ! update,
			io:format("~w: adding ~w: old ifaces ~w new ifaces ~w~n", [Name, Node, Intf, NewIntf]),
			router(Name, N, Hist, NewIntf, Table, Map);

		{remove, Node} ->
			{ok, Ref} = intf:ref(Node, Intf),
			erlang:demonitor(Ref),
			NewIntf = intf:remove(Node, Intf),
			self() ! broadcast,
			self() ! update,
			router(Name, N, Hist, NewIntf, Table, Map);

		{'DOWN', Ref, process, _, _} ->
			{ok, Down} = intf:name(Ref, Intf),
			io:format("~w: exit recived from ~w~n", [Name, Down]),
			NewIntf = intf:remove(Down, Intf),
			self() ! broadcast,
			self() ! update,
			router(Name, N, Hist, NewIntf, Table, Map);

		{status, From} ->
			From ! {status, {Name, N, Hist, Intf, Table, Map}},
			router(Name, N, Hist, Intf, Table, Map);

		stop ->
			io:format("~w: quitting~n", [Name]),
			ok;

		% =========================
		% Link state
		% =========================
		{links, Node, R, Links} ->
			case history:update(Node, R, Hist) of
				{new, NewHist} ->
					intf:broadcast({links, Node, R, Links}, Intf),
					NewMap = map:update(Node, Links, Map),
					if Map =:= NewMap -> self() ! update; true -> ok end,
					router(Name, N, NewHist, Intf, Table, NewMap);
				old ->
					router(Name, N, Hist, Intf, Table, Map)
			end;

		update ->
			NewTable = dijkstra:table(intf:list(Intf), Map),
			router(Name, N, Hist, Intf, NewTable, Map);

		broadcast ->
			Message = {links, Name, N, intf:list(Intf)},
			intf:broadcast(Message, Intf),
			router(Name, N+1, Hist, Intf, Table, Map);

		% =========================
		% Routing
		% =========================
		{route, Name, _, Message} ->
			io:format("~w received message ~w ~n", [Name, Message]),
			router(Name, N, Hist, Intf, Table, Map);

		ForwardMessage = {route, To, _, Message} ->
			io:format("~w: routing message (~w)~n", [Name, Message]),
			case dijkstra:route(To, Table) of
				{ok, Gw} ->
					case intf:lookup(Gw, Intf) of
						{ok, Pid} ->
							Pid ! ForwardMessage;
						_ ->
							ok
					end;
				_ ->
					ok
			end,
			router(Name, N, Hist, Intf, Table, Map);

		{send, To, Message} ->
			self() ! {route, To, Name, Message},
			router(Name, N, Hist, Intf, Table, Map)
	end.

get_status(Node) ->
	Node ! {status, self()},
	receive
		{status, {Name, N, _, Intf, Table, Map}} ->
			io:format("~w status: n=~w table=~w intf=~w map=~w~n", [Name, N, Table, Intf, dict:to_list(Map)])
	end.
