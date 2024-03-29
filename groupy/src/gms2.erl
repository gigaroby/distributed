-module(gms2).

-export([start/1, start/2]).

-define(ARGH, 1000).

bcast(Id, Msg, Nodes) ->
	lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
	case rand:uniform(?ARGH) of
		?ARGH ->
			io:format("~w: leader crash~n", [Id]),
			exit(no_luck);
		_ ->
			ok
	end.

start(Id) ->
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Self) end)}.


init(Id, Master) ->
	leader(Id, Master, [], [Master]).


start(Id, Grp) ->
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.


init(Id, Grp, Master) ->
	Self = self(),
	Grp ! {join, Master, Self},
	receive
		{view, [Leader|Slaves], Group} ->
			erlang:monitor(process, Leader),
			Master ! {view, Group},
			slave(Id, Master, Leader, Slaves, Group)
	after 5000 ->
		% wait 5 seconds for the master to reply, otherwise quit
		Master ! {error, "no reply from leader"}
	end.


leader(Id, Master, Slaves, Group) ->
	receive
		{mcast, Msg} ->
			bcast(Id, {msg, Msg}, Slaves),
			Master ! Msg,
			leader(Id, Master, Slaves, Group);

		{join, Wrk, Peer} ->
			Slaves2 = lists:append(Slaves, [Peer]),
			Group2 = lists:append(Group, [Wrk]),
			bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
			Master ! {view, Group2},
			leader(Id, Master, Slaves2, Group2);

		stop ->
			ok
	end.


slave(Id, Master, Leader, Slaves, Group) ->
	receive
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, Slaves, Group);

		{join, Wrk, Peer} ->
			Leader ! {join, Wrk, Peer},
			slave(Id, Master, Leader, Slaves, Group);

		{msg, Msg} ->
			Master ! Msg,
			slave(Id, Master, Leader, Slaves, Group);

		{view, [Leader|Slaves2], Group2} ->
			Master ! {view, Group2},
			slave(Id, Master, Leader, Slaves2, Group2);

		{view, [Other | _], _} ->
			io:format("~w: got a view with unknown leader ~w, discarding message~n", [Id, Other]),
			slave(Id, Master, Leader, Slaves, Group);


		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, Slaves, Group);

		stop ->
			ok
	end.

election(Id, Master, Slaves, [_ | Group]) ->
	Self = self(),
	case Slaves of
		[Self | Rest] ->
			bcast(Id, {view, Slaves, Group}, Rest),
			Master ! {view, Group},
			io:format("~w: leader stepping up~n", [Id]),
			leader(Id, Master, Rest, Group);
		[Leader | Rest] ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, Rest, Group)
	end.
