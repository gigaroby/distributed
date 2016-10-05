-module(gms3).

-export([start/1, start/2]).

-define(ARGH, 100).

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
	leader(Id, Master, [], [Master], 0).


start(Id, Grp) ->
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.


init(Id, Grp, Master) ->
	Self = self(),
	Grp ! {join, Master, Self},
	receive
		Msg = {view, N, [Leader|Slaves], Group} ->
			erlang:monitor(process, Leader),
			Master ! {view, Group},
			slave(Id, Master, Leader, Slaves, Group, Msg, N)
	after 5000 ->
		% wait 5 seconds for the master to reply, otherwise quit
		Master ! {error, "no reply from leader"}
	end.


leader(Id, Master, Slaves, Group, MessageID) ->
	NextMessageID = MessageID + 1,
	receive
		{mcast, Msg} ->
			bcast(Id, {msg, NextMessageID, Msg}, Slaves),
			Master ! Msg,
			leader(Id, Master, Slaves, Group, NextMessageID);

		{join, Wrk, Peer} ->
			Slaves2 = lists:append(Slaves, [Peer]),
			Group2 = lists:append(Group, [Wrk]),
			bcast(Id, {view, NextMessageID, [self()|Slaves2], Group2}, Slaves2),
			Master ! {view, Group2},
			leader(Id, Master, Slaves2, Group2, NextMessageID);

		stop ->
			ok
	end.


slave(Id, Master, Leader, Slaves, Group, LastMessage, MessageID) ->
	receive
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, Slaves, Group, LastMessage, MessageID);

		{join, Wrk, Peer} ->
			Leader ! {join, Wrk, Peer},
			slave(Id, Master, Leader, Slaves, Group, LastMessage, MessageID);

		{msg, MID, _} when MID =< MessageID ->
			slave(Id, Master, Leader, Slaves, Group, LastMessage, MessageID);

		NextMessage = {msg, NextMessageID, Msg} ->
			Master ! Msg,
			slave(Id, Master, Leader, Slaves, Group, NextMessage, NextMessageID);

		NextMessage = {view, NextMessageID, [Leader|Slaves2], Group2} when NextMessageID > MessageID ->
			Master ! {view, Group2},
			slave(Id, Master, Leader, Slaves2, Group2, NextMessage, NextMessageID);

		{view, _,  _, _} ->
			slave(Id, Master, Leader, Slaves, Group, LastMessage, MessageID);

		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, Slaves, Group, LastMessage, MessageID);

		stop ->
			ok
	end.

election(Id, Master, Slaves, [_ | Group], LastMessage, MessageID) ->
	Self = self(),
	case Slaves of
		[Self | Rest] ->
			bcast(Id, LastMessage, Rest),
			bcast(Id, {view, MessageID+1, Slaves, Group}, Rest),
			Master ! {view, Group},
			io:format("~w: leader stepping up~n", [Id]),
			leader(Id, Master, Rest, Group, MessageID + 1);
		[Leader | Rest] ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, Rest, Group, LastMessage, MessageID)
	end.
