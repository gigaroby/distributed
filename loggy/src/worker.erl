-module(worker).
-export([start/5, stop/1, peers/2]).

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

start(Name, Logger, Seed, Sleep, Jitter) ->
	spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
	Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
	rand:seed(exs1024, {Seed, Seed, Seed}),
	receive
		{peers, Peers} ->
			loop(Name, Log, Peers, Sleep, Jitter);
		stop ->
			ok
	end.

peers(Wrk, Peers) ->
	Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter)->
	Wait = rand:uniform(Sleep),
	receive
		{msg, Time, Msg} ->
			Log ! {log, Name, Time, {received, Msg}},
			loop(Name, Log, Peers, Sleep, Jitter);
		stop ->
			ok;
		Error ->
			Log ! {log, Name, time, {error, Error}}
	after Wait ->
			  Selected = select(Peers),
			  Time = na,
			  Message = {hello, rand:uniform(1000)},
			  Selected ! {msg, Time, Message},
			  jitter(Jitter),
			  Log ! {log, Name, Time, {sending, Message}},
			  loop(Name, Log, Peers, Sleep, Jitter)
	end.

select(Peers) ->
	lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) ->
	J = rand:uniform(Jitter),
	timer:sleep(J).
