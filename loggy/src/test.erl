-module(test).
-export([run/2]).
% report on your initial observation

run(Sleeps, Jitters) ->
	io:format("sleep;jitter;correct;out of order~n"),
	runb(Sleeps, Jitters).

runb([], _) -> ok;
runb(_, []) -> ok;
runb([S | Sleeps], [J | Jitters]) ->
	run(5000, S, J),
	receive
		{data, Correct, OOO} ->
			io:format("~w;~w;~w;~w~n", [S, J, Correct, OOO])
	end,
	runb(Sleeps, Jitters).

run(For, Sleep, Jitter) ->
    Log = logger:start([john, paul, ringo, george]),
    A = worker:start(john, Log, 13, Sleep, Jitter),
    B = worker:start(paul, Log, 23, Sleep, Jitter),
    C = worker:start(ringo, Log, 36, Sleep, Jitter),
    D = worker:start(george, Log, 49, Sleep, Jitter),
    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),
    timer:sleep(For),
    logger:stop(Log),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).


