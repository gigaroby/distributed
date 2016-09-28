-module(logger).
-export([start/1, stop/1]).

start(Nodes) ->
	spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
	Logger ! {stop, self()}.

init(_) ->
	loop(0, 0, dict:new()).


loop(Correct, OOO, OutOfOrder) ->
	receive
		{log, From, Time, Msg} ->
			case detect_ooo(Msg, OutOfOrder) of
				{ok, NewOutOfOrder} ->
					log(From, Time, Msg),
					loop(Correct + 1, OOO, NewOutOfOrder);
				{outoforder, NewOutOfOrder, _} ->
					log(From, Time, Msg),
					loop(Correct, OOO + 1, NewOutOfOrder);
				{newentry, NewOutOfOrder} ->
					loop(Correct, OOO, NewOutOfOrder)
			end;
		{stop, From} ->
			From ! {data, Correct, OOO},
			ok
	end.

detect_ooo({Mode, Msg}, OutOfOrder) ->
	case dict:find(Msg, OutOfOrder) of
		error ->
			{newentry, dict:store(Msg, Mode, OutOfOrder)};
		{ok, received} when Mode =:= sending ->
			{outoforder, dict:erase(Msg, OutOfOrder), Msg};
		{ok, sending} when Mode =:= received ->
			{ok, dict:erase(Msg, OutOfOrder)}
	end.



log(From, Time, Msg) ->
	ok.
	% io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
