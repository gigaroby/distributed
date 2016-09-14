-module(bench).
-export([run/2]).

run(Host, Port) ->
	Start = erlang:system_time(micro_seconds),
	execute(10000, Host, Port),
	Finish = erlang:system_time(micro_seconds),
	Finish - Start.

execute(N, Host, Port) ->
	if
		N == 0 ->
			ok;
		true ->
			request(Host, Port),
			execute(N-1, Host, Port)
	end.

request(Host, Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	{ok, Server} = gen_tcp:connect(Host, Port, Opt),
	gen_tcp:send(Server, "GET / HTTP/1.1\r\nContent-Length: 10\r\n\r\n11111111111\r\n"),
	Recv = gen_tcp:recv(Server, 0),
	case Recv of
		{ok, _} ->
			ok;
		{error, Error} ->
			io:format("test: error: ~w~n", [Error])
	end,
	gen_tcp:close(Server).
