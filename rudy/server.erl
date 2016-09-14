-module(server).
-export([start/2, stop/0, default_handler/1]).

default_handler(Request) ->
	timer:sleep(40),
	{200, dict:store("Content-Type", "text-plain", dict:new()), "ok"}.

start(Port, Handler) ->
	register(rudy, spawn(fun() -> init(Port, Handler) end)).

stop() ->
	exit(whereis(rudy), "time to die").

init(Port, Handler) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->
			try listen_and_spawn(Listen, Handler) of
				{error, Cause} -> exit(Cause)
			after
				gen_tcp:close(Listen)
			end;
		{error, Cause} -> exit(Cause)
	end.

listen_and_spawn(Listen, Handler) ->
	case gen_tcp:accept(Listen) of
		{ok, Client} ->
			Pid = spawn(fun() -> apply_handler(Client, Handler) end),
			gen_tcp:controlling_process(Client, Pid),
			listen_and_spawn(Listen, Handler);
		Err = {error, _} ->
			Err
	end.


apply_handler(Client, Handler) ->
	try http:read_request(Client) of
		{ok, Request} ->
			Response = Handler(Request),
			http:write_response(Client, Request, Response);
		{error, Cause} -> 
			exit(Cause)
	after
		gen_tcp:close(Client)
	end.
