-module(server).
-export([init/2, default_handler/1]).

default_handler(Request) ->
	{200, dict:store("Content-Type", "text-plain", dict:new()), "ok"}.

init(Port, Handler) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->
			case process_request(Listen, Handler) of
				{error, Cause} ->
					io:format("rudy error: ~w~n", [Cause]);
				_ -> 1
			end,
			gen_tcp:close(Listen);
		{error, Error} ->
			{error, Error}
	end.

process_request(Listen, Handler) ->
	case gen_tcp:accept(Listen) of
		{ok, Client} ->
			apply_handler(Client, Handler),
			gen_tcp:close(Client),
			process_request(Listen, Handler);
		{error, Error} ->
			{error, Error}
	end.


apply_handler(Client, Handler) ->
	case http:read_request(Client) of
		{ok, Request} ->
			Response = Handler(Request),
			http:write_response(Client, Request, Response);
		{error, Reason} ->
			{error, Reason}
	end.

