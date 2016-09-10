-module(http_tests).
-include_lib("eunit/include/eunit.hrl").

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

check_table(_, []) -> true;
check_table(Fn, [{Input, Expected} | Rest]) -> 
    ?assertEqual(Expected, Fn(Input)), 
    check_table(Fn, Rest).

split_crlf_test() ->
	Table = [
		{"First\r\nSecond", ["First", "Second"]},
		{"\r\nFirst", [[], "First"]},
		{"First\r\nSecond\r\n\r\nThird", ["First", "Second", [], "Third"]},
		{"First\r\n\r\n", ["First", [], []]}
	],
	check_table(fun http:split_crlf/1, Table).



parse_request_test() ->
    Table = [
        {
            "GET / HTTP/1.1\r\nHost: example.com\r\n\r\nwbody",
            {get, "/", http11, dict:store("Host", "example.com", dict:new()), "wbody"}
        }
    ],
    check_table(fun http:parse_request/1, Table).
