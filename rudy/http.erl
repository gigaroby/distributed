-module(http).
-export([parse_request/1, split_crlf/1, read_request/1, write_response/3]).

-define(CR, 13).
-define(LF, 10).
-define(SPACE, 32).

% -----------------------------------------------------------------------
%							HTTP WRITER TO SOCKET
% -----------------------------------------------------------------------

write_response(Client, {_, _, HTTPVersion, _, _}, {StatusCode, Headers, Body}) ->
	HTTPVersionWire = case HTTPVersion of http10 -> "HTTP/1.0"; http11 -> "HTTP/1.1" end,
	FinalHeaders = dict:store("Content-Length", integer_to_list(length(Body)),
				    dict:store("Connection", "close", Headers)),
	StatusLine = HTTPVersionWire ++ " " ++ integer_to_list(StatusCode) ++ " " ++ message_for_code(StatusCode) ++ "\r\n",
	HeadersWire = headers_to_wire(FinalHeaders),
	BodyWire = "\r\n" ++ Body,
	gen_tcp:send(Client, StatusLine ++ HeadersWire ++ BodyWire).


headers_to_wire(Headers) ->
	headers_to_wire(dict:to_list(Headers), []).
headers_to_wire([], WireBuffer) ->
	WireBuffer;
headers_to_wire([{Key, Value} | Rest], WireBuffer) ->
	headers_to_wire(Rest, Key ++ ":" ++ Value ++ "\r\n" ++ WireBuffer).


message_for_code(Code) ->
	Messages = dict:from_list([{100, "Continue"},
							   {101, "Switching Protocols"},
							   {200, "OK"},
							   {201, "Created"},
							   {202, "Accepted"},
							   {203, "Non-Authoritative Information"},
							   {204, "No Content"},
							   {205, "Reset Content"},
							   {206, "Partial Content"},
							   {300, "Multiple Choices"},
							   {301, "Moved Permanently"},
							   {302, "Found"},
							   {303, "See Other"},
							   {304, "Not Modified"},
							   {305, "Use Proxy"},
							   {307, "Temporary Redirect"},
							   {400, "Bad Request"},
							   {401, "Unauthorized"},
							   {402, "Payment Required"},
							   {403, "Forbidden"},
							   {404, "Not Found"},
							   {405, "Method Not Allowed"},
							   {406, "Not Acceptable"},
							   {407, "Proxy Authentication Required"},
							   {408, "Request Time-out"},
							   {409, "Conflict"},
							   {410, "Gone"},
							   {411, "Length Required"},
							   {412, "Precondition Failed"},
							   {413, "Request Entity Too Large"},
							   {414, "Request-URI Too Large"},
							   {415, "Unsupported Media Type"},
							   {416, "Requested range not satisfiable"},
							   {417, "Expectation Failed"},
							   {500, "Internal Server Error"},
							   {501, "Not Implemented"},
							   {502, "Bad Gateway"},
							   {503, "Service Unavailable"},
							   {504, "Gateway Time-out"},
							   {505, "HTTP Version not supported"}]),
	case dict:find(Code, Messages) of
		error ->
			"Code " ++ integer_to_list(Code);
		{ok, Value} ->
			Value
	end.

% -----------------------------------------------------------------------
%							HTTP READER FROM SOCKET
% -----------------------------------------------------------------------

read_request(Client) ->
	case read_until_body(Client) of
		{ok, UntilBody, Rest} ->
			{Method, URL, HTTPVersion, Headers, _} = http:parse_request(UntilBody),
			case read_body(Client, {Method, URL, HTTPVersion, Headers}, Rest) of
				{ok, Body} ->
					{ok, {Method, URL, HTTPVersion, Headers, Body}};
				{error, Cause} ->
					{error, Cause}
			end;
		{error, Cause} ->
			{error, Cause}
	end.

read_until_body(Client) ->
	read_until_body(Client, [], []).

read_until_body(Client, [], Accumulator) ->
	case gen_tcp:recv(Client, 0) of
		{ok, Data} ->
			read_until_body(Client, Data, Accumulator);
		{error, Cause} ->
			{error, Cause}
	end;

read_until_body(_, [?CR, ?LF, ?CR, ?LF | Rest], Accumulator) ->
	{ok, lists:reverse([?LF, ?CR, ?LF, ?CR] ++ Accumulator), Rest};

read_until_body(Client, [B | Rest], Accumulator) ->
	read_until_body(Client, Rest, [B | Accumulator]).

read_body(_, _, Rest) ->
	{ok, Rest}.

% -----------------------------------------------------------------------
%							HTTP PARSER
% -----------------------------------------------------------------------

parse_request(Content) -> 
	[RequestLine | HeadersAndBody] = split_crlf(Content),
	{Method, URL, HTTPVersion} = parse_request_line(RequestLine),
	{Headers, Body} = parse_rest(HeadersAndBody),
	{Method, URL, HTTPVersion, Headers, Body}.

parse_request_line(Content) ->
	[M, URL, H] = string:tokens(Content, " "),
	Method = case string:to_upper(M) of 
				 "GET" -> get; "POST" -> post; "PUT" -> put;
				 "DELETE" -> delete; "HEAD" -> head
			 end,
	HTTPVersion = case H of "HTTP/1.1" -> http11; "HTTP/1.0" -> http10 end,
	{Method, URL, HTTPVersion}.

parse_rest(HeadersAndBody) ->
	parse_rest(HeadersAndBody, dict:new()).

parse_rest([[], Body], Headers) ->
	{Headers, Body};
parse_rest([Header | Rest], Headers) ->
	{Key, Value} = split_header(Header),
	parse_rest(Rest, dict:store(Key, Value, Headers)).


split_header(HeaderLine) ->
	{Key, Value} = lists:splitwith(fun(C) -> C =/= $: end, HeaderLine),
	{Key, string:strip(string:substr(Value, 2), both, ?SPACE)}.


split_crlf(Content) ->
	split_crlf(Content, [], []).

split_crlf([], Current, All) ->
	lists:reverse([lists:reverse(Current) | All]);
split_crlf([?CR, ?LF | Rest], Current, All) ->
	split_crlf(Rest, [], [lists:reverse(Current) | All]);
split_crlf([C | Rest], Current, All) ->
	split_crlf(Rest, [C | Current], All).
