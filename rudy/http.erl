-module(http).
-export([parse_request/1, split_crlf/1]).

-define(CR, 13).
-define(LF, 10).
-define(SPACE, 32).


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
