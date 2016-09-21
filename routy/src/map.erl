-module(map).
-export([new/0, reachable/2, all_nodes/1, update/3]).

% new returns a new, empty map
% the map does not guarantee ordering
new() -> dict:new().

update(Node, Connections, Map) ->
	dict:store(Node, Connections, Map).

reachable(Node, Map) ->
	case dict:find(Node, Map) of
		{ok, Value} -> Value;
		_ -> []
	end.

all_nodes(Map) -> 
	NodeSet = dict:fold(fun add_all/3, sets:new(), Map),
	sets:to_list(NodeSet).

add_all(Node, Connections, AccIn) ->
	SetAfterConn = lists:foldl(fun sets:add_element/2, AccIn, Connections),
	sets:add_element(Node, SetAfterConn).
