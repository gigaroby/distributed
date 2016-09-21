-module(history).
-export([new/1, update/3]).

new(Name) -> dict:set(Name, -1, dict:new()).

update(Node, N, History) ->
	case dict:find(Node, History) of
		{ok, Age} when Age >= N -> old; 
		_ -> {new, dict:set(Node, N, History)}
	end.

