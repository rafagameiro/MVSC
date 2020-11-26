-module(listMax).
-export([list_max/1]).

list_max([H|T]) -> list_max_rec(T, H).

list_max_rec([], V) -> V;
list_max_rec([H|T], V) ->
    if
        H > V -> list_max_rec(T, H);
        true -> list_max_rec(T, V)
    end.
    
