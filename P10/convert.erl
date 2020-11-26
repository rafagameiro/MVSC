-module(convert).
-export([c/1]).

c({T, V}) ->
    if
        T == 'f' -> R = tempConvert:f2c(V);
        T == 'c' -> R = tempConvert:c2f(V)
    end,
    io:format("Result: ~p~n", [R]).
