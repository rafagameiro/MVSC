-module(tempConvert).
-export([f2c/1, c2f/1]).

f2c(T) -> 
    R = (T - 32) * (5 / 9),
    io:format("Result: ~p~n", [R]).


c2f(T) -> 
    R = T * (9 / 5) + 32,
    io:format("Result: ~p~n", [R]).
