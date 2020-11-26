-module(tempConvert).
-export([f2c/1, c2f/1]).

f2c(T) -> 
    (T - 32) * (5 / 9).


c2f(T) -> 
    T * (9 / 5) + 32.
