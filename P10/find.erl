-module(find).
-export([find/2]).

find([], _ ) -> error;
find([{K, V}|T], KF) -> 
    if
        K == KF -> io:format("~p~n", [{'ok', V}]);
        true -> find(T, KF)
    end.
