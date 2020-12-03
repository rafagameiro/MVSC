-module(mutex).
-export([start/0, stop/0]).
-export([acquire/0, release/0, init/0]).

start() ->
    register(mutex, spawn(?MODULE, init, [])).

stop() ->
    mutex ! stop.

acquire() ->
    mutex ! {acquire, self()},
    receive
        ok -> ok
    end.

release() ->
    mutex ! {release, self()},
    ok.

init() -> free().

free() ->
    receive
        {acquire, Pid} ->
            Pid ! ok,
            busy(Pid);
        stop ->
            terminate()
    end.

busy(Pid) ->
    receive 
        {release, Pid} ->
            free()
    end.

terminate() ->
    receive
        {acquire, Pid} ->
            exit(Pid, kill),
            terminate()
    after 0 -> ok
    end.

