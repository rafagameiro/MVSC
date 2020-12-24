-module(consumer).
-export([start/1, init/1]).

%Pedro Valente, n50355
%Rafael Gameiro, n50677

start(Value) ->
    init_consumers(2, Value).


%initializes the consumers assigned to ask for values to buffer
init_consumers(0, _) -> ok;
init_consumers(NCons, Value) ->
    spawn(?MODULE, init, [Value]),
    init_consumers(NCons - 1, Value).

%initializes a single producer with the number of times, it has to send a value to buffer
init(Value) ->
    loop(Value).

%main producer loop, replies to two types of requests according to the size of the buffer
%after sending a request, if the value was received, the number of values to request is decremented
%otherwise, tries to request another value in the future
loop(0) -> ok;
loop(Value) ->
    {Reply, V} = consume(),
    if
        Reply == empty -> 
            io:format("The buffer is currently empty.~n"),
            loop(Value);
        
        true -> 
            io:format("Value: ~p~n", [V]),
            loop(Value - 1)
    end.

%sends a request to the buffer and awaits an answer
consume() ->
    buffer ! {consume, self()},
    receive
        {reply, Reply} -> Reply 
    end,
    Reply.
