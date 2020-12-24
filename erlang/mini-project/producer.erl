-module(producer).
-export([start/1, init/1]).

%Pedro Valente, n50355
%Rafael Gameiro, n50677

start(Value) ->
    init_producers(3, Value).

%initializes the producers assigned to send values to buffer
init_producers(0, _) -> ok;
init_producers(NProd, Value) ->
    spawn(?MODULE, init, [Value]),
    init_producers(NProd - 1, Value).

%initializes a single producer with the number of times, it has to send a value to buffer
init(Value) ->
    loop(Value).

%main producer loop, replies to two types of requests according to the size of the buffer
%after sending a request, if the value was successfully inserted, the number of values to send is decremented
%otherwise, tries to resend the value in the future
loop(0) -> ok;
loop(Value) ->
    Reply = produce(Value),
    if
        Reply == full -> 
            io:format("The buffer is already full.~n"),
            loop(Value);

        true -> 
            io:format("Value inserted successfully.~n"),
            loop(Value - 1)
    end.

%sends a value to the buffer and awaits an answer
produce(Value) ->
    buffer ! {produce, self(), Value},
    receive
        {reply, Reply} -> Reply
    end,
    Reply.

