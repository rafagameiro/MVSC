-module(philosopher).
-export([createPhilosopher/1, init/1]).

%Pedro Valente, n50355
%Rafael Gameiro, n50677

%Sleep function for T milliseconds
sleep(T) ->
    receive
    after
        T -> true
    end.  

%Random number generator
random(Min, Max) ->
    V = (rand:uniform() * (Max - Min)) + Min,
    round(V).

%Philosopher eats for a set amount of time, returns chopsticks to the waiter and begins thinking
eating(Id, ChopstickA, ChopstickB) ->
    sleep(500),
    io:format("Eating terminated, philosopher ~p returning chopsticks.~n", [Id]),
    waiter ! {free, ChopstickA, ChopstickB},
    thinking(Id).

%Philosopher thinks for a random amount of time and requests chopsticks from the waiter, 
%if accepted he begins eating, otherwise he begins thinking again
thinking(Id) ->
    sleep(random(300, 500)),
    io:format("Requesting access, philosopher ~p.~n", [Id]),
    waiter ! {request, self(), Id - 1},
    receive
        {denied} -> 
            io:format("Access denied, philosopher ~p waiting.~n", [Id]),
            thinking(Id);
        {accepted, A, B} -> 
            io:format("Access granted, philosopher ~p eating.~n", [Id]),
            eating(Id, A, B)
    end.

%Start the Philosopher loop
init(Id) ->
    thinking(Id).

%Initialise and launch Philosopher thread
createPhilosopher(Id) ->
    spawn(?MODULE, init, [Id]).
