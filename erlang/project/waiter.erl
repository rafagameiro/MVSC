-module(waiter).
-export([createWaiter/1, init/1]).

%Pedro Valente, n50355
%Rafael Gameiro, n50677

%Creates list containing counters for every Philosopher
createPhilosopherList(Size) ->
    if
        Size == 0 -> [];
        true -> 
            List = createPhilosopherList(Size - 1),
            [0 |List]
    end.

%Creates a boolean list representing the availability of chopsticks
createChopstickList(Size) ->
    if
        Size == 0 -> [];
        true -> 
            List = createChopstickList(Size - 1),
            [true |List]
    end.

%Gets element at given index from the given list
getListElement([H|T], Index) ->
    if
        Index == 0 -> H;
        true -> getListElement(T, Index - 1)
    end.

%Replaces value at the specified index in the specified list
changeListElement([H|T], Index, Value) ->
    if
        Index == 0 -> [Value | T];
        true -> 
            NT = changeListElement(T, Index - 1, Value),
            [H|NT]
    end.

%Get the indexes of the chopsticks that the specified Philosopher can use
getPhilosopherChopsticks(Id) ->
    A = Id - 1,
    if 
        A < 0 -> {4, Id};
        true -> {A, Id}
    end.

%Checks if all the Philosopher counters are above 0
checkEquality([H|T]) ->
    if
        H == 0 -> false;
        true ->
            if
                T == [] -> true;
                true -> checkEquality(T)
            end
    end.

%Decrements all the counters in the given list by one
decrementList([H|T]) ->
    if
        T == [] -> [H - 1|[]];
        true -> 
            NT = decrementList(T),
            [H - 1|NT]
    end.

%Checks if the chopsticks are free
chopsticksFree(Chop1, Chop2) ->
    if
        Chop1 -> 
            if
                Chop2 -> true;
                true -> false
            end;
        true -> false
    end.

%Main Waiter loop
loop(Chopsticks, Philosophers) ->
    receive
        {request, Pid, Id} -> %received an request to access chopsticks
            io:format("Welcome philosopher ~p!~n", [Id + 1]),
            {A, B} = getPhilosopherChopsticks(Id), %obtains chopsticks
            Chop1 = getListElement(Chopsticks, A),
            Chop2 = getListElement(Chopsticks, B),
            Free = chopsticksFree(Chop1, Chop2), %checks if both are available at the moment
            if
                 Free ->
                    Streak = getListElement(Philosophers, Id), %computes the number streaks the Philosopher has
                    io:format("Checking number of accesses for philosopher ~p.~n", [Id]),
                    if
                        Streak < 2 -> %If the number is lesser than the threshold
                            io:format("Philosopher ~p receives permition!~n", [Id]),
                            NewPhilosophers1 = changeListElement(Philosophers, Id, Streak + 1),
                            Equality = checkEquality(NewPhilosophers1),
                            NewChopsticks1 = changeListElement(Chopsticks, A, false),
                            NewChopsticks2 = changeListElement(NewChopsticks1, B, false),
                            Pid ! {accepted, A, B}, %Obtains the chopsticks and sends them to the Philosopher
                            if
                                Equality -> %In case all Philosopher did the exact same number of sucessful requests
                                    io:format("Fairness reached, decrementing philosophers access count.~n"),
                                    NewPhilosophers2 = decrementList(NewPhilosophers1),
                                    loop(NewChopsticks2, NewPhilosophers2);
                                true -> loop(NewChopsticks2, NewPhilosophers1)
                            end;
                        true -> Pid ! {denied}, %if the number of streaks is bigger
                                loop(Chopsticks, Philosophers)
                    end; %if the one or both chopsticks are occupied
                true -> Pid ! {denied},
                        loop(Chopsticks, Philosophers)
            end;
        {free, A, B} -> %Receives the chopsticks used and makes them available again
            Temp = changeListElement(Chopsticks, A, true),
            NewChopsticks = changeListElement(Temp, B, true),
            loop(NewChopsticks, Philosophers);
        _ -> %in case some message that it is not specified arrives
            io:format("Wrong message~n"),
            loop(Chopsticks, Philosophers)
    end.

%Initialises Waiter and starts main loop
init(Capacity) ->
    Chopsticks = createChopstickList(Capacity),
    Philosophers = createPhilosopherList(Capacity),
    loop(Chopsticks, Philosophers).

%Launch Waiter thread
createWaiter(Capacity) ->
    register(waiter, spawn(?MODULE, init, [Capacity])).
