-module(main).
-export([start/0, init/0]).

%Pedro Valente, n50355
%Rafael Gameiro, n50677

start() ->
    spawn(?MODULE, init, []).

%Initializes both the Waiter and the Philosophers
init() ->
    startWaiter(),
    startPhilosophers(5).

%Launches the process Waiter with an expected number
%of Philosophers
startWaiter() -> 
    waiter:createWaiter(5).

%Launches a specific number of Philosophers
startPhilosophers(0) -> ok;
startPhilosophers(NPhi) ->
    philosopher:createPhilosopher(NPhi),
    startPhilosophers(NPhi - 1).
