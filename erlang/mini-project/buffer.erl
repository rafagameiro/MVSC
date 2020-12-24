-module(buffer).
-export([start/1, initSupervisor/1, init/1]).

%Pedro Valente, n50355
%Rafael Gameiro, n50677

start(Size) ->
    spawn(?MODULE, initSupervisor, [Size]).

%initializes the supervisor process and launches a linked buffer thread
initSupervisor(Size) ->
    process_flag(trap_exit, true),
    register(buffer, spawn_link(?MODULE, init, [Size])),
    supervisor(Size).

%main loop of the supervisor, if the buffer exits normally, then the supervisor exists aswell, if it exits with an error, the supervisor lauches a new linked replacement
supervisor(Size) ->
    receive
        {'EXIT', _Pid, normal} ->
            io:format("Buffer exited noramlly, shutting down supervisor.~n");
        {'EXIT', _Pid, _Reason} ->
            register(buffer, spawn_link(?MODULE, init, [Size])),
            supervisor(Size)
    end.

%initializes buffer
init(Size) ->
    Buffer = {Size, 0, []},
    loop(Buffer).

%main buffer loop, replies to both consumer and producer requests according to the size of the buffer
loop(Buffer)->
    receive
        {produce, Pid, Value} -> 
            {NewBuffer, Reply} = produce_reply(Buffer, Value),
            %io:format("Buffer: ~p~n", [NewBuffer]),
            reply(Pid, Reply),
            loop(NewBuffer);
        {consume, Pid} -> 
            {NewBuffer, Reply} = consume_reply(Buffer),
            %io:format("Buffer: ~p~n", [NewBuffer]),
            reply(Pid, Reply),
            loop(NewBuffer);
        true -> loop(Buffer)
    end.

%sends specified message to the specified thread, coded reply
reply(Pid, Reply) -> Pid ! {reply, Reply}.

%produces reply for a producer request when the buffer is empty, coded ok and inserts new element in the front of the buffer
produce_reply({Size, Len, []}, Value) -> {{Size, Len + 1, [Value]}, ok};
%produces reply for a producer request, if the buffer is full, codes the message with full and does nothing, otherwise with ok and inserts new element in the front of the buffer
produce_reply({Size, Len, [H|T]}, Value) -> 
    if 
        Len < Size -> {{Size, Len + 1, [Value, H|T]}, ok};
        true -> {{Size, Len, [H|T]}, full}
    end.

%produces reply for a consumer when the buffer is full, coded empty and with a -1, does nothing to the buffer
consume_reply({Size, Len, []}) -> {{Size, Len, []}, {empty, -1}};
%produces reply for a consumer when the buffer is either full or with atleast one element, coded ok, removes the element at the front of the buffer and places it in the reply
consume_reply({Size, Len, [H|T]}) ->
    {{Size, Len - 1, T}, {ok, H}}.

