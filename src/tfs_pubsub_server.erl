-module(tfs_pubsub_server).

-export([start/1]).

-spec start(atom()) -> pid().
start(StreamName) ->
    case whereis(StreamName) of
        undefined ->
            Pid = spawn(fun () -> loop([]) end),
            true = register(StreamName, Pid),
            Pid;
        Pid -> Pid
    end.

-spec loop([pid()]) -> no_return().
loop(Subscribers) ->
    receive
        {audio, _, _} = Message -> notify(Message, Subscribers), loop(Subscribers);
        {video, _, _} = Message -> notify(Message, Subscribers), loop(Subscribers);
        {subscribe, Pid}        -> monitor(process, Pid),        loop(Subscribers ++ [Pid]);
        {'DOWN', _, _, Pid, _}  ->                               loop(Subscribers -- [Pid])
    after 1000 ->
            case Subscribers of
                [] -> exit(normal);
                _  -> loop(Subscribers)
            end        
    end.

-spec notify(any(), [pid()]) -> ok.
notify(Message, Subscribers) ->
    lists:foreach(fun (Subscriber) -> Subscriber ! Message end, Subscribers).
