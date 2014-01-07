-module(tfs_pubsub_server).

-export([start_link/0]).

-spec start_link() -> pid().
start_link() ->
    spawn_link(fun () -> loop([]) end).

-spec loop([pid()]) -> no_return().
loop(Subscribers) ->
    receive
        {audio, Timestamp, FlvData} ->
            ok = lists:foreach(fun (Subscriber) -> Subscriber ! {audio, Timestamp, FlvData} end, Subscribers),
            loop(Subscribers);
        {video, Timestamp, FlvData} ->
            ok = lists:foreach(fun (Subscriber) -> Subscriber ! {video, Timestamp, FlvData} end, Subscribers),
            loop(Subscribers);
        {subscribe, Subscriber} ->
            monitor(process, Subscriber),
            loop([Subscriber | Subscribers]);
        {'DOWN', _, _, Pid, _} ->
            Subscribers2 = lists:delete(Pid, Subscribers),
            loop(Subscribers2)
    end.
