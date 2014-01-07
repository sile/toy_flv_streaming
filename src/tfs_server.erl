%% @doc FLVのストリーミング機能(publish/play)を提供するサーバ
-module(tfs_server).

-export([start_link/1]).

%% @doc 指定のポートでストリーミングサーバを起動する
-spec start_link(inet:port_number()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Port) ->
    case gen_tcp:listen(Port, [binary, {active, false}]) of
        {error, Reason}    -> {error, Reason};
        {ok, ServerSocket} ->
            Pid = spawn_link(fun () ->
                                     PubSubPid = tfs_pubsub_server:start_link(),
                                     accept_loop({ServerSocket, PubSubPid})
                             end),
            ok = gen_tcp:controlling_process(ServerSocket, Pid),
            {ok, Pid}
    end.

-spec accept_loop({gen_tcp:socket(), pid()}) -> no_return().
accept_loop({ServerSocket, PubSubPid} = State) ->
    case gen_tcp:accept(ServerSocket) of
        {error, Reason}    -> exit(Reason);
        {ok, ClientSocket} ->
            Pid = spawn(fun () ->
                                ok = timer:sleep(1),
                                ok = inet:setopts(ClientSocket, [{active, true}, {buffer, 32 * 1024}]),
                                server_loop(ClientSocket, tfs_handler_default, tfs_handler_default:init(ClientSocket, PubSubPid), <<>>)
                        end),
            ok = gen_tcp:controlling_process(ClientSocket, Pid),
            accept_loop(State)
    end.

-spec server_loop(gen_tcp:socket(), module(), term(), binary()) -> no_return().
server_loop(ClientSocket, Handler, HandlerState, RemainData) ->
    receive
        {tcp_closed, _}        -> exit(normal);
        {tcp_error, _, Reason} -> exit(Reason);
        {tcp, _, Data}         ->
            {Handler2, HandlerState2, RemainData2} = Handler:handle_data(<<RemainData/binary, Data/binary>>, HandlerState),
            server_loop(ClientSocket, Handler2, HandlerState2, RemainData2);
        {send, Data} ->
            ok = gen_tcp:send(ClientSocket, Data),
            server_loop(ClientSocket, Handler, HandlerState, RemainData);
        Message ->
            {Handler2, HandlerState2} = Handler:handle_message(Message, HandlerState),
            server_loop(ClientSocket, Handler2, HandlerState2, RemainData)
    end.
