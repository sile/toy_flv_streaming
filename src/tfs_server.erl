%% @doc FLV形式のvideo/audioデータのストリーミング機能(publish/play)を提供するサーバモジュール
-module(tfs_server).

-export([start/1]).
-export_type([state/0]).

-type state() :: {gen_tcp:socket(), PubSubServer::(undefined | pid())}.

%% @doc 指定のポートでストリーミングサーバを起動する
-spec start(inet:port_number()) -> no_return().
start(Port) ->
    {ok, ServerSocket} = gen_tcp:listen(Port, [binary, {active, false}]),  % listen
    accept_loop(ServerSocket).

%% @doc acceptをひたすら繰り返すループ
-spec accept_loop(gen_tcp:socket()) -> no_return().
accept_loop(ServerSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ServerSocket), 
    Worker = spawn(fun () ->  % accept後の実際の処理は別プロセスに任せる
                           receive owner_delegated -> ok end,
                           ok = inet:setopts(ClientSocket, [{active, true}, {buffer, 32 * 1024}]),
                           server_loop(tfs_handler_default, {ClientSocket, undefined}, <<"">>)  % 最初のハンドラは tfs_handler_default
                   end),
    ok = gen_tcp:controlling_process(ClientSocket, Worker),
    Worker ! owner_delegated,
    accept_loop(ServerSocket).

%% @doc クライアントとやりとりを行うループ
-spec server_loop(module(), state(), binary()) -> no_return().
server_loop(Handler, {ClientSocket, _} = State, RemainData) ->
    receive
        {tcp_closed, _}        -> exit(normal);
        {tcp_error, _, Reason} -> exit(Reason);
        {tcp, _, Data}         -> % クライアントからデータ受信
            {Handler2, State2, RemainData2} = Handler:handle_data(<<RemainData/binary, Data/binary>>, State),
            server_loop(Handler2, State2, RemainData2);
        {send, Data} ->           % クライアントにデータ送信
            ok = gen_tcp:send(ClientSocket, Data),
            server_loop(Handler, State, RemainData);
        Message ->                % その他Erlangメッセージ (サーバの制御用メッセージ)
            {Handler2, State2} = Handler:handle_message(Message, State),
            server_loop(Handler2, State2, RemainData)
    end.
