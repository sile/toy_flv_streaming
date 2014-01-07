%% @doc クライアントからのplayリクエストを処理する(ストリームを配信する)ハンドラモジュール
-module(tfs_handler_play).

-export([handle_data/2, handle_message/2]).

-spec handle_data(binary(), tfs_server:state()) -> {module(), tfs_server:state(), binary()}.
handle_data(_Data, State) -> {?MODULE, State, <<"">>}.

%% @doc Pub/Subサーバから送信されてきたvideo/audioデータをクライアントに配信する
-spec handle_message(any(), tfs_server:state()) -> {module(), tfs_server:state()}.
handle_message(Message, {ClientSocket, _} = State) ->
    Data = case Message of  % バイナリ形式にエンコード
               {video, Timestamp, Video} -> <<0:8, Timestamp:32, (byte_size(Video)):32, Video/binary>>;
               {audio, Timestamp, Audio} -> <<1:8, Timestamp:32, (byte_size(Audio)):32, Audio/binary>>
           end,
    ok = gen_tcp:send(ClientSocket, Data),
    {?MODULE, State}.
