%% @doc クライアントからのpublishを処理するハンドラモジュール
-module(tfs_handler_publish).

-export([handle_data/2, handle_message/2]).

%% @doc 送られてきたバイナリをデコードして、得られたvideo/audioデータをPub/Subサーバに送信する
-spec handle_data(binary(), tfs_server:state()) -> {module(), tfs_server:state(), binary()}.
handle_data(<<Data/binary>>, {_, PubSubServer} = State) ->
    case Data of
        <<0:8, Timestamp:32, Size:32, Video:Size/binary, Rest/binary>> -> PubSubServer ! {video, Timestamp, Video};
        <<1:8, Timestamp:32, Size:32, Audio:Size/binary, Rest/binary>> -> PubSubServer ! {audio, Timestamp, Audio};
        <<Rest/binary>>                                                -> ok
    end,
    {?MODULE, State, Rest}.

-spec handle_message(any(), tfs_server:state()) -> {module(), tfs_server:state()}.
handle_message(_Message, State) -> {?MODULE, State}.
