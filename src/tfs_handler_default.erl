%% @doc クライアントとやりとりする際の入り口となるハンドラモジュール
-module(tfs_handler_default).

-export([handle_data/2, handle_message/2]).

%% @doc クライアントが publish or play のどちらを行おうとしているかを判別し、それに応じたハンドラモジュールの切り替えやPub/Subサーバの準備を行う
-spec handle_data(binary(), tfs_server:state()) -> {module(), tfs_server:state(), binary()}.
handle_data(<<"/publish ", Rest/binary>>, State) -> change_handler(tfs_handler_publish, State, ready_pubsub_server(false, Rest));
handle_data(<<"/play ",    Rest/binary>>, State) -> change_handler(tfs_handler_play,    State, ready_pubsub_server(true, Rest)).

-spec handle_message(any(), tfs_server:state()) -> {module(), tfs_server:state()}.
handle_message(_Message, State) -> {?MODULE, State}.

%% @doc 対象ストリームの名前の取得とPub/Subサーバの準備を行う
-spec ready_pubsub_server(boolean(), binary()) -> {pid(), binary()}.
ready_pubsub_server(IsPlay, Bin) ->
    [StreamName, Rest] = binary:split(Bin, <<" ">>),                                   % ストリーム名を取得
    PubSubServer = tfs_pubsub_server:ensure_started(binary_to_atom(StreamName, utf8)), % Pub/Subサーバを起動
    case IsPlay of
        false -> ok;
        true  -> PubSubServer ! {subscribe, self()}  % /playの場合は、subscriberとして登録する
    end,
    {PubSubServer, Rest}.

-spec change_handler(module(), tfs_server:state(), {pid(), binary()}) -> {module(), tfs_server:state(), binary()}.
change_handler(NewHandler, {Socket, undefined}, {PubSubServer, Data}) ->
    {NewHandler, {Socket, PubSubServer}, Data}.
