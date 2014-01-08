%% @doc publishされたデータを、多数のクライアントに配信するための中継地点となるPub/Subサーバ
-module(tfs_pubsub_server).

-export([ensure_started/1]).

%% @doc 指定されたストリーム名に対応するPub/Subサーバを起動する (既に起動済みの場合は何もしない)
-spec ensure_started(atom()) -> pid().
ensure_started(StreamName) ->
    case whereis(StreamName) of
        undefined -> % 新規
            Pid = spawn(fun () -> loop([]) end), % サーバプロセスの起動
            true = register(StreamName, Pid),    % ストリーム名とプロセスの対応付けを行う
            Pid;
        Pid -> Pid   % 起動済み
    end.

%% @doc サーバのメインループ
-spec loop([pid()]) -> no_return().
loop(Subscribers) ->
    receive
        {audio, _, _} = Msg -> notify(Msg, Subscribers), loop(Subscribers);          % audioデータがpublishされた => subscriberに配信
        {video, _, _} = Msg -> notify(Msg, Subscribers), loop(Subscribers);          % videoデータがpublishされた => subscriberに配信
        {subscribe, Pid}    -> monitor(process, Pid),    loop(Subscribers ++ [Pid]); % 新規subscriber(プロセス)の登録 (monitor/2を使ってダウンも検出)
        {'DOWN',_,_,Pid,_}  ->                           loop(Subscribers -- [Pid])  % subscriber(プロセス)がダウン(or 正常終了)したので登録解除
    after 1000 ->
            case Subscribers of     % 一定期間、何のデータもpublishされず、かつsubscriberがいないなら、サーバプロセスを終了する
                [] -> exit(normal);
                _  -> loop(Subscribers)
            end        
    end.

%% @doc メッセージを登録済みのsubscriber群に一斉送信するための補助関数
-spec notify(any(), [pid()]) -> ok.
notify(Message, Subscribers) ->
    lists:foreach(fun (Subscriber) -> Subscriber ! Message end, Subscribers).
