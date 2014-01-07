toy_flv_streaming
=================

映像／音声データのストリーミング機能を有したTCPサーバをErlangで実装したもの。  
全く実用的ではないトイプログラム (サーバ部分の行数は150行程度)。

ビルド
------
ビルドにはErlang(R16B02以降)が必要。
```sh
$ git clone git://github.com/sile/toy_flv_streaming.git
$ cd toy_flv_streaming
$ make init
```

実行例
------
サーバ側:
```erlang
$ make start

% サーバ起動
> tfs_server:start(7070).
```

クライアント側: (ストリーム配信者)
```erlang
$ make start
% FLVファイルを読み込む
> Flv = flv:parse_file("/path/to/movie.flv"),

% サーバへ配信
> StreamName = "stream1",
> tfs_client:publish("localhost", 7070, StreamName, Flv).
```

クライアント側: (ストリーム受信者)
```sy
# telnetでplayコマンドを直接たたく (/play<SPACE>stream1<SPACE><NEWLINE>)
$ telnet localhost 7070
/play stream1
... 以後受信ストリームの生データが延々と出力される...
```

プロトコル
---------
配信および受信には独自定義の簡易プロトコルを使用。

配信コマンド: (テキストベース)
```
/publish<SPACE>ストリーム名<SPACE>
=> この後、サーバに映像・音声データを送信する
```

受信コマンド: (テキストベース)
```
/play<SPACE>ストリーム名<SPACE>
=> この後、サーバから映像・音声データが送られてくる
```

映像・音声データ形式: (バイナリベース)
```
種別:8bit | タイムスタンプ:32bit | 映像・音声データサイズ:32bit | サイズ分の映像・音声データ

- 種別:             0=映像、1=音声
- タイムスタンプ:   単位はミリ秒
- データサイズ:     単位はバイト
- 映像・音声データ: 具体的なフォーマットは未定義(サーバ側はこの内容には関与せずにただ中継するだけ)
```
