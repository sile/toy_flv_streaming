-module(tfs_handler_publish).

-export([init/2, handle_data/2, handle_message/2]).

init(_ClientSocket, PubSubPid) -> PubSubPid.

handle_data(<<FlvDataSize:32, 0:8, Timestamp:32, FlvData:FlvDataSize/binary, Rest/binary>>, PubSubPid) ->
    PubSubPid ! {video, Timestamp, FlvData},
    {?MODULE, PubSubPid, Rest};
handle_data(<<FlvDataSize:32, 1:8, Timestamp:32, FlvData:FlvDataSize/binary, Rest/binary>>, PubSubPid) ->
    PubSubPid ! {audio, Timestamp, FlvData},
    {?MODULE, PubSubPid, Rest};
handle_data(<<Rest/binary>>, PubSubPid) ->
    {?MODULE, PubSubPid, Rest}.

handle_message(_Ignore, State) ->
    {?MODULE, State}.

