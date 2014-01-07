-module(tfs_handler_play).

-export([init/2, handle_data/2, handle_message/2]).

init(ClientSocket, PubSubPid) ->
    PubSubPid ! {subscribe, self()},
    ClientSocket.

handle_data(_Ignore, State) ->
    {?MODULE, State, <<"">>}.

handle_message({video, Timestamp, FlvData}, ClientSocket) ->
    ok = gen_tcp:send(ClientSocket, <<(byte_size(FlvData)):32, 0:8, Timestamp:32, FlvData/binary>>),
    {?MODULE, ClientSocket};
handle_message({audio, Timestamp, FlvData}, ClientSocket) ->
    ok = gen_tcp:send(ClientSocket, <<(byte_size(FlvData)):32, 1:8, Timestamp:32, FlvData/binary>>),
    {?MODULE, ClientSocket}.
