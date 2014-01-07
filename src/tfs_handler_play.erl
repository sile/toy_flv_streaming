-module(tfs_handler_play).

-export([handle_data/2, handle_message/2]).

handle_data(_Ignore, State) ->
    {?MODULE, State, <<"">>}.

handle_message(Message, {ClientSocket, _} = State) ->
    Data = case Message of
               {video, Timestamp, Video} -> <<0:8, Timestamp:32, (byte_size(Video)):32, Video/binary>>;
               {audio, Timestamp, Audio} -> <<1:8, Timestamp:32, (byte_size(Audio)):32, Audio/binary>>
           end,
    ok = gen_tcp:send(ClientSocket, Data),
    {?MODULE, State}.
