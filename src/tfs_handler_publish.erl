-module(tfs_handler_publish).

-export([handle_data/2, handle_message/2]).

handle_data(<<Data/binary>>, {_, PubSubPid} = State) ->
    case Data of
        <<0:8, Timestamp:32, Size:32, Video:Size/binary, Rest/binary>> -> PubSubPid ! {video, Timestamp, Video};
        <<1:8, Timestamp:32, Size:32, Audio:Size/binary, Rest/binary>> -> PubSubPid ! {audio, Timestamp, Audio};
        <<Rest/binary>>                                                -> ok
    end,
    {?MODULE, State, Rest}.

handle_message(_Ignore, State) ->
    {?MODULE, State}.

