-module(tfs_handler_default).

-export([handle_data/2, handle_message/2]).

handle_data(<<"/publish ", Rest/binary>>, State) -> change_handler(tfs_handler_publish, State, ready_pubsub_server(false, Rest));
handle_data(<<"/play ",    Rest/binary>>, State) -> change_handler(tfs_handler_play,    State, ready_pubsub_server(true, Rest)).

handle_message(_Message, State) -> {?MODULE, State}.

-spec ready_pubsub_server(boolean(), binary()) -> RemainBytes::binary().
ready_pubsub_server(DoSubscribe, Bin) ->
    [StreamName, Rest] = binary:split(Bin, <<" ">>),
    Pid = tfs_pubsub_server:start(binary_to_atom(StreamName, utf8)),
    case DoSubscribe of
        false -> ok;
        true  -> Pid ! {subscribe, self()}
    end,
    {Pid, Rest}.

change_handler(NewHandler, {Socket, undefined}, {PubSubPid, Data}) ->
    {NewHandler, {Socket, PubSubPid}, Data}.
