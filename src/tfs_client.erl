-module(tfs_client).

-include_lib("flv/include/flv.hrl").

-export([publish/3]).

-record(publish_state, {
          socket                     :: gen_tcp:socket(),
          flv_first_timestamp        :: undefined | non_neg_integer(), % unit: milliseconds
          publish_start_time = now() :: erlang:timestamp()
        }).

-spec publish(iodata(), inet:port_number(), flv:flv()) -> ok | {error, Reason::term()}.
publish(Host, Port, {_, FlvEntries}) ->
    case gen_tcp:connect(Host, Port, [binary, {active, false}]) of
        {error, Reason} -> {error, Reason};
        {ok, Socket}    ->
            ok = gen_tcp:send(Socket, "/publish"),
            publish_loop([E#flv_body_entry.tag || E <- FlvEntries], #publish_state{socket = Socket})
    end.

-spec publish_loop([#flv_tag{}], #publish_state{}) -> ok.
publish_loop([], _State)                                              -> ok;
publish_loop([#flv_tag{data = #flv_tag_script_data{}} | Tail], State) -> publish_loop(Tail, State);
publish_loop([#flv_tag{data = #flv_tag_unknown{}} | Tail], State)     -> publish_loop(Tail, State);
publish_loop([Head | Tail], State) ->
    State2 = rate_emulate(Head, State),
    ok = gen_tcp:send(State2#publish_state.socket, encode_flv_tag(Head)),
    publish_loop(Tail, State2).

-spec encode_flv_tag(#flv_tag{}) -> iodata().
encode_flv_tag(#flv_tag{timestamp = Timestamp, data = Data}) ->
    {Type, Bytes} =
        case Data of
            #flv_tag_video{} -> {0, flv_tag:encode_video(Data)};
            #flv_tag_audio{} -> {1, flv_tag:encode_audio(Data)}
        end,
    [<<(iolist_size(Bytes)):32, Type:8, Timestamp:32>>, Bytes].

-spec rate_emulate(#flv_tag{}, #publish_state{}) -> #publish_state{}.
rate_emulate(Tag, #publish_state{flv_first_timestamp = undefined} = State) ->
    rate_emulate(Tag, State#publish_state{flv_first_timestamp = Tag#flv_tag.timestamp});
rate_emulate(#flv_tag{timestamp = Timestamp}, State) ->
    Elapsed = timer:now_diff(now(), State#publish_state.publish_start_time) div 1000,
    Offset  = Timestamp - State#publish_state.flv_first_timestamp,
    WaitDuration = max(0, Offset - Elapsed),
    ok = timer:sleep(WaitDuration),
    State.
