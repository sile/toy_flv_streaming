-module(tfs_handler_default).

-export([init/2, handle_data/2, handle_message/2]).

init(_ClientSocket, PubSubPid) -> PubSubPid.

handle_data(<<"/publish", Rest/binary>>, State) -> {tfs_handler_publish, State, Rest};
handle_data(<<"/play",    Rest/binary>>, State) -> {tfs_handler_play,    State, Rest}.

handle_message(_Ignore, State) -> {?MODULE, State}.
