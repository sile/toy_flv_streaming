-module(tfs_handler_default).

-export([init/1, handle_data/2, handle_message/2]).

init({ClientSocket, PubSubPid}) -> {ClientSocket, PubSubPid}.

handle_data(<<"/publish", Rest/binary>>, State) -> {tfs_handler_publish, tfs_handler_publish:init(State), Rest};
handle_data(<<"/play",    Rest/binary>>, State) -> {tfs_handler_play,    tfs_handler_play:init(State),    Rest}.

handle_message(_Ignore, State) -> {?MODULE, State}.
