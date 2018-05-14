-module(node_util).

-export([connect_all_nodes/1]).
-export([connect_node/1]).
-compile([{parse_transform, lager_transform}]).

connect_node(Node) ->
    lager:debug("Attempting connect to Node: ~p~n",[Node]),
    case net_kernel:connect(Node) of
        true ->
            lager:info("Connected to Node succesfully: ~p~n",[Node]),
            ok;
        false ->
            lager:info("Can't connect to DCC, will keep trying"),
            timer:sleep(1000),
            connect_node(Node);
        ignored ->
            lager:notice("DCC is down!!!")
    end.

connect_all_nodes([]) -> ok;
connect_all_nodes(Nodes) when is_list(Nodes) ->
    Replies = lists:duplicate(length(Nodes), ok),
    Replies = pmap:map(fun connect_node/1, Nodes),
    ok.

