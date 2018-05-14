-module(node_util).
-author("Gina Hagg ghagg@sensity.com").

-export([connect_all_nodes/1]).
-export([connect_node/1]).


connect_node(Node) ->
    lager:debug("Attempting connect to Node: ~p~n",[Node]),
    case net_kernel:connect(Node) of
        true ->
            lager:info("Connected to Node succesfully: ~p~n",[Node]),
            ok;
        false ->
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