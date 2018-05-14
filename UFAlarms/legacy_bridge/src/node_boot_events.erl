-module(node_boot_events).
-author('Gina Hagg <ghagg@sensity.com>').

-export([subscribe/1,unsubscribe/1,notify/2]).

-compile([{parse_transform, lager_transform}]).

subscribe(EventType) ->
	lager:debug("Subscribing to Event: ~p~n",[EventType]),
    gproc:reg({p, l, {node_boots, EventType}}).

unsubscribe(EventType) ->
    gproc:unreg({p, l, {node_boots, EventType}}).

notify(EventType, Msg) ->
    lager:debug("Sending Msg:~p Event: ~p~n",[Msg,EventType]),
    gproc:send({p, l, {node_boots, EventType}}, Msg).