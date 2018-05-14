-module(mnesia_test).
-export([install/1, start/1, insert/1, insert_rec/0]).

-record(dcc_msgs, {node_id, msg_name, time_stamp, msg_bytes}).


install(Nods) ->
    Nodes = [node()|Nods],
    mnesia:create_schema([node()]),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(dcc_msgs,
                        [{attributes, record_info(fields, dcc_msgs)},
                         {disc_copies, Nodes}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

start(Nods) ->
    Nodes = [node()|Nods],
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:wait_for_tables([dcc_msgs], 5000).

insert(0) ->
    ok;
insert(N) ->
    mnesia:dirty_write({dcc_msgs, N, N}),
    timer:sleep(200),
    insert(N - 1).

insert_rec() ->
%rd(dcc_msgs, {node_id, msg_name, time_stamp, msg_bytes}),
Msgbytes = <<131,164,110,97,109,101,176,67,111,110,110,101,99,116,105,111,110,83,116,97,116,117,115,166,110,111,100,101,73,100,169,78,48,49,50,51,50,101,97,53,166,115,116,97,116,117,115,169,99,111,110,110,101,99,116,101,100>>,
 mnesia:dirty_write(#dcc_msgs{node_id = <<"N01232ea5">>, msg_name = <<"ConnectionStatus">>, time_stamp = os:system_time(), msg_bytes = Msgbytes}).