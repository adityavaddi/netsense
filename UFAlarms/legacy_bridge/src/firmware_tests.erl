-module(firmware_tests).
-author("Gina Hagg <ghagg@ysensity.com").
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).
-define(BATCH_NUM,2).

%[[{<<"N0e1">>,"v4-ca0584a",1,1},{<<"N0e2">>,"v4-ca0584a",1,1}],
 %[{<<"N0e3">>,"v4-ca0584a",0,2},{<<"N0e4">>,"v4-ca0584a",0,2}],[{<<"N0e5">>,"v4-ca0584a",0,3}]]
test_chunk_nodes() ->
    Nodeids = [<<"N0e1">>,<<"N0e2">>,<<"N0e3">>,<<"N0e4">>,<<"N0e5">>],
    Version = "v4-ca0584a",
    NodeIdsBy10 = helpers:n_length_chunks_fast(Nodeids, ?BATCH_NUM),
    Len = length(NodeIdsBy10),
    lager:debug("NODEIDSBY10: ~p, send_to_nodes~n",[NodeIdsBy10]),
    NewNodes = [[case Group of 1 -> {Nid,Version,1, Group}; _->{Nid,Version,0, Group} end || Nid <- lists:nth(Group,NodeIdsBy10)]|| Group <- lists:seq(1,Len)],
    lager:debug("NODEIDSBY10: ~p, send_to_nodes~n",[NewNodes]),
    {NodeIdsBy10,NewNodes}.
    %?assert_equal(Len, 3).

 
    