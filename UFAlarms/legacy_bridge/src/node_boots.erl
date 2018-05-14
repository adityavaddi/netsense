-module(node_boots).
-author('Gina Hagg <ghagg@sensity.com>').
-behaviour(gen_server).

-export([start_link/0]).
-export([
    init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,terminate/2
    ,code_change/3
]).

-record(state, {}).

-compile([{parse_transform, lager_transform}]).

-define(BATCH_NUM,10).
-define(EVENTS_TO_SUBSCRIBE, [new_login, ota_sent, send_to_nodes, ten_sent]).

start_link() ->
    lager:info("Starting node_boots server~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    [gproc:reg({p, l, E}) || E <- ?EVENTS_TO_SUBSCRIBE ],
    {ok, #state{}}.

%[{<<"N0e3">>,"v4-ca0584a",1,2}]
process_node( Nid, NodeVersion) -> 
    NodeDetails = tempcache:dets_fetch_fw(Nid),
    lager:debug("NodeDetails: ~p~n",[NodeDetails]),
    check_details(Nid,NodeVersion,NodeDetails).

check_details(Nid, _,[]) ->
    lager:debug("we got nothing on this node ~p ~n",[Nid]);

check_details(Nid, LoginVersion, [{_,NewVersion, Trytimes, Group, unknown}]) ->
    lager:debug("Node details: Id:~p, NodeVersion:~p, NewVersion:~p, Trytimes:~p, Group:~p~n",[Nid, LoginVersion,NewVersion, Trytimes, Group]),
    case string:str(LoginVersion,NewVersion) > 0 of true ->   %ota succeeded
        lager:info("UPDATE SUCCESS firmware ~p for Node ~p~n", [NewVersion, Nid]),
        FwNodes = gproc:get_value({n,l,NewVersion}),
        LeftTodo = lists:delete(Nid, FwNodes),
        all_nodes_succeeded_or_not(LeftTodo,{Nid, NewVersion,Trytimes, Group});
    _-> %ota failed on this node, send 2nd time, increase trytimes, hold the update.
        try_once_more_or_fail(NewVersion,Nid,Trytimes,Group)
    end.

all_nodes_succeeded_or_not([],{Nid, NewVersion,Trytimes, Group}) ->
    tempcache:dets_fw_delete(Nid),
    tempcache:do_dets({Nid, NewVersion, Trytimes,Group, success}),
    self()! {ready_for_next_batch,{Group+1, NewVersion}};

all_nodes_succeeded_or_not(LeftTodo,{Nid, NewVersion,Trytimes, Group}) ->
    gproc:unreg({n,l,NewVersion}),
    gproc:reg({n,l,NewVersion}, LeftTodo),
    tempcache:dets_fw_delete(Nid),
    tempcache:do_dets({Nid, NewVersion, Trytimes,Group, success}).

try_once_more_or_fail(NewVersion, Nid, Trytimes, Group) ->
    case Trytimes of 3 -> % this makes third try and fail
            lager:notice("UPDATE FAIL after 3 tries for firmware ~p for Node ~p~n", [NewVersion, Nid]),
            lager:notice("WARNING!!STOP UPDATES for FAULTY firmware ~p~n!!!!!", [NewVersion]),
            Left = gproc:get_value({n,l,NewVersion}),
            NodesNotUpdated = find_nodes_not_updated(NewVersion, Group),
            lager:notice("These nodes have not been updated ~p~n",[[Left | NodesNotUpdated]]),
            unregister_when_all_fails(NewVersion, NodesNotUpdated, Left);
        _-> legacy_fw_worker:send_to_nodes('true', NewVersion, [Nid], []),
            tempcache:dets_fw_delete(Nid),
            tempcache:do_dets({Nid, NewVersion, Trytimes +1, Group, unknown}),
            lager:notice("Trying ~pth time for node ~p~n",[Trytimes+1, Nid])
    end.

%NOI: [{<0.2177.0>,["N0e1","N0e2","N0e3","N0e4","N0e5","N0e6","N0e7","N0e8","N0e9","N0e10"]}]
yes_node_of_interest(Version, Nodeid, NodeVersion) ->
    NeedOta = gproc:where({n,l,Version}),
    lager:debug("NeedOta: ~p for version: ~p ~n",[NeedOta, Version]),
    case NeedOta of undefined -> [];
        _-> [{_, NodesOfInterest}] = gproc:lookup_values({n,l,Version}),
            lager:debug("NOI: ~p, Nid: ~p, logged in, we are working with version: ~p~n",[NodesOfInterest, Nodeid,  Version]),
            Nodeidd = case is_binary(Nodeid) of true -> binary_to_list(Nodeid); _-> Nodeid end,
            case lists:member(Nodeidd, NodesOfInterest) orelse lists:member(Nodeid, NodesOfInterest) of true ->
                process_node( Nodeidd, NodeVersion);
            _->lager:debug("this node is not our concern ~p~n",[Nodeidd])
            end
    end.

find_nodes_not_updated(NewVersion, Group) ->
    [{_,Tensents}] = tempcache:dets_fetch_fw(NewVersion),
    {NodeIdsBy10, _WhichList} = Tensents,
    [lists:nth(N,NodeIdsBy10) || N <- lists:seq(Group, length(NodeIdsBy10))].

unregister_when_all_fails(NewVersion, NodesNotUpdated, Left) ->
    gproc:unreg({n,l,NewVersion}),
    gproc:unreg({p,l,version}),
    tempcache:dets_fw_delete(NewVersion),
    [tempcache:dets_fw_delete(Nod) || Nod <- [Left | NodesNotUpdated]].

handle_info({hello,Msg}, State) ->
    lager:debug("You said: ~p and I say YAY~n", [Msg]),
    {noreply, State};

%Node = #{"nid" => Nid, "type" => Clitype, "fw" => Swid}, %"v4-6ddde99" 
handle_info({new_login,{Nodeid,Msg}}, State) ->
    lager:debug("NODE_BOOTS_GEEWHIZ: ~p  logged in, are we interested?~n",[Nodeid]),   
    {'LoginReq',_Nid,_Protov,Clitype1,Swid,_Wifi,_Prf,_Chan,_ConfigT,_Ip,_Time,_Bssid,_Mac,_Auth} = Msg,  
    %lager:debug("gproc table in login: ~p~n",[ets:tab2list(gproc)]),
    Pid =  gproc:lookup_pids({p,l,version}),
    lager:debug("is version in gproc? ~p~n",[Pid]),
    case Pid of []
     -> lager:debug("No OTA is going on, let it go"); 
        _-> Value = gproc:lookup_values({p,l,version}),  %[{<0.2186.0>,"v4-6ddde99"}]
            [{_, Version}] = Value,
            Clitype = lists:nth(2,string:tokens(Clitype1,"-")),
            NodeVersion = Clitype ++ "-" ++ Swid,
            yes_node_of_interest(Version,Nodeid, NodeVersion)
    end,
    {noreply, State};

handle_info({send_to_nodes,{FilesAreHere,Nodeids, Version, UnpackedMsgs}}, State) ->
    lager:debug("Nid: ~p, with Version:~p send_to_nodes ~n",[Nodeids, Version]),
    {NodeIdsByTen, NewNodes} = chunk_nodes(Nodeids, Version), 
    First10 = lists:nth(1,NodeIdsByTen),
    lager:debug("Tries: 1 in send_to_nodes: nodes:~p~n",[First10]),
    legacy_fw_worker:send_to_nodes(FilesAreHere, Version,First10 , UnpackedMsgs),
    register_version(Version, First10),   
    NodesToUpdate = helpers:flatten3(NewNodes),
    cache_version_details(Version, NodesToUpdate, NodeIdsByTen,Nodeids),
    {noreply,State};

handle_info({ota_sent,{Nid, Version}}, State) ->
    lager:debug("Nid: ~p, Version:~p ota_sent~n",[Nid,Version]),
    {noreply, State};

handle_info({ota_downloaded, {Nodeid, FwId,FileName}}, State) ->
    lager:debug("Nid: ~p, Fwid:~p with File:~p ota downloaded",[Nodeid, FwId, FileName]),
    {noreply, State};

handle_info({ten_sent,{Nodeids, Version}}, State) ->
     lager:debug("Send firmware to nodes: ~p, with Version:~p, sending next 10",[Nodeids, Version]),
     %[{"v4-6ddde99",{[[<<"N0e1">>,<<"N0e2">>],[<<"N0e3">>,<<"N0e4">>],[<<"N0e5">>]],2}}]
     [{_,Tensents}] = tempcache:dets_fetch_fw(Version),
     Nodes = gproc:lookup_value({n,l,Version}),  %[{<0.2186.0>,"v4-6ddde99"}]
     WhichNodes = [F || F <- Nodes, lists:member(F,Nodeids) == false],
     lager:debug("Tensents: ~p~n",[Tensents]),
     send_next_group_if_not_all_sent(Version,WhichNodes,Tensents),
     lager:info("Uploaded Firmware ~p to all the nodes succesfully.",[Version, Tensents]),
     {noreply, State};

handle_info({ready_for_next_batch,{WhichGroup, Version}}, State) ->
     lager:debug("Group: ~p, with Version:~p send_to_nodes, sending next 10",[WhichGroup, Version]),
     [{_,Tensents}] = tempcache:dets_fetch_fw(Version),    
     {NodeIdsBy10, _WhichList} = Tensents,
     Len = length(NodeIdsBy10),
     case WhichGroup =< Len of true ->
        NextTen = lists:nth(WhichGroup,NodeIdsBy10),
        lager:debug("Group: ~p send_to_nodes, sending next 10",[NextTen]),
        legacy_fw_worker:send_to_nodes('true', Version, NextTen , []),
        try gproc:unreg({n,l,Version}), gproc:reg({n,l,Version},NextTen) 
            catch error:badarg -> {lager:debug("error registering gproc version~p for ~p~n",[Version, NextTen])} end;
     _-> lager:notice("Uploaded Firmware ~p to all the nodes ~p succesfully.",[Version, NodeIdsBy10])
     end,
     {noreply, State};

handle_info(_Info, State) ->
    {'noreply', State}.

handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

handle_cast(_Msg, State) ->
    {'noreply', State}.
terminate(Reason, _State) ->
    lager:debug("Reason for terminating node_boots: ~p~n",[Reason]),
    [gproc:unreg({p, l, E}) || E <- ?EVENTS_TO_SUBSCRIBE ].  

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

register_version(Version, Nodeids) ->
try gproc:reg({n,l,Version},Nodeids) catch error:badarg -> {lager:error("error registering gproc for firmware:~p and nodes~p~n",[Version,Nodeids])} end,
try gproc:reg({p,l,version},Version) catch error:badarg -> {lager:debug("error registering gproc version for ~p~n",[Version])} end.


 %[{<<"N0e3">>,"v4-ca0584a",0,2},{<<"N0e4">>,"v4-ca0584a",0,2}],[{<<"N0e5">>,"v4-ca0584a",0,3}]]
chunk_nodes(Nodeids, Version) ->
    NodeIdsBy10 = helpers:n_length_chunks_fast(Nodeids, ?BATCH_NUM),
    Len = length(NodeIdsBy10),
    lager:debug("NODEIDSBY10: ~p, send_to_nodes~n",[NodeIdsBy10]),
    NewNodes = [[case Group of 1 -> {Nid,Version,1, Group, unknown}; _->{Nid,Version,0, Group, unknown} end || Nid <- lists:nth(Group,NodeIdsBy10)]|| Group <- lists:seq(1,Len)],
    {NodeIdsBy10,NewNodes}.

cache_version_details(Version, NodesToUpdate, NodeIdsByTen, Nodeids) ->
    tempcache:dets_fw_delete(Version),
    [tempcache:dets_fw_delete(Nid) || Nid <- Nodeids],
    tempcache:do_dets(NodesToUpdate),
    tempcache:do_dets(Version,{NodeIdsByTen,1}).

send_next_group_if_not_all_sent(Version,WhichNodes,Tensents) ->
    {NodeIdsBy10, WhichList} = Tensents,
     Len = length(NodeIdsBy10),
    case (WhichList =< Len) andalso (length(WhichNodes)>0) of true->
        NewTenSents = {NodeIdsBy10, WhichList + 1},
        tempcache:dets_fw_delete(Version),
        tempcache:dets_replace_fw(Version, NewTenSents),
        gproc:unreg({p,l,Version}),
        gproc:reg({p,l,Version},NewTenSents);
     _-> lager:info("Uploaded Firmware ~p to all the nodes succesfully.",[Version, Tensents])
     end.
    