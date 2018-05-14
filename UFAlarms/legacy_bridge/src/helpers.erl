-module(helpers).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).
-define(CONNECTION_STATUS, <<"ConnectionStatus">>).
-define(STATUS_EXT, <<"node/status">>).
-define(DISCONNECTED, <<"disconnected">>).
-define(CONNECTED, <<"connected">>).
-define(DCC,'device_command_center@127.0.0.1').
-define(POOLNAME, legacy_zmq_pool).

listOrd1(Searched, List) when is_list(List) ->
    listOrd1(Searched, List, 1).

listOrd1(_, [], _) -> nil;

listOrd1(Searched, [ Searched | _ ], Count) ->
    Count;

listOrd1(Searched, [ _ | Tail ], Count) ->

listOrd1(Searched, Tail, Count + 1).

now_us({MegaSecs,Secs,MicroSecs}) ->
  (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

%% Work from the back and keep a tail of things done.
flatten3(L) -> flatten3(L, []).

flatten3([H|T], Tail) ->
    flatten3(H, flatten3(T, Tail));
flatten3([], Tail) -> Tail;
flatten3(E, Tail) -> [E|Tail].

%% Lift nested lists to the front of the list.
flatten1([[H|T1]|T2]) -> flatten1([H,T1|T2]);
flatten1([[]|T]) -> flatten1(T);
flatten1([E|T]) -> [E|flatten1(T)];
flatten1([]) -> [].

%% Keep a list of things todo and put tails onto it.
flatten2(L) -> flatten2(L, []).

flatten2([H|T], Todo) ->
    flatten2(H, [T|Todo]);
flatten2([], [H|Todo]) -> flatten2(H, Todo);
flatten2([], []) -> [];
flatten2(E, Todo) -> [E|flatten2(Todo, [])].

%% setnth(Index, List, NewElement) -> List.

setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

%% Can add following caluse if you want to be kind and allow invalid indexes.
%% I wouldn't!
%% setnth(_, [], New) -> New.

topic_tokens(Topic) ->
    TT = binary_to_list(Topic),
    string:tokens(TT,"/").

%%["TopicA","Cisco","EmeryVille","N01234","req","Login"]
%%Drop last and append "resp" for the response queue to publish to.
topic_from_tokens(Tokens,Ext)->
    Resptopic1 = helpers:setnth(5,Tokens,Ext),
    Resptopic = string:join(Resptopic1, "/"),
    lager:debug("resptopic:~p~n",[Resptopic]),
    Resptopic.

build_resptopic(Topic,Ext)->
    Tokens = topic_tokens(Topic),
    topic_from_tokens(Tokens,Ext).

binarize_if_string(SomeElement) when is_atom(SomeElement)->
  atom_to_binary(SomeElement, unicode);

binarize_if_string(SomeElement) when is_list(SomeElement) ->
  case io_lib:printable_list(SomeElement) of
  true -> iolist_to_binary(SomeElement);
  _-> SomeElement
  end;

binarize_if_string(SomeElement)->
  SomeElement.

binarizeliststrings(L) ->
  lists:map(fun(X) ->
    S1 = element(1,X), S2 = element(2,X),
    %lager:debug("S1: ~p~n",[S1]),
    S22 = case is_list(S2) of true -> list_to_binary(S2); _-> S2 end,
    S11 = case is_list(S1) of true -> list_to_binary(S1); _-> S1 end,
    %lager:debug("S11: ~p, S22: ~p~n",[S11, S22]),
    {S11, S22}
   end, L).

unbinarizeliststrings(L) ->
  lists:map(fun(X) ->
    case is_binary(X) of true -> list_to_existing_atom(binary_to_list(X)); _-> X end
   end, L).

etsit(K,V) ->
  io:format("K, V : ~p,~p~n",[K,V]),
  spawn(fun() ->
    case lists:member(nodes,ets:all()) of
      true -> io:format("nodes exists"), ets:insert(nodes, {K,V});
      _ -> io:format("no nodes"),ets:new(nodes,[named_table,public]), ets:insert(nodes, {K,V}) end
    end).

send_email(Subject, Body) ->
  Email = {
    "ghagg@sensity.com",
    ["ghagg@sensity.com"],
    list_to_binary(["From: GH <ginahagg@gmail.com>\r\nTo: GH <ghagg@sensity.com>\r\nSubject: ",Subject,"\r\n\r\n",Body])
},
Options = [
    {ssl,true},
    {no_mx_lookups,true},
    {relay,"smtp.gmail.com"},
    {username,"ginahagg@gmail.com"},
    {password,"xxxx"},
    {auth,always}
],
gen_smtp_client:send_blocking(Email, Options).

star_translations({{Y,M,D},{H,Min,Sec}}) ->
  Yr = case  Y =:= '*' of true -> undefined; _-> Y end,
  Mo = case M =:= '*' of true -> undefined; _-> M end,
  Day = case D =:= '*' of true -> undefined; _-> D end,
  Hr = case H =:= '*' of true -> 0; _-> H end,
  Minn = case Min =:= '*' of true -> 0; _-> Min end,
  Secc = case Sec =:= '*' of true -> 0; _-> Sec end,
  {{Yr, Mo, Day},{Hr, Minn, Secc}}.

translate_calendar({{Y,M,D},{H,Min,Sec}}) ->
  {{Yr, Mo, Day},{Hr, Minn, Secc}} = star_translations({{Y,M,D},{H,Min,Sec}}),
  {Hr,Minn,Secc,undefined,Day,Mo,Yr};

translate_calendar({{Y,M,D},Day,{H,Min,Sec}}) ->
  {{Yr, Mo, Day},{Hr, Minn, Secc}} = star_translations({{Y,M,D},{H,Min,Sec}}),
  Days = [{sun,1},{mon,2},{tue,3},{wed, 4},{thu,5},{fri,6}, {sat,7}],
  Whichday = proplists:get_value(Day, Days),
  {Hr,Minn,Secc,Whichday,Day,Mo,Yr}.

%{{2015,12,14},{6,19,10}}
utc_microsecs() ->
  (calendar:datetime_to_gregorian_seconds(calendar:universal_time()) * 1000000) - (719528*24*3600*1000*1000).

e_type_string(S, Bin) ->
    Utf8 = unicode:characters_to_binary(S),
    Bin2 = e_varint(byte_size(Utf8), Bin),
    io:format("Bin2: ~p~n",[Bin2]),
    <<Bin2/binary, Utf8/binary>>.

e_varint(N, Bin) when N =< 127 -> L = <<Bin/binary, N>>, io:format("Bin/binary,N is ~p~n",[L]), L;
e_varint(N, Bin) ->
    Bin2 = <<Bin/binary, (N band 127 bor 128)>>,
    lager:debug("Bin2: ~p~n",[Bin2]),
    e_varint(N bsr 7, Bin2).

%%I am not sure if we need a response for LoginResp. I think this is what is created
%%when we answer a login req. I don't think it comes from device. Clarify if we login to a device??

fill_missing_hdr_fields(Data1) ->
    Data = binary_to_list(Data1),
    IsHostMissing = string:rstr(Data,"Host"),
    io:format("isHostMissing: ~p~n", [isHostMissing]),
  case IsHostMissing of
          0 ->
            BlankHdrPos = string:rstr(Data,"\r\n\r\n"),
            io:format("BlankHdrPos: ~p~n", [BlankHdrPos]),
            StripHdr =  string:substr(Data,1,BlankHdrPos + 1),
            io:format("StripHdr: ~p~n", [StripHdr]),
            NewHdr = list_to_binary([StripHdr,"Host: ensense-2.sensity.com:10443\r\nUser-Agent: spray-can/1.3.2-SNAPSHOT\r\n\r\n"]),
            io:format("isHostMissing: ~p~n", [NewHdr]),
            NewHdr;
          _-> Data1
  end.

init_gproc_register(Nodeid) ->
  case gproc:lookup_pids({n,l,Nodeid}) of [] ->
    lager:debug("node is not registered for ~p~n",[Nodeid]),
    gproc:reg({n,l,Nodeid});
  _->
    gproc:unreg({n,l,Nodeid}),
    lager:debug("node is registered for , unregister first ~p~n",[Nodeid]),
    gproc:unreg_shared({n, l, Nodeid})
  end,
  gproc:reg({p,l,keepalive}).

send_disconnected(Nodeid,Con,Prefix) ->
  CStat = #{<<"name">> => ?CONNECTION_STATUS, <<"nodeid">> => Nodeid , <<"status">> => ?DISCONNECTED},
  lager:debug("Node DISCONNECTED!!:CStat: ~p~n",[CStat]),
  PackStat = msgpack:pack(CStat),
  StatTopic = list_to_binary([Prefix,Nodeid,"/", ?STATUS_EXT]),
  emqttc:publish(Con, StatTopic, PackStat, 1).

send_disconnected(Nodeid) ->
  CStat = #{<<"name">> => ?CONNECTION_STATUS, <<"nodeid">> => Nodeid , <<"status">> => ?DISCONNECTED},
  lager:debug("Node DISCONNECTED!!:CStat: ~p, sending directly to datadealer~n",[CStat]),
  %M = binary_to_list(Nodeid) ++ " is disconnected!!",
  Packed = msgpack:pack(CStat),
  send_to_dd_login(Packed).
  %{commandcenter, ?DCC} ! {legacy, M, Packed}.

send_connected(Nodeid,Con,Prefix) ->
  CStat = #{<<"name">> => ?CONNECTION_STATUS, <<"nodeid">> => Nodeid , <<"status">> => ?CONNECTED},
  lager:debug("Node CONNECTED!!:CStat: ~p~n",[CStat]),
  PackStat = msgpack:pack(CStat),
  StatTopic = list_to_binary([Prefix,Nodeid,"/", ?STATUS_EXT]),
  emqttc:publish(Con, StatTopic, PackStat, 1).

send_connected(Nodeid) ->
  CStat = #{<<"name">> => ?CONNECTION_STATUS, <<"nodeid">> => Nodeid , <<"status">> => ?CONNECTED},
  lager:debug("Node CONNECTED!!:CStat: ~p, sending directly to datadealer! ~n",[CStat]),
  Packed = msgpack:pack(CStat),
  %M = binary_to_list(Nodeid) ++ " is connected!!",
  send_to_dd_login(Packed).


send_connection_status(Status,Nodeid, Prefix) ->
  Con = gproc:lookup_value({n,l,mqttc}),
  CStat = #{<<"name">> => ?CONNECTION_STATUS, <<"nodeid">> => Nodeid , <<"status">> => Status},
  lager:debug("send_connection_status:CStat: ~p~n",[CStat]),
  PackStat = msgpack:pack(CStat),
  StatTopic = list_to_binary([Prefix,Nodeid,"/", ?STATUS_EXT]),
  lager:debug("sending connection status to mqtt for Nodeid: ~p, status ~p to topic ~p~n",[Nodeid, Status, StatTopic]),
  %legacy_mqtt_agent:publish_to_mqtt(PackStat,StatTopic).
  emqttc:publish(Con, StatTopic, PackStat, 1).

n_length_chunks_fast(List,Len) ->
    SkipLength = case length(List) rem Len of
        0 -> 0;
        N -> Len - N
    end,
    n_length_chunks_fast(lists:reverse(List),[],SkipLength,Len).

n_length_chunks_fast([],Acc,_Pos,_Max) -> Acc;

n_length_chunks_fast([H|T],Acc,Pos,Max) when Pos==Max ->
    n_length_chunks_fast(T,[[H] | Acc],1,Max);

n_length_chunks_fast([H|T],[HAcc | TAcc],Pos,Max) ->
    n_length_chunks_fast(T,[[H | HAcc] | TAcc],Pos+1,Max);

n_length_chunks_fast([H|T],[],Pos,Max) ->
    n_length_chunks_fast(T,[[H]],Pos+1,Max).

send_to_dd(Packed)->
    gen_server:cast('zmqpushclient', {'zmqsend', Packed}).

send_to_dd_login(Packed)->
    gen_server:cast('zmqpushclient', {'zmqsend', Packed}).

send_to_dd_transaction(PoolName,Packed) ->
  %Worker = poolboy:checkout(PoolName, true, infinity),
  %gen_server:call(Worker, {zmqsend, Packed}, infinity),
  %poolboy:checkin(PoolName, Worker).
  poolboy:transaction(PoolName, fun(Worker) ->
    lager:debug("Worker in transaction~n"),
    gen_server:call(Worker, {zmqsend, Packed}, 10000) end).

find_subscriber(MsgType) ->
  Subs = [{<<"LightingForceState">>, "lighting"}, {"LightingForceState", "lighting"},
  {<<"LightingSetAuto">>, "lighting"},{"LightingSetAuto", "lighting"},{<<"LightingClearSchedule">>, "lighting"}, {"LightingClearSchedule", "lighting"},
  {<<"LightingScheduledEvent">>, "schedules"}, {"LightingScheduledEvent", "schedules"},
  {<<"SoftwareUpdateReq">>, "firmware"}, {"SoftwareUpdateReq", "firmware"},{<<"AssignFirmware">>, "firmware"},{"AssignFirmware", "firmware"},
  {<<"SensorSampleReq">>, "sensor"}, {"SensorSampleReq", "sensor"},{"VideoUploadReq", "video"},
  {<<"DeviceActionReq">>, "device"}, {"DeviceActionReq", "device"},
  {<<"VideoUploadReq">>, "video"},{<<"ConfigResp">>, "config"},{"ConfigResp", "config"}],
  proplists:get_value(MsgType, Subs).

notify_delivery_error(Nodeid, Msg, MsgType) ->
  Map = #{<<"name">> => <<"failed-delivery">>, <<"nodeid">> => Nodeid, <<"msg">> => Msg},
  P = msgpack:pack(Map),
  Topic = list_to_binary(["/streamv1/faileddelivery/legacy/", Nodeid, "/", MsgType]),
  gen_server:cast(legacy_mqtt_agent,{'publish_to_mqtt', P, Topic}).

fetch_WsPid_for_node(Nodeid) ->
    Pid = gproc:where({n, l, Nodeid}),
    case Pid of
        undefined ->
            %let's make sure and ask the nodes to identify themselves,
            Pids = gproc:lookup_pids({p,l,keepalive}),
            [PID ! {identify_yourself} || PID <- Pids],
            %%wait a bit (1millisec) for nodes to identify themselves
            timer:sleep(1),
            %ask again, by now they should have identified themselves
            gproc:where({n, l, Nodeid});
        _ -> Pid
    end.

%{10,2,26,157} -> 10.2.26.157
get_host_addr() ->
  {ok,Ip} =  application:get_env(legacy_bridge, host),
  lager:debug("Ip ~p",[Ip]),
  Ip.

get_connected() ->
  MatchHead1 = '_',
    Guard = [],
    Result = ['$$'],
    Vals =  gproc:select(names,[{MatchHead1, Guard, Result}]),
    [element(3,lists:nth(1,Y)) || Y <- Vals, lists:nth(1,Y) =/= {n,l,mqttc}].

get_best_pid(Group) ->
  Members = pg2:get_members(Group),
  Members1 = lists:map(fun(Pid) ->
      [{message_queue_len, Messages}] = erlang:process_info(Pid, [message_queue_len]),
      {Pid, Messages}
    end, Members),
  case lists:keysort(2, Members1) of
    [{Pid, _} | _] -> Pid;
    [] -> {error, empty_process_group}
  end.

get_queue_len(Name) ->
    {'message_queue_len', Length} = erlang:process_info(whereis(Name), 'message_queue_len'),
    Length.


send_to_least_busy(Group, Packed) ->
  Which = get_best_pid(Group),
  %lager:debug("GINAAAA BEST PID ~p for group ~p~n",[Which, Group]),
  gen_server:cast(Which,{zmqsend, Packed}).
