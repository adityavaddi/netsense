-module(tester).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).

video_test() ->
Topic = iolist_to_binary(["global/va-v1/cfg/login"]),
Msg = <<134,165,102,116,121,112,101,168,86,111,108,97,116,105,108,101,165,108,101,118,101,108,0,164,109,97,115,107,1,164,110,97,109,101,178,76,105,103,104,116,105,110,103,70,111,114,99,101,83,116,97,116,101,163,112,114,105,3,170,113,117,97,108,105,102,105,101,114,115,169,117,110,100,101,102,105,110,101,100>>,
{ok,D} = emqttc:start_link([{client_id, <<"videologin">>},{username,<<"node">>,{password,<<"kentspeed">>}}]),        
emqttc:publish(D,Topic, Msg, [{qos, 1}]).

allcfgtest() ->
Topic = iolist_to_binary(["N02c00073/va-v1/cfg/e5d3cb47-6e71-3067-8999-a695c3c46b44/inventory"]),
M = #{cfgs => []},
Msg = msgpack:pack(M),
{ok,D} = emqttc:start_link([{host,"devmqtt.sensity.com"}, {port,8883},{client_id, <<"videologin">>},{username,<<"node">>,{password,<<"kentspeed">>,{cacertfile,"priv/ssl/sensity.ca.cer"}}}]),        
emqttc:publish(D,Topic, Msg, [{qos, 1}]).

lt_off(Nodeid) ->
  Lf = #{"name" => "LightingForceState","pri" => 3, "mask" => 1,"level" => 0,
  "qualifiers" => "undefined","ftype" => "Volatile", "nodeid" => [Nodeid]},
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, "lighting", Lf}.

lt_20(Nodeid) ->
  Map = #{"name" => "LightingForceState","pri" => 3, "mask" => 1,"level" => 20,
  "qualifiers" => "undefined","ftype" => "Volatile", "nodeid" => [Nodeid]},
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, "lighting", Map}.

lt_100(Nodeid) ->
  Map = #{"name" => "LightingForceState","pri" => 3, "mask" => 1,"level" => 100,
  "qualifiers" => "undefined","ftype" => "Volatile", "nodeid" => [Nodeid]},
 {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, "lighting", Map}.

get_test_resp() ->
  Timestamp = helpers:now_us(os:timestamp()),
  #{"name" =>"LoginResp","ok" => true,"ts" => Timestamp}.

cfgjsxdata()->
  #{"name"=>"ConfigResp","key" => "sensor.pc.pint","svalue" =>"undefined","lvalue"=>3600000,
  "bvalue"=>"undefined"}.


%%this: #{"name"=>"ConfigResp","key" => "sensor.pc.pint","svalue" =>"undefined","lvalue"=>3600000,"bvalue"=>"undefined"}.
%%to this:"{\"bvalue\":\"undefined\",\"key\":\"sensor.pc.pint\",\"lvalue\":3600000,\"name\":\"ConfigResp\",\"svalue\":\"undefined\"}"
cfgjsx() ->
  B = cfgjsxdata(),
  jsx:encode(B).


cfgmapdata() ->
  [#{"name"=>"ConfigResp","key" => "sensor.pc.pint","svalue" =>"undefined","lvalue"=>3600000,"bvalue"=>"undefined"},
  #{"name"=>"ConfigResp","key" => "sensor.rf.pint","svalue" =>"undefined","lvalue"=>3600000,"bvalue"=>"undefined"},
  #{"name"=>"ConfigResp","key" =>"sensor.rf.dint","svalue" =>"undefined","lvalue"=>30000,"bvalue"=>"undefined"},
  #{"name"=>"ConfigResp","key" =>"sensor.v.mode","svalue" =>"undefined","lvalue"=>1,"bvalue"=>"undefined"},
  #{"name"=>"ConfigResp","key" =>"sensor.aw.pint","svalue" =>"undefined","lvalue"=>600000,"bvalue"=>"undefined"},
  #{"name"=>"ConfigResp","key" =>"sensor.ai.mode","svalue" =>"undefined","lvalue"=>1,"bvalue"=>"undefined"},
  #{"name"=>"ConfigResp","key" =>"sensor.ai.dint","svalue" =>"undefined","lvalue"=>30000,"bvalue"=>"undefined"},
  #{"name"=>"ConfigResp","key" =>"sensor.ai.pint","svalue" =>"undefined","lvalue"=>3600000,"bvalue"=>"undefined"},
  #{"name"=>"ConfigResp","key" =>"sensor.v.dint","svalue" =>"undefined","lvalue"=>30000,"bvalue"=>"undefined"},
  #{"name"=>"ConfigResp","key" =>"sensor.v.pint","svalue" =>"undefined","lvalue"=>3600000,"bvalue"=>"undefined"},
  #{"name"=>"ConfigResp","key" =>"sensor.rf.mode","svalue" =>"undefined","lvalue"=>1,"bvalue"=>"undefined"}].

eztest() ->
    application:start(sasl),
    application:start(gen_listener_tcp),
    application:start(ezmq),
    Port = 5555,
    {ok, Socket} = ezmq:start([{type, rep}]),
    ezmq:bind(Socket, tcp, Port, []),
    loop(Socket).

loop(Socket) ->
    ezmq:recv(Socket),
    io:format("Received Hello~n"),
    ezmq:send(Socket, ["World"]),
    loop(Socket).

%%this :#{"name"=>"gina","lname"=>"hagg"}
%%tothis: "#{\"lname\" => \"hagg\",\"name\" => \"gina\"}"
termtostring()->
A = #{"name"=>"gina","lname"=>"hagg"},
R= io_lib:format("~p",[A]) ,
lists:flatten(R).

go_ets(N) ->
    go_ets(ets:new(ets_test, [private]), N).

go_ets(Ets, 0) ->
    Ets;
go_ets(Ets, N) ->
    Key = lists:concat(["key-", N]),
    Val = {state, 12345, "this is a string", [1,2,3,4], 45.6789, atom1, atom2},
    ets:insert(Ets, {Key, Val}),
    go_ets(Ets, N - 1).

spawn_many(0, _Interval)-> done;
spawn_many(N, Interval)->
    Mp = <<139,172,97,115,115,111,99,99,104,97,110,110,101,108,48,168,99,102,103,116,111,107,101,110,168,102,49,98,53,100,52,56,49,170,99,108,105,101,110,116,116,121,112,101,168,117,110,111,100,101,45,118,52,162,105,112,172,49,48,46,50,48,46,49,48,57,46,51,52,164,110,97,109,101,168,76,111,103,105,110,82,101,113,166,110,111,100,101,105,100,169,78,48,49,50,51,50,101,97,53,171,112,114,111,102,105,108,101,110,97,109,101,161,120,168,112,114,111,116,111,118,115,110,0,164,115,115,105,100,166,88,101,114,97,76,50,167,115,119,118,115,110,105,100,167,50,98,57,102,48,50,99,162,116,115,207,0,5,34,4,185,1,23,28>>,
    Pid = spawn(?MODULE,withinterval,[Mp, Interval]),
    lager:info("spawned PID: ~p~n",[Pid]),
    spawn_many(N - 1, Interval).

withinterval(Mp,Interval)->
    {ok, _TRef} = timer:apply_interval(timer:seconds(Interval),zmqpushclient, zmqsend,[Mp]),
    timer:sleep(2000),
    receive
          "stop" -> exit(normal)
    end.

testzmqlt() ->
    Msg = <<134,165,102,116,121,112,101,168,86,111,108,97,116,105,108,101,165,108,101,118,101,108,0,164,109,97,115,107,1,164,110,97,109,101,178,76,105,103,104,116,105,110,103,70,111,114,99,101,83,116,97,116,101,163,112,114,105,3,170,113,117,97,108,105,102,105,101,114,115,169,117,110,100,101,102,105,110,101,100>>,
    Dcclistener =  "tcp://127.0.0.1:6542",
    lager:info("erlzmqclient=>Mapmsg for zmqsend ~p~n", [Msg]),
    {ok, Ctx} = erlzmq:context(),
    {ok, Reqsocket} = erlzmq:socket(Ctx, [req, {active, false}]),
        case  erlzmq:connect(Reqsocket, Dcclistener) of
            ok ->
                erlzmq:send(Reqsocket, Msg),
                {ok, Reply} = erlzmq:recv(Reqsocket),
                lager:info("Received packed reply to our zmqsend ~p~n", [Reply]),
                M = msgpack:unpack(Reply),
                lager:info("Unpacked reply is ~p~n", [M]);
            Error ->
                lager:warning("erlzmqClient=> reqsocket failed to connect with error: ~p~n", [ Error])
        end,
    erlzmq:close(Reqsocket),
    erlzmq:term(Ctx).

lightmaps() ->
  Defaulton = #{"name" => "lightschedule", "nodeid"=> "N01232ea5","id"=> 1, "year"=> 0,"mon"=>0, "mday" => 0, "wday" => 0,
   "sec" => 0,"min" => 30, "hr" => 17, "pri"=> 3 , "mask" => 1,"level" => 0, "qualifiers" => 0},
  Defaultoff = #{"name" => "lightschedule", "nodeid"=> "N01232ea5","id"=> 1, "year"=> 0,"mon"=>0, "mday" => 0, "wday" => 0,
   "sec" => 0,"min" => 0, "hr" => 6, "pri"=> 3 , "mask" => 1,"level" => 0, "qualifiers" => 0},
  Defaultdimon = #{"name" => "lightschedule", "nodeid"=> "N01232ea5","id"=> 1, "year"=> 0,"mon"=>0, "mday" => 0, "wday" => 0,
   "sec" => 0,"min" => 0, "hr" => 2, "pri"=> 3 , "mask" => 1,"level" => 0, "qualifiers" => 0},
  Defaultdimoff = #{"name" => "lightschedule","nodeid"=> "N01232ea5", "id"=> 1, "year"=> 0,"mon"=>0, "mday" => 0, "wday" => 0,
   "sec" => 0,"min" => 0, "hr" => 4, "pri"=> 3 , "mask" => 1,"level" => 0, "qualifiers" => 0},
  [{defaulton, Defaulton}, {defaultoff, Defaultoff}, {defaultdimon, Defaultdimon}, {defaultdimoff, Defaultdimoff}].

get_schedules(_Nodeid) ->
 [{nodeid, "N01232ea5"},
 [ [{sheduleid, 1}, {type, defaultoff}, {calendar, {{6, 0, 0}}},{pri, 2},{mask,0}, {level,0}, {qualifiers, 0},
  {msg, #{"name" =>"LightingScheduledEvent", "nodeid"=> "N01232ea5", "id"=> 1, "year"=> 0,
  "mon"=>0, "mday" => 0, "wday" => 0,
   "sec" => 0,"min" => 30, "hr" => 17, "pri"=> 3 , "mask" => 1,"level" => 0, "qualifiers" => 0}}],
 [{sheduleid, 2}, {type, defaulton},{calendar, {{17, 30, 0}}},{pri, 2},{mask,0}, {level,1}, {qualifiers, 0},
 {msg, #{"name" => "LightingScheduledEvent", "nodeid" => "N01232ea5", "id"=> 1, "year"=> 0,"mon"=>0, "mday" => 0, "wday" => 0,
   "sec" => 0,"min" => 10, "hr" => 6, "pri"=> 3 , "mask" => 1,"level" => 1, "qualifiers" => 0}}]
 %[{sheduleid, 3}, {type, defaultdimon},{calendar,{{'*','*','*'}, [fri, sat], {2,0,0}}},{pri, 2},{mask,0}, {level,<0>}, {qualifiers, 0},
 %{msg, #{"name" =>"lightschedule", "nodeid"=> "N01232ea5", "id"=> 1, "year"=> 0,"mon"=>0, "mday" => 0, "wday" => 0
 %   "sec" => 0,"min" => 10, "hr" => 6, "pri"=> 3 , "mask" => 1,"level" => 0, "qualifiers" => 0}}]
 %[{sheduleid, 4}, {type, defaultdimoff},{calendar,{{'*','*','*'}, [fri, sat], {4,0,0}}},{pri, 2},{mask,0}, {level,<0>}, {qualifiers, 0},
 %{msg, #{"name" =>"lightschedule", "nodeid"=> "N01232ea5","id"=> 1, "year"=> 0,"mon"=>0, "mday" => 0, "wday" => 0
 %   "sec" => 0,"min" => 10, "hr" => 6, "pri"=> 3 , "mask" => 1,"level" => 0, "qualifiers" => 0}}]
 ]
 ].

testdeviceaction(Nodeid, Action) ->
  Map = #{"name" => "DeviceActionReq", "nodeid" => [Nodeid], "action" => Action},
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, "device", Map}.

schedrawdata() -> %for more_than_8_scheds
<<131,164,110,97,109,101,182,76,105,103,104,116,105,110,103,83,99,104,101,100,117,108,101,100,69,118,101,110,116,166,110,111,100,101,105,100,145,169,78,48,49,51,51,52,49,101,53,169,115,99,104,101,100,117,108,101,115,220,0,17,136,162,104,114,5,162,105,100,5,165,108,101,118,101,108,100,164,109,97,115,107,1,163,109,105,110,0,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,204,128,163,115,101,99,0,136,162,104,114,7,162,105,100,4,165,108,101,118,101,108,50,164,109,97,115,107,1,163,109,105,110,23,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,0,163,115,101,99,0,136,162,104,114,7,162,105,100,4,165,108,101,118,101,108,0,164,109,97,115,107,1,163,109,105,110,53,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,0,163,115,101,99,0,136,162,104,114,16,162,105,100,5,165,108,101,118,101,108,50,164,109,97,115,107,1,163,109,105,110,43,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,204,128,163,115,101,99,0,136,162,104,114,17,162,105,100,5,165,108,101,118,101,108,100,164,109,97,115,107,1,163,109,105,110,13,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,204,128,163,115,101,99,0,136,162,104,114,23,162,105,100,4,165,108,101,118,101,108,50,164,109,97,115,107,1,163,109,105,110,0,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,0,163,115,101,99,0,139,162,104,114,0,162,105,100,5,165,108,101,118,101,108,100,164,109,97,115,107,1,164,109,100,97,121,31,163,109,105,110,0,163,109,111,110,12,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,204,128,163,115,101,99,0,164,121,101,97,114,205,7,224,139,162,104,114,7,162,105,100,4,165,108,101,118,101,108,0,164,109,97,115,107,1,164,109,100,97,121,31,163,109,105,110,23,163,109,111,110,12,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,0,163,115,101,99,0,164,121,101,97,114,205,7,224,139,162,104,114,17,162,105,100,5,165,108,101,118,101,108,100,164,109,97,115,107,1,164,109,100,97,121,31,163,109,105,110,1,163,109,111,110,12,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,204,128,163,115,101,99,0,164,121,101,97,114,205,7,224,137,162,104,114,5,162,105,100,5,165,108,101,118,101,108,100,164,109,97,115,107,1,163,109,105,110,0,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,204,128,163,115,101,99,0,164,119,100,97,121,7,137,162,104,114,7,162,105,100,4,165,108,101,118,101,108,0,164,109,97,115,107,1,163,109,105,110,51,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,0,163,115,101,99,0,164,119,100,97,121,7,137,162,104,114,17,162,105,100,5,165,108,101,118,101,108,100,164,109,97,115,107,1,163,109,105,110,17,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,204,128,163,115,101,99,0,164,119,100,97,121,7,137,162,104,114,23,162,105,100,4,165,108,101,118,101,108,0,164,109,97,115,107,1,163,109,105,110,0,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,0,163,115,101,99,0,164,119,100,97,121,7,137,162,104,114,5,162,105,100,5,165,108,101,118,101,108,100,164,109,97,115,107,1,163,109,105,110,0,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,204,128,163,115,101,99,0,164,119,100,97,121,1,137,162,104,114,7,162,105,100,4,165,108,101,118,101,108,0,164,109,97,115,107,1,163,109,105,110,53,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,0,163,115,101,99,0,164,119,100,97,121,1,137,162,104,114,17,162,105,100,5,165,108,101,118,101,108,100,164,109,97,115,107,1,163,109,105,110,11,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,204,128,163,115,101,99,0,164,119,100,97,121,1,137,162,104,114,23,162,105,100,4,165,108,101,118,101,108,0,164,109,97,115,107,1,163,109,105,110,0,163,112,114,105,5,170,113,117,97,108,105,102,105,101,114,115,0,163,115,101,99,0,164,119,100,97,121,1>>.
testscheds(Nodeid) ->
  Map = #{"nodeid" => [list_to_binary(Nodeid)], "name" => "LightingScheduledEvent", "schedules" => more_than_8_schedules()},
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, "schedules", Map}.

testnocronscheds(Nodeid) ->
  Map = #{"nodeid" => [list_to_binary(Nodeid)], "name" => "LightingScheduledEvent", "schedules" => nothing_to_cron()},
  Msg = msgpack:pack(Map),
  Ret = command_msg_handler:inspect_adapt_command(Map,Msg),
  {direct, Topic,_NewMsg} = Ret,
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, Topic, Map}.

testadaptscheds(Nodeid) ->
  Map = #{"nodeid" => [list_to_binary(Nodeid)], "name" => "LightingScheduledEvent", "schedules" => very_long_schedules()},
  Msg = msgpack:pack(Map),
  Ret = command_msg_handler:inspect_adapt_command(Map,Msg),
  {direct, Topic,_NewMsg} = Ret,
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, Topic, Map}.

test_schedule(Nodeid,Schedule) ->
  Map = #{"nodeid" => [list_to_binary(Nodeid)], "name" => "LightingScheduledEvent", "schedules" => Schedule},
  %Msg = msgpack:pack(Map),
  %Ret = command_msg_handler:inspect_adapt_command(Map,Msg),
  %{direct, Topic,_NewMsg} = Ret,
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, "schedules", Map}.

testfwins3(Nodeids) ->
  Nodes = case is_list(Nodeids) of true -> Nodeids; _-> [Nodeids] end,
  Map = #{"nodeid" => [Nodes], "name" => "AssignFirmware", "firmwareid" => "v4-ca0584a"},
  Msg = msgpack:pack(Map),
  Ret = command_msg_handler:inspect_adapt_command(Map,Msg),
  lager:info("Ret:~p~n",[Ret]),
  {direct, Topic,_NewMsg} = Ret,
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, Topic, Map}.

testfwnotins3(Nodeids) ->
  Nodes = case is_list(Nodeids) of true -> Nodeids; _-> [Nodeids] end,
  Map = #{"nodeid" => [Nodes], "name" => "AssignFirmware", "firmwareid" => "v4-ca0584a"},
  Msg = msgpack:pack(Map),
  Ret = command_msg_handler:inspect_adapt_command(Map,Msg),
  lager:info("Ret:~p~n",[Ret]),
  {direct, Topic,_NewMsg} = Ret,
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, Topic, Map}.

testdirectfirmware() ->
Lb = 'legacy_bridge@127.0.0.1',
{legacy_mqtt_agent, Lb} ! {dcc,"firmware",#{"firmwareid" => "v4-c6665dd","name" => "AssignFirmware","nodeid" => ["N013341e5"]}}.

testfwinrep(Nodeids, Firmwareid) ->
  Nodes = case is_list(Nodeids) of true -> Nodeids; _-> [Nodeids] end,
  Map = #{"nodeid" => Nodes, "name" => "AssignFirmware", "firmwareid" => Firmwareid},
  Msg = msgpack:pack(Map),
  Ret = command_msg_handler:inspect_adapt_command(Map,Msg),
  lager:info("Ret:~p~n",[Ret]),
  {direct, Topic,_NewMsg} = Ret,
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, Topic, Map}.

testassignfirmware(Nodeid) ->
  Map = #{"nodeid" => [Nodeid], "name" => "AssignFirmware", "firmwareid" => "6339c35"},
  Topic = "firmware",
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, Topic, Map}.

testserverupdate(Nodeid) ->
  Map = #{
  <<"name">>=> <<"ConfigResp">>,
  <<"configtype">> => <<"serverupdate">>,
  <<"nodeid">> => [
    Nodeid
  ],
  <<"kvpairs">>=> #{
    <<"server">> => "chi.sensity.com"
    ,<<"token">> => "60d504aa"
    }
  },
  Msg = msgpack:pack(Map),
  Ret = command_msg_handler:inspect_adapt_command(Map, Msg),
  {direct, Topic,_NewMsg} = Ret,
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, Topic, Map}.

testwifiupdate(Nodeid) ->
  Map = #{
  "name"=> "ConfigResp",
  "configtype" => "wifiupdate",
  "nodeid"=> [
    Nodeid
  ],
  "kvpairs"=> #{
    "network.x.ssid" => "XeraL2"
    ,"token" => "60d504aa"
    }
  },
  Msg = msgpack:pack(Map),
  Ret = command_msg_handler:inspect_adapt_command(Map, Msg),
  {direct, Topic,_NewMsg} = Ret,
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, Topic, Map}.

testconfigupdate(Nodeid) ->
    Map = configfromjs(Nodeid),
    Msg = msgpack:pack(Map),
    Ret = command_msg_handler:inspect_adapt_command(Map, Msg),
    {direct, Topic,_NewMsg} = Ret,
    {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, Topic, Map}.

cfgdata(Nodeid)->
    KVPairs = #{
        "debugmode" => false
        ,"ota.disable" => false
        ,"network.telnet" => true
        ,"sensor.rf.pint" => 3600000
        ,"sensor.rf.dint" => 30000
        ,"sensor.rf.mode" => 1
        ,"sensor.v.pint" => 3600000
        ,"sensor.v.dint" => 30000
        ,"sensor.v.mode" => 1
        ,"sensor.ai.pint" => 3600000
        ,"sensor.ai.dint" => 30000
        ,"sensor.ai.mode" => 1
        ,"sensor.aw.pint" => 600000
        ,"sensor.aw.dint" => 30000
        ,"sensor.aw.mode" => 1
        ,"sensor.aPF.pint" => 3600000
        ,"sensor.aPF.dint" => 30000
        ,"sensor.aPF.mode" => 1
        ,"sensor.mi.pint" => 3600000
        ,"sensor.mi.dint" => 30000
        ,"sensor.mi.mode" => 1
        ,"sensor.mw.pint" => 600000
        ,"sensor.mw.dint" => 30000
        ,"sensor.mw.mode" => 1
        ,"sensor.mPF.pint" => 3600000
        ,"sensor.mPF.dint" => 30000
        ,"sensor.mPF.mode" => 1
        ,"sensor.lIR.pint" => 3600000
        ,"sensor.lIR.dint" => 30000
        ,"sensor.lIR.mode" => 1
        ,"sensor.l.pint" => 3600000
        ,"sensor.l.dint" => 30000
        ,"sensor.l.mode" => 1
        ,"sensor.p.pint" => 1000
        ,"sensor.p.dint" => 30000
        ,"sensor.p.mode" => 2
        ,"sensor.pc.pint" => 3600000
        ,"sensor.pc.dint" => 30000
        ,"sensor.pc.mode" => 1
        ,"sensor.t.pint" => 3600000
        ,"sensor.t.dint" => 30000
        ,"sensor.t.mode" => 1
        ,"sensor.T.pint" => 600000
        ,"sensor.T.dint" => 30000
        ,"sensor.T.mode" => 1
        ,"sensor.mt.pint" => 3600000
        ,"sensor.mt.dint" => 30000
        ,"sensor.mt.mode" => 1
        ,"network.region" => "US"
        ,"network.x.ssid" => "XeraL2"
        ,"network.vpn_on_demand" => false
        ,"server" => "chi.sensity.com"
        ,"token" => "60d504aa"
    },
    #{
        "name" => "ConfigResp"
        ,"nodeid" => [Nodeid]
        ,"kvpairs" => KVPairs
    }.

reply_binary_msg(Req1, State) ->
    Url = "https://ensense-2.sensity.com:10443/device/N01232ea5/fw.bin",
             UpdateReq = {'SoftwareUpdateReq',Url},
             Resp = 'Messages':encode_msg(UpdateReq),
             lager:info("Resp: ~p~n",[Resp]),
             {ok, Req2}=cowboy_req:reply(200, [{"content-type", "application/octet-stream"}], Resp, Req1),
             {ok, Req2, State}.

testlightforce(Nodeid) ->
  Map = #{"ftype" => "Volatile","level" => 100,"mask" => 1,"name" => "LightingForceState","nodeid" => [Nodeid],"pri" => 3,"qualifiers" => "undefined"},
  %Topic = iolist_to_binary(["TopicA/Cisco/Emeryville/", Nodeid ,"/dcc/cmd/LightingForceState"]),
  %Msg = 135,166,110,111,100,101,105,100,145,169,78,48,49,50,51,50,101,97,53,164,110,97,109,101,178,76,105,103,104,116,105,110,103,70,111,114,99,101,83,116,97,116,101,163,112,114,105,3,164,109,97,115,107,1,165,108,101,118,101,108,100,170,113,117,97,108,105,102,105,101,114,115,169,117,110,100,101,102,105,110,101,100,165,102,116,121,112,101,168,86,111,108,97,116,105,108,101,
  Msg= msgpack:pack(Map),
  Ret = command_msg_handler:inspect_adapt_command(Map, Msg),
  {direct, Topic,_NewMsg} = Ret,
  {legacy_mqtt_agent, 'legacy_bridge@127.0.0.1'} ! {dcc, Topic, Map}.


mixed_nodeids() ->
  ["N01232ea5","N021232es5","N013341e5","N023341e5"].

testall(Nodeidss) ->
  Nodeids = case Nodeidss =:= [] of true ->
    ["N01232ea5","N013341e5"];
    _-> Nodeidss
  end,
  testfwnotins3(Nodeids),
  testfwinrep(Nodeids,"v4_6ddde99"),
  testconfigupdate(Nodeids),
  testlightforce(Nodeids),
  testadaptscheds(binary_to_list(lists:first(Nodeids))).

testgroupnodes() ->
  Nodeids = mixed_nodeids(),
  helpers:group_by_type(Nodeids).


 schedules() ->
 [#{"hr" => 8,"id" => 0,"level" => 0,"mask" => 0,"min" => 0,"name" => "LightingScheduledEvent","pri" => 1,"qualifiers" => 0,"sec" => 0},
 #{"hr" => 16,"id" => 1,"level" => 100,"mask" => 0,"min" => 0,"name" => "LightingScheduledEvent","pri" => 1,"qualifiers" => 0,"sec" => 0},
 #{"hr" => 7,"id" => 2,"level" => 0,"mask" => 0,"min" => 30,"name" => "LightingScheduledEvent","pri" => 4,"qualifiers" => 0,"sec" => 0},
 #{"hr" => 16,"id" => 3,"level" => 100,"mask" => 0,"min" => 30,"name" => "LightingScheduledEvent","pri" => 4,"qualifiers" => 0,"sec" => 0},
 #{"hr" => 23,"id" => 4,"level" => 0,"mask" => 0,"min" => 0,"name" => "LightingScheduledEvent","pri" => 4,"qualifiers" => 0,"sec" => 0},
 #{"hr" => 5,"id" => 5,"level" => 50,"mask" => 0,"min" => 0,"name" => "LightingScheduledEvent","pri" => 4,"qualifiers" => 0,"sec" => 0},
 #{"hr" => 0,"id" => 4,"level" => 0,"mask" => 0,"min" => 0,"name" => "LightingScheduledEvent","pri" => 7,"qualifiers" => 0,"sec" => 0,"wday" => 5},
 #{"hr" => 6,"id" => 5,"level" => 50,"mask" => 0,"min" => 0,"name" => "LightingScheduledEvent","pri" => 7,"qualifiers" => 0,"sec" => 0,"wday" => 5},
 #{"hr" => 1,"id" => 4,"level" => 0,"mask" => 0,"min" => 0,"name" => "LightingScheduledEvent","pri" => 7,"qualifiers" => 0,"sec" => 0,"wday" => 6},
 #{"hr" => 6,"id" => 5,"level" => 50,"mask" => 0,"min" => 30,"name" => "LightingScheduledEvent","pri" => 7,"qualifiers" => 0,"sec" => 0,"wday" => 6}].

more_than_8_schedules2() ->
[#{"hr" => 5,"id" => 5,"level" => 100,"mask" => 1,"min" => 0,"pri" => 5,"qualifiers" => 128,"sec" => 0},
#{"hr" => 7,"id" => 4,"level" => 50,"mask" => 1,"min" => 23,"pri" => 5,"qualifiers" => 0,"sec" => 0},
#{"hr" => 7,"id" => 4,"level" => 0,"mask" => 1,"min" => 53,"pri" => 5,"qualifiers" => 0,"sec" => 0},
#{"hr" => 16,"id" => 5,"level" => 50,"mask" => 1,"min" => 43,"pri" => 5,"qualifiers" => 128,"sec" => 0},
#{"hr" => 17,"id" => 5,"level" => 100,"mask" => 1,"min" => 13,"pri" => 5,"qualifiers" => 128,"sec" => 0},
#{"hr" => 23,"id" => 4,"level" => 50,"mask" => 1,"min" => 0,"pri" => 5,"qualifiers" => 0,"sec" => 0},
#{"hr" => 0,"id" => 5,"level" => 100,"mask" => 1,"mday" => 15,"min" => 0,"mon" => 2,"pri" => 5,"qualifiers" => 128,"sec" => 0,"year" => 2016},
#{"hr" => 7,"id" => 4,"level" => 0,"mask" => 1,"mday" => 15,"min" => 23,"mon" => 2,"pri" => 5,"qualifiers" => 0,"sec" => 0,"year" => 2016},
#{"hr" => 17,"id" => 5,"level" => 100,"mask" => 1,"mday" => 31,"min" => 1,"mon" => 3,"pri" => 5,"qualifiers" => 128,"sec" => 0,"year" => 2016},
#{"hr" => 5,"id" => 5,"level" => 100,"mask" => 1,"min" => 0,"pri" => 5,"qualifiers" => 128,"sec" => 0,"wday" => 1},
#{"hr" => 17,"id" => 4,"level" => 0,"mask" => 1,"min" => 10,"pri" => 5,"qualifiers" => 0,"sec" => 0,"wday" => 2},
#{"hr" => 17,"id" => 5,"level" => 100,"mask" => 1,"min" => 17,"pri" => 5,"qualifiers" => 128,"sec" => 0,"wday" => 3},
#{"hr" => 17,"id" => 4,"level" => 0,"mask" => 1,"min" => 5,"pri" => 5,"qualifiers" => 0,"sec" => 0,"wday" => 2},
#{"hr" => 17,"id" => 5,"level" => 100,"mask" => 1,"min" => 20,"pri" => 5,"qualifiers" => 128,"sec" => 0,"wday" => 2},
#{"hr" => 77,"id" => 4,"level" => 0,"mask" => 1,"min" => 53,"pri" => 5,"qualifiers" => 0,"sec" => 0,"wday" => 2},
#{"hr" => 17,"id" => 5,"level" => 100,"mask" => 1,"min" => 11,"pri" => 5,"qualifiers" => 128,"sec" => 0,"wday" => 2},
#{"hr" => 17,"id" => 4,"level" => 0,"mask" => 1,"min" => 11,"pri" => 5,"qualifiers" => 0,"sec" => 0,"wday" => 2}].

more_than_8_schedules() ->
[#{"hr" => 5,"id" => 5,"level" => 100,"mask" => 1,"min" => 0,"pri" => 5,"qualifiers" => 128,"sec" => 0},
#{"hr" => 7,"id" => 4,"level" => 50,"mask" => 1,"min" => 23,"pri" => 5,"qualifiers" => 0,"sec" => 0},
#{"hr" => 7,"id" => 4,"level" => 0,"mask" => 1,"min" => 53,"pri" => 5,"qualifiers" => 0,"sec" => 0},
#{"hr" => 16,"id" => 5,"level" => 50,"mask" => 1,"min" => 43,"pri" => 5,"qualifiers" => 128,"sec" => 0},
#{"hr" => 17,"id" => 5,"level" => 100,"mask" => 1,"min" => 13,"pri" => 5,"qualifiers" => 128,"sec" => 0},
#{"hr" => 23,"id" => 4,"level" => 50,"mask" => 1,"min" => 0,"pri" => 5,"qualifiers" => 0,"sec" => 0},
#{"hr" => 0,"id" => 5,"level" => 100,"mask" => 1,"mday" => 31,"min" => 0,"mon" => 12,"pri" => 5,"qualifiers" => 128,"sec" => 0,"year" => 2016},
#{"hr" => 7,"id" => 4,"level" => 0,"mask" => 1,"mday" => 31,"min" => 23,"mon" => 12,"pri" => 5,"qualifiers" => 0,"sec" => 0,"year" => 2016},
#{"hr" => 17,"id" => 5,"level" => 100,"mask" => 1,"mday" => 31,"min" => 1,"mon" => 12,"pri" => 5,"qualifiers" => 128,"sec" => 0,"year" => 2016},
#{"hr" => 5,"id" => 5,"level" => 100,"mask" => 1,"min" => 0,"pri" => 5,"qualifiers" => 128,"sec" => 0,"wday" => 7},
#{"hr" => 7,"id" => 4,"level" => 0,"mask" => 1,"min" => 51,"pri" => 5,"qualifiers" => 0,"sec" => 0,"wday" => 7},
#{"hr" => 17,"id" => 5,"level" => 100,"mask" => 1,"min" => 17,"pri" => 5,"qualifiers" => 128,"sec" => 0,"wday" => 7},
#{"hr" => 23,"id" => 4,"level" => 0,"mask" => 1,"min" => 0,"pri" => 5,"qualifiers" => 0,"sec" => 0,"wday" => 7},
#{"hr" => 5,"id" => 5,"level" => 100,"mask" => 1,"min" => 0,"pri" => 5,"qualifiers" => 128,"sec" => 0,"wday" => 1},
#{"hr" => 7,"id" => 4,"level" => 0,"mask" => 1,"min" => 53,"pri" => 5,"qualifiers" => 0,"sec" => 0,"wday" => 1},
#{"hr" => 17,"id" => 5,"level" => 100,"mask" => 1,"min" => 11,"pri" => 5,"qualifiers" => 128,"sec" => 0,"wday" => 1},
#{"hr" => 23,"id" => 4,"level" => 0,"mask" => 1,"min" => 0,"pri" => 5,"qualifiers" => 0,"sec" => 0,"wday" => 1}].

nothing_to_cron() ->
[#{"hr" => 1,"id" => 0,"level" => 100,"mask" => 1,"min" => 32,"pri" => 1,"qualifiers" => 4,"sec" => 0},
#{"hr" => 15,"id" => 1,"level" => 0,"mask" => 1,"min" => 9,"pri" => 1,"qualifiers" => 4,"sec" => 0},
#{"hr" => 1,"id" => 2,"level" => 100,"mask" => 1,"min" => 32,"pri" => 7,"qualifiers" => 0,"sec" => 0},
#{"hr" => 15,"id" => 3,"level" => 0,"mask" => 1,"min" => 9,"pri" => 7,"qualifiers" => 0,"sec" => 0}].


testzmqctx(N) ->
  lists:foreach(fun(I)-> ctx_oc(), io:format("[~3..0B] ------~n",[I]), timer:sleep(1500) end,
    lists:seq(1,N)).

ctx_oc()->
    {ok, Context} = erlzmq:context(),
    io:format("*** erlzmq:term/1 return ~p ***~n",[erlzmq:term(Context)]).
schedulemap() -> 
  Map = #{"nodeid" => ["N01232ea5"], "name" => "LightingScheduledEvent", "schedules" => more_than_8_schedules()},
  msgpack:pack(Map).

testzmqlistener(Nodeid) ->
  application:start(sasl),
  application:start(gen_listener_tcp),
  application:start(ezmq),
  Map = #{"nodeid" => [Nodeid], "name" => "LightingScheduledEvent", "schedules" => more_than_8_schedules()},
  Msg = msgpack:pack(Map),
  {ok, Socket} = ezmq:start([{type, req}]),
  ezmq:connect(Socket, tcp, {127,0,0,1}, 5555, []),
  lager:info("Sending more than 8 schedules Msg to ZMQListener ~p ...~n",[Msg]),
  ezmq:send(Socket, [Msg]),
  {ok, R} = ezmq:recv(Socket),
  lager:info("Received p~n", [R]).

startzmqlistener() ->
  application:start(sasl),
  application:start(gen_listener_tcp),
  application:start(ezmq),
  Port = 5555,
  {ok, Socket} = ezmq:start([{type, rep}]),
  ezmq:bind(Socket, tcp, Port, []),
  listenerloop(Socket).

listenerloop(Socket) ->
  Ret = ezmq:recv(Socket),
  lager:info("listener received ~p~n", Ret),
  ezmq:send(Socket, ["Gotit"]),
  loop(Socket).

testerlzmqlistener() ->
   {ok, Ctx} = erlzmq:context(),
   {ok, Req} = erlzmq:socket(Ctx, [req, {active, false}]),
    C = erlzmq:connect(Req, "tcp://127.0.0.1:6541"),
    io:format("connected to 6541 ~p~n",[C]),
    Msg = schedulemap(),
    %%  Send a request.
    io:format("sending map~n"),
    Ok = erlzmq:send(Req, Msg),
     io:format("sent to 6541 ~p~n",[Ok]),
    Rec = erlzmq:recv(Req),
    lager:info("Received ~p~n",[Rec]),
    erlzmq:close(Req),
    erlzmq:term(Ctx).

configfromjs(Nodeid) ->
  #{
  "name"=> "ConfigResp",
  "nodeid"=> [
    Nodeid
  ],
  "kvpairs"=> #{
    "sensor.pc.pint"=> 3600000,
    "sensor.rf.pint"=> 3600000,
    "networkYPasskey"=> "kentspeed",
    "sensor.T.pint"=> 600000,
    "sensor.v.dint"=> 30000,
    "server"=> "10.20.108.92",
    "networkXSSID"=> "SensityDefault",
    "sensor.ai.mode"=> 1,
    "sensor.t.mode"=> 1,
    "sensor.aPF.dint"=> 30000,
    "sensor.p.mode"=> 2,
    "sensor.lIR.pint"=> 3600000,
    "sensor.aw.dint"=> 30000,
    "sensor.mPF.pint"=> 3600000,
    "sensor.pc.dint"=> 30000,
    "sensor.mw.mode"=> 1,
    "sensor.mt.dint"=> 30000,
    "sensor.l.mode"=> 1,
    "sensor.mPF.dint"=> 30000,
    "sensor.mw.dint"=> 30000,
    "sensor.ai.pint"=> 3600000,
    "networkXSecurity"=> "wpa2p",
    "sensor.mi.mode"=> 1,
    "sensor.aPF.pint"=> 3600000,
    "sensor.aPF.mode"=> 1,
    "sensor.mt.mode"=> 1,
    "sensor.mPF.mode"=> 1,
    "sensor.lIR.dint"=> 30000,
    "networkYSecurity"=> "wpa2p",
    "sensor.mi.pint"=> 3600000,
    "sensor.rf.dint"=> 30000,
    "sensor.mi.dint"=> 30000,
    "sensor.lIR.mode"=> 1,
    "sensor.p.dint"=> 30000,
    "sensor.l.pint"=> 3600000,
    "sensor.aw.pint"=> 600000,
    "sensor.ai.dint"=> 30000,
    "sensor.p.pint"=> 1000,
    "sensor.T.mode"=> 1,
    "sensor.t.dint"=> 30000,
    "sensor.v.mode"=> 1,
    "sensor.v.pint"=> 3600000,
    "networkYSSID"=> "XeraL4",
    "sensor.rf.mode"=> 1,
    "networkXPasskey"=> "kentspeed",
    "sensor.T.dint"=> 30000,
    "sensor.t.pint"=> 3600000,
    "sensor.aw.mode"=> 1,
    "sensor.pc.mode"=> 1,
    "sensor.mw.pint"=> 600000,
    "sensor.l.dint"=> 30000,
    "token"=> "7c98c54e",
    "sensor.mt.pint"=> 3600000
  }
}.


shortconfig() ->
#{
  "name"=> "ConfigResp",
  "nodeid"=> [
    "N0gina3"
  ],
 "kvpairs"=> 
  #{"configid"=> "52f74b90-f070-11e5-abe1-c124f900b420",
    "sensor.ai.dint"=> 30000,
    "sensor.p.pint"=> 1000,
    "sensor.T.mode"=> 1,
    "sensor.t.dint"=> 30000,
    "sensor.v.mode"=> 1,
    "sensor.v.pint"=> 3600000,
    "networkYSSID"=> "XeraL4",
    "sensor.rf.mode"=> 1,
    "networkXPasskey"=> "kentspeed",
    "sensor.T.dint"=> 30000,
    "sensor.t.pint"=> 3600000,
    "sensor.aw.mode"=> 1,
    "sensor.pc.mode"=> 1,
    "sensor.mw.pint"=> 600000,
    "sensor.l.dint"=> 30000,
    "configToken"=> "7c98c54e",
    "sensor.mt.pint"=> 3600000
  }
}.

tiny_schedule() ->
  [#{"hr" => 13,"id" => 0,"level" => 30,"mask" => 1,"min" => 17,"pri" => 0,"qualifiers" => 4,"sec" => 0},
  #{"hr" => 2,"id" => 1,"level" => 80,"mask" => 1,"min" => 59,"pri" => 0,"qualifiers" => 4,"sec" => 0},
  #{"hr" => 7,"id" => 2,"level" => 100,"mask" => 1,"min" => 0,"pri" => 3,"qualifiers" => 0,"sec" => 0}].

very_long_schedules() ->
  [
#{"hr" => 2,"id" => 0,"level" => 100,"mask" => 1,"min" => 35,"pri" => 1,"qualifiers" => 4,"sec" => 0},
#{"hr" => 13,"id" => 1,"level" => 0,"mask" => 1,"min" => 46,"pri" => 1,"qualifiers" => 4,"sec" => 0},
#{"hr" => 2,"id" => 2,"level" => 100,"mask" => 1,"min" => 35,"pri" => 7,"qualifiers" => 0,"sec" => 0},
#{"hr" => 14,"id" => 3,"level" => 0,"mask" => 1,"min" => 16,"pri" => 7,"qualifiers" => 0,"sec" => 0},
#{"hr" => 6,"id" => 4,"level" => 50,"mask" => 1,"min" => 0,"pri" => 5,"qualifiers" => 0,"sec" => 0},
#{"hr" => 12,"id" => 5,"level" => 100,"mask" => 1,"min" => 0,"pri" => 5,"qualifiers" => 128,"sec" => 0},
#{"hr" => 13,"id" => 6,"level" => 50,"mask" => 1,"min" => 46,"pri" => 5,"qualifiers" => 0,"sec" => 0},
#{"hr" => 2,"id" => 7,"level" => 50,"mask" => 1,"min" => 5,"pri" => 5,"qualifiers" => 128,"sec" => 0},
#{"hr" => 1,"id" => 0,"level" => 100,"mask" => 1,"mday" => 1,"min" => 1,"mon" => 1,"pri" => 1,"qualifiers" => 4,"sec" => 0,"year" => 2017},
#{"hr" => 15,"id" => 1,"level" => 0,"mask" => 1,"mday" => 31,"min" => 22,"mon" => 12,"pri" => 1,"qualifiers" => 4,"sec" => 0,"year" => 2016},
#{"hr" => 1,"id" => 2,"level" => 100,"mask" => 1,"mday" => 1,"min" => 1,"mon" => 1,"pri" => 7,"qualifiers" => 0,"sec" => 0,"year" => 2017},
#{"hr" => 15,"id" => 3,"level" => 0,"mask" => 1,"mday" => 31,"min" => 22,"mon" => 12,"pri" => 7,"qualifiers" => 0,"sec" => 0,"year" => 2016},
#{"hr" => 2,"id" => 0,"level" => 100,"mask" => 1,"min" => 40,"pri" => 1,"qualifiers" => 4,"sec" => 0,"wday" => 1},
#{"hr" => 13,"id" => 1,"level" => 0,"mask" => 1,"min" => 39,"pri" => 1,"qualifiers" => 4,"sec" => 0,"wday" => 7},
#{"hr" => 2,"id" => 2,"level" => 100,"mask" => 1,"min" => 40,"pri" => 7,"qualifiers" => 0,"sec" => 0,"wday" => 1},
#{"hr" => 14,"id" => 3,"level" => 0,"mask" => 1,"min" => 9,"pri" => 7,"qualifiers" => 0,"sec" => 0,"wday" => 7},
#{"hr" => 2,"id" => 0,"level" => 100,"mask" => 1,"min" => 34,"pri" => 1,"qualifiers" => 4,"sec" => 0,"wday" => 2},
#{"hr" => 13,"id" => 1,"level" => 0,"mask" => 1,"min" => 48,"pri" => 1,"qualifiers" => 4,"sec" => 0,"wday" => 1},
#{"hr" => 2,"id" => 2,"level" => 100,"mask" => 1,"min" => 34,"pri" => 7,"qualifiers" => 0,"sec" => 0,"wday" => 2},
#{"hr" => 14,"id" => 3,"level" => 0,"mask" => 1,"min" => 18,"pri" => 7,"qualifiers" => 0,"sec" => 0,"wday" => 1}].
