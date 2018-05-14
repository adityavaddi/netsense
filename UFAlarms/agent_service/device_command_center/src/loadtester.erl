-module(loadtester).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).

spawn_many(0, _Interval)-> spawned_all_processes;

spawn_many(N, Interval)-> 
    %Mp = <<139,172,97,115,115,111,99,99,104,97,110,110,101,108,48,168,99,102,103,116,111,107,101,110,168,102,49,98,53,100,52,56,49,170,99,108,105,101,110,116,116,121,112,101,168,117,110,111,100,101,45,118,52,162,105,112,172,49,48,46,50,48,46,49,48,57,46,51,52,164,110,97,109,101,168,76,111,103,105,110,82,101,113,166,110,111,100,101,105,100,169,78,48,49,50,51,50,101,97,53,171,112,114,111,102,105,108,101,110,97,109,101,161,120,168,112,114,111,116,111,118,115,110,0,164,115,115,105,100,166,88,101,114,97,76,50,167,115,119,118,115,110,105,100,167,50,98,57,102,48,50,99,162,116,115,207,0,5,34,4,185,1,23,28>>,
    Pid = spawn(?MODULE,withinterval,[N, Interval]),
    timer:sleep(3000),
    lager:info("spawned PID: ~p~n",[Pid]),
    spawn_many(N - 1, Interval).

%when (N band 1 == 1) -> odd 
withinterval(N, Interval)->
    Nodeid = list_to_binary(lists:concat(["N0eac" , N])),
	lager:info("we are spawned with pid: ~p, for node: ~p~n", [self(), Nodeid]),
	Timestamp = os:system_time(),

	%first send connection status for spawned module.
	Msg1 = #{<<"name">> => <<"ConnectionStatus">> , <<"nodeId">> => Nodeid,<<"status">> => <<"connected">>},
	Conmsg = msgpack:pack( Msg1,[{format,map}]),
	zmqpushclient:zmqsend(Conmsg),
	timer:sleep(1000),

	Msg2 = #{<<"name">> =><<"LoginReq">>,<<"nodeid">> => Nodeid,<<"protovsn">> => 0, <<"clienttype">> => <<"unode-v4">>,
	<<"swvsnid">> =><<"2b9f02c">>, <<"ssid">> =><<"XeraL2">>, <<"profilename">> => <<"x">>, <<"assocchannel">> => 48, 
  	<<"cfgtoken">> => <<"f1b5d481">>,<<"ip">> => <<"10.20.109.34">>, <<"ts">> => Timestamp},
	Loginmsg = msgpack:pack( Msg2,[{format,map}]),
	zmqpushclient:zmqsend(Loginmsg),
	timer:sleep(1000),

	%%now start sending samples like the node does in intervals.
	random:seed(os:system_time()),
	Value = random:uniform(100),
	Msg3 =  #{<<"name">> => <<"SensorSample">>,<<"nodeId">> => Nodeid,
  	<<"sensor">> => <<"lt">>,<<"time">> => Timestamp,<<"units">> => <<>>,<<"value">> => Value},
  	Sensorsample = msgpack:pack( Msg3,[{format,map}]),
    {ok, _TRef} = timer:apply_interval(timer:seconds(Interval),zmqpushclient, zmqsend, [Sensorsample] ),
    timer:sleep(2000),
    receive
          <<"stop">> -> exit(normal)
    end.


