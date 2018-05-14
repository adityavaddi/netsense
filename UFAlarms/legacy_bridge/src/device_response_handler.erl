-module(device_response_handler).

-author("Gina Hagg ghagg@sensity.com").

-compile(export_all).

-compile([{parse_transform, lager_transform}]).

-define(PREFIX, <<"TopicA/Cisco/Emeryville/">>).

-define(DCC, 'device_command_center@127.0.0.1').

-define(SensorSampleEnvlp, 10).

register_and_forward_msg(EnvlpType, Rest, Nodeid,
			 Ota) ->
    {MsgType, DecodedMsg} =
	unode_proto_handler:decode_env_by_type(EnvlpType, Rest),
    lager:debug("LoginReq ~p : ~p from node ~p, forwarding "
	       "it to directly to datadealer. ~n",
	       [MsgType, DecodedMsg, Nodeid]),
    register_notify_firmware_event(MsgType, DecodedMsg,
				   Nodeid, Ota),
    %send_to_dd(DecodedMsg, Nodeid),
    send_to_dcc_login(DecodedMsg, Nodeid),
    DecodedMsg.

forward_msg(?SensorSampleEnvlp, Rest, Nodeid) ->
    {MsgType, DecodedMsg} =
	unode_proto_handler:decode_env_by_type(?SensorSampleEnvlp,
					       Rest),
    lager:debug("Message received ~p : ~p from node ~p "
	       "forwarding to directly to datadealer. "
	       "~n",
	       [MsgType, DecodedMsg, Nodeid]),
    %send_to_dcc_sensor(DecodedMsg, Nodeid);
    send_to_dd(DecodedMsg, Nodeid);
forward_msg(EnvlpType, Rest, Nodeid) ->
    {MsgType, DecodedMsg} =
	unode_proto_handler:decode_env_by_type(EnvlpType, Rest),
    lager:debug("Message received ~p : ~p from node ~p "
	       "forwarding to directly to datadealer. "
	       "~n",
	       [MsgType, DecodedMsg, Nodeid]),
    send_to_dd(DecodedMsg, Nodeid).

do_mqtt({DecodedMsg, NodeId, Prefix, Con}) ->
    {Topic, PackedMsg} =
	legacy_mqtt_handler:prepare_4_mqtt(DecodedMsg, NodeId,
					   Prefix),
    lager:debug("sending msg to mqtt Topic: , msg: ~p, "
		"PackedMsg: ~p~n",
		[Topic, DecodedMsg]),
    emqttc:publish(Con, Topic, PackedMsg, 1).

register_notify_firmware_event('LoginReq', DecodedMsg,
			       Nodeid, Ota) ->
    case Ota of
      true ->
	  gproc:send({p, l, new_login},
		     {new_login, {Nodeid, DecodedMsg}});
      _ -> ok
    end;
register_notify_firmware_event(_, _, _, _) -> ok.
send_to_dd(DecodedMsg, Nodeid) ->
    Packed =
	legacy_msgpack_handler:msgpack_payload(DecodedMsg,
					       Nodeid),
    helpers:send_to_dd(Packed).

%%I am not sure if we need a response for LoginResp. I think this is what is created
%%when we answer a login req. I don't think it comes from device. Clarify if we login to a device??

%%place holder, encode the same message and send back until we know what we get.
%produce_response_for_message(MsgType, DecodedMsg, NodeId) ->
%  unode_proto_handler:encode_env_by_type(MsgType,DecodedMsg, NodeId).

%  required string x509UpdateURL = 1; // [(nanopb).max_size = 123];
%  required X509Update x509Update = 2;
%  optional uint32 httpsPort = 3;
%{'X509UpdateReq',"localhost",'X509ClientOnly',10443}
sendX509UpdateReq(NodeId) ->
    EnvType = 'X509UpdateReq',
    Resp = {'X509UpdateReq',
	    "https://localhost:10443/download/certificate",
	    'X509ClientOnly', 10443},
    EncodedEvlp =
	unode_proto_handler:encode_env_by_type(EnvType, Resp,
					       NodeId),
    lager:debug("EncodedEvlp: ~p~n", [EncodedEvlp]),
    [EncodedEvlp].

%<<10,21,10,14,115,101,110,115,111,114,46,112,99,46,112,105,110,116,24,128,221,219,1>>
sendTestConfigResponse(NodeId) ->
    [sendTestLightingForceCommand1(NodeId),
     unode_proto_handler:encode_env_by_type('ConfigResp',
					    {'ConfigResp',
					     {'KVPair', "sensor.pc.pint",
					      undefined, 3600000, undefined}},
					    NodeId),
     unode_proto_handler:encode_env_by_type('ConfigResp',
					    {'ConfigResp',
					     {'KVPair', "sensor.rf.pint",
					      undefined, 3600000, undefined}},
					    NodeId),
     unode_proto_handler:encode_env_by_type('ConfigResp',
					    {'ConfigResp',
					     {'KVPair', "sensor.rf.dint",
					      undefined, 30000, undefined}},
					    NodeId),
     unode_proto_handler:encode_env_by_type('ConfigResp',
					    {'ConfigResp',
					     {'KVPair', "sensor.v.mode",
					      undefined, 1, undefined}},
					    NodeId),
     unode_proto_handler:encode_env_by_type('ConfigResp',
					    {'ConfigResp',
					     {'KVPair', "sensor.aw.pint",
					      undefined, 600000, undefined}},
					    NodeId),
     unode_proto_handler:encode_env_by_type('ConfigResp',
					    {'ConfigResp',
					     {'KVPair', "sensor.ai.mode",
					      undefined, 1, undefined}},
					    NodeId),
     unode_proto_handler:encode_env_by_type('ConfigResp',
					    {'ConfigResp',
					     {'KVPair', "sensor.ai.dint",
					      undefined, 30000, undefined}},
					    NodeId),
     unode_proto_handler:encode_env_by_type('ConfigResp',
					    {'ConfigResp',
					     {'KVPair', "sensor.ai.pint",
					      undefined, 3600000, undefined}},
					    NodeId),
     unode_proto_handler:encode_env_by_type('ConfigResp',
					    {'ConfigResp',
					     {'KVPair', "sensor.v.dint",
					      undefined, 30000, undefined}},
					    NodeId),
     unode_proto_handler:encode_env_by_type('ConfigResp',
					    {'ConfigResp',
					     {'KVPair', "sensor.v.pint",
					      undefined, 3600000, undefined}},
					    NodeId),
     unode_proto_handler:encode_env_by_type('ConfigResp',
					    {'ConfigResp',
					     {'KVPair', "sensor.rf.mode",
					      undefined, 1, undefined}},
					    NodeId),
     sendTestLightingForceCommand(NodeId)].

%%ScheduleOverride(mode = UnodeProto1.ScheduleMode(priority = 3, mask = 0x00000001, driver = 0, qualifiers = 0x00000000), life = UnodeProto1.ScheduleLife.Volatile)
sendTestLightingForceCommand(NodeId) ->
    [unode_proto_handler:encode_env_by_type('LightingForceState',
					    {'LightingForceState',
					     {'LightingCtrl', 3, 1, <<0:8>>,
					      undefined},
					     undefined},
					    NodeId)].

sendTestLightingForceCommand1(NodeId) ->
    [unode_proto_handler:encode_env_by_type('LightingForceState',
					    {'LightingForceState',
					     {'LightingCtrl', 3, 1, <<1:8>>,
					      undefined},
					     undefined},
					    NodeId)].

sendTestLightingForceCommand(NodeId, Level) ->
    [unode_proto_handler:encode_env_by_type('LightingForceState',
					    {'LightingForceState',
					     {'LightingCtrl', 3, 1, <<Level>>,
					      undefined},
					     undefined},
					    NodeId)].

send_to_dcc(DecodedMsg, Nodeid) ->
  Packed = legacy_msgpack_handler:msgpack_payload(DecodedMsg, Nodeid),
  helpers:send_to_dd(Packed).

send_to_dcc_login(DecodedMsg, Nodeid) ->
  Packed = legacy_msgpack_handler:msgpack_payload(DecodedMsg, Nodeid),
  helpers:send_to_dd_login(Packed).
