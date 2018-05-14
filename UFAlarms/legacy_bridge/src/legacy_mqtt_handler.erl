-module(legacy_mqtt_handler).
-author("Gina Hagg <ghagg@sensity.com").
-compile(export_all).
-define(Prefix, "TopicA/Cisco/Emeryville/").
-define(ReqExt, "/node/req/").
-define(EvtExt, "/node/evt/").
-define(CmdExt, "/dcc/cmd/").
-define(AlarmExt, "/node/alarm/").

%{topic-version}/{customer}/{site}/{nodeid}/node/req/

prepare_4_mqtt(DecodedMsgEnvlp, NodeId, Prefix) ->
    Topic = create_topic_by_message(DecodedMsgEnvlp, NodeId, Prefix),
    PackedMsg = legacy_msgpack_handler:msgpack_payload(DecodedMsgEnvlp, NodeId),
    {Topic, PackedMsg}.

create_topic_by_message(MsgEnvlp, NodeId, Prefix) ->
    Msgname = element(1,MsgEnvlp),
    topic_by_type(Msgname, NodeId, Prefix).

topic_by_type('LoginReq' , NodeId, Prefix)  ->
    list_to_binary([Prefix, NodeId, ?ReqExt , "LoginReq"]);

topic_by_type('SensorSample' , NodeId, Prefix)  ->
    list_to_binary([Prefix, NodeId, ?EvtExt , "SensorSample"]);

topic_by_type('DeviceAlarm', NodeId, Prefix) ->
    list_to_binary([Prefix, NodeId, ?AlarmExt , "DeviceAlarm"]);

topic_by_type(Msgname, NodeId, Prefix)  ->
    list_to_binary([Prefix, NodeId, ?ReqExt , atom_to_list(Msgname)]).

%%returns below.
%%[{ok,{[{"name","LoginResp"},{"ok",true},{"time",1445839078815779}]}}]
unpack_msgs(MsgPacked) ->
    msgpack:unpack(MsgPacked).