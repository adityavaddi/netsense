-module(unode_proto_handler).

-author("Gina Hagg <ghagg@sensity.com").

-compile(export_all).

-compile([{parse_transform, lager_transform}]).

-define(LOGINPROTO, {'LoginResp', true, os:system_time(micro_seconds)}).
-define(TIMEPROTO, {'TimeResp', os:system_time(micro_seconds)}).
-define(SENSOR_POS, 11).
-define(LOGIN_POS, 2).
-define(LOGINRESP_POS, 3).
-define(GPS_SAMPLE_POS, 17).
-define(GPS_ACTION_RSP_POS, 19).
-define(GPS_ACTION_REQ_POS, 18).

%LoginReq = 1;LoginResp = 2;UnusedConfigReq = 3;ConfigResp = 4;ConfigRespDone = 5;SoftwareUpdateReq = 6;DeviceAlarm = 7;
%DeviceActionReq = 8;X509UpdateReq = 9;SensorSample = 10;VideoUploadReq = 11;VideoUploadResp = 12;TimeReq = 13;TimeResp = 14;
%SensorSampleReq = 15;LightingForceState = 20;LightingSetAuto = 21;LightingScheduledEvent = 22;LightingAstronomicalEvent = 23;
%LightingClearScheduleEvent = 24;ErrorResp = 31;
%% open the Msg binary, look at the first byte, that tells us the Envelope type. Then decode accordingly.. i.e 10 is SensorSample, 7 DeviceAlarmCode etc.

-define(EmptyEnvelope,
	{'Envelope',undefined,
            undefined,undefined,undefined,undefined,undefined,
            undefined,undefined,undefined,undefined,undefined,
            undefined,undefined,undefined,undefined,undefined,
            undefined,undefined,undefined,undefined,undefined,
            undefined,undefined}).

message_types() ->
    ['LoginReq', 'LoginResp', 'UnusedConfigReq',
     'ConfigResp', 'ConfigRespDone', 'SoftwareUpdateReq',
     'DeviceAlarm', 'DeviceActionReq', 'X509UpdateReq',
     'SensorSample', 'VideoUploadReq', 'VideoUploadResp',
     'TimeReq', 'TimeResp', 'SensorSampleReq', 'GpsSample',
     'GpsActionReq', 'GpsActionRsp', undefined, 'LightingForceState',
     'LightingSetAuto', 'LightingScheduledEvent',
     'LightingAstronomicalEvent', 'LightingClearSchedule',undefined,
     undefined,undefined,
     'AuxRelayActionReq', undefined,undefined,
     'ErrorResp'].

message_enums() ->
    [{'LoginReq', 1}, {'LoginResp', 2},
     {'UnusedConfigReq', 3}, {'ConfigResp', 4},
     {'ConfigRespDone', 5}, {'SoftwareUpdateReq', 6},
     {'DeviceAlarm', 7}, {'DeviceActionReq', 8},
     {'X509UpdateReq', 9}, {'SensorSample', 10},
     {'VideoUploadReq', 11}, {'VideoUploadResp', 12},
     {'TimeReq', 13}, {'TimeResp', 14},
     {'SensorSampleReq', 15}, {'GpsSample', 16},
     {'GpsActionReq', 17},{'GpsActionRsp', 18},
     {'LightingForceState', 19},
     {'LightingSetAuto', 20}, {'LightingScheduledEvent', 21},
     {'LightingAstronomicalEvent', 22},
     {'LightingClearSchedule', 23}, {'ErrorResp', 31}, {'AuxRelayActionReq', 28}].

%%This below only true for Media Types. I could have unpacked using Envelope.
%	 M = 'Messages':de_msg(<<82,22,10,1,118,16,152,229,167,211,210,245,199,2,24,225,217,6,34,4,118,111,108,116>> , 'Envelope').
%    M.
%	{'Envelope',undefined,undefined,undefined,undefined,
%            undefined,undefined,undefined,undefined,undefined,
%            {'SensorSample',"v",1442203484943000,109793,"volt"},
%            undefined,undefined,undefined,undefined,undefined,undefined,
%            undefined,undefined,undefined,undefined,undefined}.

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
decode_env_by_type(N, NestedMsg) ->
    %lager:info("Nestedmsg: ~p~n",[NestedMsg]),
    EnvType = lists:nth(N, message_types()),
    DecodedMsg = 'Messages':decode_msg(NestedMsg, EnvType),
    %lager:debug("decode_env_by_type: ~p ~p ~n",[DecodedMsg, EnvType]),
    {EnvType, DecodedMsg}.

encode_env_by_type('DeviceActionReq', Resp, _NodeId) ->
    %lager:debug("in message EnvType: ~p, Resp: ~p~n",['DeviceActionReq', Resp]),
    Pos = proplists:get_value('DeviceActionReq',
			      message_enums()),
    NewEvlp = setelement(Pos + 1, ?EmptyEnvelope, Resp),
    'Messages':encode_msg(NewEvlp);
encode_env_by_type('LightingForceState', Resp, _NodeId) ->
    Pos = proplists:get_value('LightingForceState',
                  message_enums()),
    NewEvlp = setelement(Pos + 1, ?EmptyEnvelope, Resp),
    lager:debug("in message EnvType: ~p, Resp: ~p~n",['LightingForceState', NewEvlp]),
    'Messages':encode_msg(NewEvlp);
encode_env_by_type('SensorSample', Resp, _NodeId) ->
    NewEvlp = setelement(?SENSOR_POS, ?EmptyEnvelope, Resp),
    'Messages':encode_msg(NewEvlp);
encode_env_by_type('GpsActionReq', Resp, _NodeId) ->
    NewEvlp = setelement(?GPS_ACTION_REQ_POS, ?EmptyEnvelope, Resp),
    'Messages':encode_msg(NewEvlp);
encode_env_by_type(EnvType, Resp, _NodeId) ->
    %lager:debug("in message EnvType: ~p, Resp: ~p~n",[EnvType, Resp]),
    Pos = proplists:get_value(EnvType, message_enums()),
    NewEvlp = setelement(Pos + 1, ?EmptyEnvelope, Resp),
    'Messages':encode_msg(NewEvlp).

encode_login_resp() ->
    NewEvlp = setelement(?LOGINRESP_POS, ?EmptyEnvelope,
			 ?LOGINPROTO),
    'Messages':encode_msg(NewEvlp).

encode_time_resp() ->
    NewEvlp = setelement(15, ?EmptyEnvelope, ?TIMEPROTO),
    'Messages':encode_msg(NewEvlp).

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
encode_msgs([MsgList]) when is_tuple(MsgList) ->
    Msg = encode_env_by_type(element(1, MsgList), MsgList, 1),
    [Msg];
encode_msgs([MsgList])
    when is_list(MsgList), length(MsgList) > 0 ->
    [encode_msgs_1(V1) || V1 <- MsgList];
encode_msgs({}) -> [];
encode_msgs(MsgList) ->
    case is_tuple(MsgList) of
      true ->
	  encode_env_by_type(element(1, MsgList), MsgList, 1);
      _ -> [encode_msgs_2(V2) || V2 <- MsgList]
    end.

encode_msgs_1(Msg) ->
    encode_env_by_type(element(1, Msg), Msg, 1).

encode_msgs_2(Msg) ->
    encode_env_by_type(element(1, Msg), Msg, 1).

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
%%Map will be like this
%%[{"name","LoginResp"},{"ok",true},{"time",1445837449229657}]
%%It will come out like this
%%{'LoginResp',true,Timestamp}
translatetoproto(_Nodeid, Respmap1) ->
    Respmap = maps:to_list(Respmap1),
    Msgname = proplists:get_value(<<"name">>, Respmap),
    Translated = dont_send_if_loginresp(Msgname, Respmap),
    lager:debug("sending translated :~p~n", [Translated]),
    Translated.

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
dont_send_if_loginresp(<<"LoginResp">>, _Respmap) -> {};
dont_send_if_loginresp(Msgname, Respmap) ->
    Rlist2 = [element(2, X) || X <- Respmap],
    Rlist3 = helpers:setnth(1, Rlist2, binary_to_atom(Msgname, unicode)),
    list_to_tuple(Rlist3).
