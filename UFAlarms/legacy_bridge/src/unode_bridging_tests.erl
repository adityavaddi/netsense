-module(unode_bridging_tests).
-author("Gina Hagg <ghagg@ysensity.com").
-include_lib("eunit/include/eunit.hrl").


handle_decoding_msgs_test() ->
    Msgpacked = <<162,1,11,10,7,8,3,16,1,26,1,0,16,2>>,
    MsgToPack = {'LightingForceState',{'LightingCtrl',3, 1,<<0>>,undefined},'Volatile'},
    LoginReqBin = <<10,19,10,5,78,83,49,95,50,16,1,26,8,117,110,111,100,101,45,118,51>>, 
    LoginReqMsg = {'LoginReq',"NS1_2",1,"unode-v3",undefined,undefined,undefined,undefined,undefined,undefined,undefined},                                 
    SensorSampleBin = <<82,19,10,2,108,116,16,232,165,241,209,210,245,199,2,24,58,34,2,112,102>>,
    SensorSampleMsg = {'SensorSample',"lt",1442203481953000,58,"pf"},
    LoginRespBin = <<18,2,8,1>>,
    LoginRespMsg = {'LoginResp',true,undefined},
    _TupledSensorSample = [{msg_type,'SensorSample'},{sensor,<<"t">>},{time,1442203514943000},{value,21179},{units,<<"fahrenheigt">>}],

     %MsgpackedMsg = <<218,0,100,123,34,109,115,103,95,116,121,112,101,34,58,34,83,101,110,115,111,
     %               114,83,97,109,112,108,101,34,44,34,115,101,110,115,111,114,34,58,34,116,34,
     %               44,34,116,105,109,101,34,58,49,52,52,50,50,48,51,53,49,52,57,52,51,48,48,48,
     %               44,34,118,97,108,117,101,34,58,50,49,49,55,57,44,34,117,110,105,116,115,34,
     %               58,34,102,97,104,114,101,110,104,101,105,103,116,34,125>>,


    {{LoginReqMsgCode,LoginBin},_} = protobuffs:decode(LoginReqBin ,bytes),                 
    ?assertEqual(1,LoginReqMsgCode),

    {_, LoginReqM} = unode_proto_handler:decode_env_by_type(1, LoginBin),
    ?assertEqual(LoginReqMsg,LoginReqM),

    {{SensorMsgCode,SensorBin},_} = protobuffs:decode(SensorSampleBin ,bytes),                 
    ?assertEqual(10,SensorMsgCode),

    {_, SensorM} = unode_proto_handler:decode_env_by_type(10, SensorBin),
    ?assertEqual(SensorSampleMsg,SensorM),

    {{LoginRespMsgCode,LoginRespM1},_} = protobuffs:decode(LoginRespBin ,bytes),                 
    ?assertEqual(2,LoginRespMsgCode),

    {_, LoginRespM} = unode_proto_handler:decode_env_by_type(2, LoginRespM1),
    ?assertEqual(LoginRespMsg,LoginRespM),

    ?assertEqual(Msgpacked,unode_proto_handler:encode_msgs(MsgToPack)).



        