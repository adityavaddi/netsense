-module(command_msg_handler_tests).
-author("Gina Hagg <ghagg@ysensity.com").
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).

group_nodes_test() ->
 Nodeids = tester:mixed_nodeids(),
 Return = helpers:group_by_type(Nodeids),	
 Expected = {[<<"N013341e5">>,<<"N01232ea5">>],[<<"N023341e5">>,<<"N021232es5">>]},
 ?assertEqual(Expected, Return).

send_config_update_test() ->
	Nodeid = <<"N01232ea5">>,
	Map = tester:cfgdata(Nodeid),
  	Msg = <<135,166,98,118,97,108,117,101,169,117,110,100,101,102,105,110,101,100,167,103,114,111,117,112,105,100,218,0,36,101,53,48,52,54,57,52,48,45,97,50,98,53,45,49,49,101,53,45,98,55,50,101,45,102,49,102,54,54,52,98,54,48,101,52,100,163,107,101,121,174,115,101,110,115,111,114,46,112,99,46,112,105,110,116,166,108,118,97,108,117,101,206,0,54,238,128,164,110,97,109,101,170,67,111,110,102,105,103,82,101,115,112,166,110,111,100,101,105,100,145,153,78,48,49,51,51,52,49,101,53,166,115,118,97,108,117,101,169,117,110,100,101,102,105,110,101,100>>,
  	ToSend = command_msg_handler:inspect_adapt_command(Map, Msg),
  	?assertEqual(is_tuple(ToSend), true).

send_schedule_legacy_test() ->
  	Map = #{<<"nodeid">> => legacy_nodeids(), <<"name">> => <<"LightingScheduledEvent">>, <<"schedules">> => tester:more_than_8_schedules()},
  	ToSend = command_msg_handler:inspect_adapt_command(Map,tester:schedrawdata()),
  	?assertEqual(is_tuple(ToSend), true).

send_light_force_legacy_test() ->
  	Map = #{<<"ftype">> => <<"Volatile">>,<<"level">> => 100,<<"mask">> => 1,<<"name">> => <<"LightingForceState">>,<<"nodeid">> => [<<"N01323ea5">>],<<"pri">> => 3,<<"qualifiers">> => <<"undefined">>},
  	Msg = <<135,166,110,111,100,101,105,100,145,169,78,48,49,50,51,50,101,97,53,164,110,97,109,101,178,76,105,103,104,116,105,110,103,70,111,114,99,101,83,116,97,116,101,163,112,114,105,3,164,109,97,115,107,1,165,108,101,118,101,108,100,170,113,117,97,108,105,102,105,101,114,115,169,117,110,100,101,102,105,110,101,100,165,102,116,121,112,101,168,86,111,108,97,116,105,108,101>>,
  	Return = command_msg_handler:inspect_adapt_command(Map, Msg), 
    Expected = <<"TopicA/Cisco/Emeryville/dcc/cmd/lighting">>, 	
  	?assertEqual(Expected, element(1,Return)).

send_light_force_mixed_test() ->
  	Nodeids = tester:mixed_nodeids(),
  	Map = #{<<"ftype">> => <<"Volatile">>,<<"level">> => 100,<<"mask">> => 1,<<"name">> => <<"LightingForceState">>,<<"nodeid">> => Nodeids,<<"pri">> => 3,<<"qualifiers">> => <<"undefined">>},
  	Msg = <<135,166,110,111,100,101,105,100,145,169,78,48,49,50,51,50,101,97,53,164,110,97,109,101,178,76,105,103,104,116,105,110,103,70,111,114,99,101,83,116,97,116,101,163,112,114,105,3,164,109,97,115,107,1,165,108,101,118,101,108,100,170,113,117,97,108,105,102,105,101,114,115,169,117,110,100,101,102,105,110,101,100,165,102,116,121,112,101,168,86,111,108,97,116,105,108,101>>,
  	command_msg_handler:inspect_adapt_command(Map, Msg).

assignfw_legacy_test() ->
  	Nodeids = legacy_nodeids(),
  	Map = #{<<"nodeid">> => Nodeids, <<"name">> => <<"AssignFirmware">>, <<"firmwareid">> => <<"6ddde96">>},
    Expected =  <<"TopicA/Cisco/Emeryville/dcc/cmd/firmware">>,
  	Return = command_msg_handler:inspect_adapt_command(Map,<<>>),
    ?assertEqual(Expected, element(1,Return)).

tosend_lightdata() ->
  	[{<<"TopicA/Cisco/Emeryville/dcc/cmd/LightingForceState">>,
  	<<135,166,110,111,100,101,105,100,145,169,78,48,49,50,51,50,101,97,53,164,110,97,109,101,178,76,105,103,104,116,105,110,103,70,111,114,99,101,83,116,97,116,101,163,112,114,105,3,164,109,97,115,107,1,165,108,101,118,101,108,100,170,113,117,97,108,105,102,105,101,114,115,169,117,110,100,101,102,105,110,101,100,165,102,116,121,112,101,168,86,111,108,97,116,105,108,101>>}].

tosend_lighttopic() ->
   		<<"TopicA/Cisco/Emeryville/dcc/cmd/LightingForceState">>.

 tosend_fwtopic() ->
   		<<"TopicA/Cisco/Emeryville/dcc/cmd/SoftwareUpdateReq">>.
 
legacy_nodeids() ->
 	[<<"N01323ea5">>,<<"N013341e5">>].