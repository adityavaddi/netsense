-module(handshake_handler).
-author("Gina Hagg <ghagg@ysensity.com").
-compile(export_all).
-compile([{parse_transform, lager_transform}]).

 %{'LoginReq',"N01232ea5",0,"unode-v4","5549c61","SensityDefault","x",60,"a8733d94","10.20.108.92",1447470224149108}
handle_login(LoginReqMsg, Nodeid) ->
	lager:info("LoginReqMsg: ~p~n",[LoginReqMsg]),
	Timestamp = os:system_time(micro_seconds),
	LoginResp = {'LoginResp', true, Timestamp},
	unode_proto_handler:encode_env_by_type('LoginResp', LoginResp, Nodeid).

send_firmware_update_if_needed(LoginReqMsg) ->
	{ok,DownloadHost} = application:get_env(legacy_bridge,host),
	{_, Nodeid, _, _Unodetype, _, _, _, _, _Firmware, _, _Time} = LoginReqMsg,
	%% look at firmware id here and decide if it is necessary to request node to update. Let's say yes for now.
	%% send firmware update request
	%Url = edoc_lib:escape_uri("https://" ++ Host ++ ":10443/device/" ++ Nodeid ++ "/fw.bin"),
	Url = "https://" ++ DownloadHost ++ ":10443/device/" ++ Nodeid ++ "/fw.bin",
	UpdateReq = {'SoftwareUpdateReq',Url},
	unode_proto_handler:encode_env_by_type('SoftwareUpdateReq',UpdateReq,Nodeid).


