%curl -v http://localhost:8080/ws -H "Upgrade: WebSocket" -H "Connection: Upgrade" -H "Sec-WebSocket-Version: 13" -H "Sec-WebSocket-Key: 123" -H "Sec-WebSocket-Protocol: sip" -H "Host: localhost" -H "Origin: http//localhost"

-module(test_ws).
-compile(export_all).

start() ->
    application:ensure_all_started(gun),
	{ok, ConnPid} = gun:open("127.0.0.1", 8080),
	{ok, http} = gun:await_up(ConnPid),
	gun:ws_upgrade(ConnPid, "/websocket"),
	receive
		{gun_ws_upgrade, ConnPid, ok} ->
			data_stream(ConnPid);
		{gun_ws_upgrade, ConnPid, error, IsFin, Status, Headers} ->
		    io:format("ooops error ~p~n", [error]),
	 		exit({ws_upgrade_failed, Status, Headers});
	 	Msg ->io:format("Unexpected message when upgrading protocol ~p", [Msg])
	 after 10000 ->
	 	exit(timeout)
	 end.

	data_stream(ConnPid) ->
		gun:ws_send(ConnPid, {binary, <<"Hello websocket">>}),
		receive
			{gun_ws, ConnPid, Frame} ->
			handle_frame(ConnPid, Frame)
		end,
		gun:ws_send(ConnPid, close).

	handle_frame(ConnPid, Frame) ->
		io:format("Frame ~p~n", [Frame]).