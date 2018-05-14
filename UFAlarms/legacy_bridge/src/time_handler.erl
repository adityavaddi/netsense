-module(time_handler).
-compile(export_all).

handle_time_request(TimeReqMsg) ->
	lager:info("TimeReqMsg: ~p~n",[TimeReqMsg]),
	TimeReqMsg.
	