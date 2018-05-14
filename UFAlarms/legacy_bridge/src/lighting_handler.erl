-module(lighting_handler).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).
-define(Maxschedules, 24).
-define(ClearSchedule,[<<194,1,0>>]).
-define(RunSchedule, [<<170,1,0>>]).

prepare_schedules(Schedlist) ->
	Schedules11 =  maps:get("schedules", Schedlist),
	Schedules1 = lists:map(fun(X1) ->
  		X2 = maps:to_list(X1),
  		X4 = lists:map(fun(X) -> {Y,Z} = X, case is_binary(Y) of true -> {binary_to_list(Y),Z}; _-> {Y,Z}  end end, X2),
  		maps:from_list(X4)
	end, Schedules11),
	GroupedScheds1 = library:flatten3(schedules:group_weekdays_for_bitmask(Schedules1)),
	GroupedScheds = lists:sort(fun(X,Y) -> element(1,X) < element(1,Y) end, GroupedScheds1),
	lager:debug("Reducedweeklies: ~p", [GroupedScheds]),
	SchedLen = length(GroupedScheds),
	Nodeids = maps:get("nodeid", Schedlist),
	AllNows = case SchedLen =< ?Maxschedules of
		true -> 
			schedules:combine_clear_run_24(GroupedScheds);
		_->
			schedules:combine_clear_run_more_than_24(GroupedScheds,Nodeids)
	end,	
	AllNows.


send_light_schedule({Nodeids, Binary}) ->
	lists:map(fun(Nodeid) ->
		send_light_schedule(Nodeid, Binary)
	end, Nodeids).

send_light_schedule(Nodeid, Binary) ->
    lager:debug("send_light_schedule: ~p, Binary: ~p~n", [Nodeid, Binary]),
	 Wspid = gproc:where({n, l, Nodeid}),
	 case Wspid of undefined -> lager:info("Node ~p is not connected, not sending to it~n", [Nodeid]);
      _->          			
	    lager:info("lighting_handler:send_light_schedule is called with Nodeid: ~s for wspid ~p~n",[Nodeid, Wspid]),
		Wspid ! {send_light_schedule, [Binary]}		
	end.

protosize_when_more_than_six(UnformattedRest) ->
	protosize(UnformattedRest,[]).

protosize([], Acc) -> Acc;	
protosize([Sch|Rest], Acc) ->
	{_Id, Level,Yr,M, Mday, Wday,H, Sec, Min,Pri, Qlfr,Mask} = schedules:sch_fields_for_typing(Sch),
	Msg = {'LightingForceState',{'LightingCtrl',Pri, Mask,<<Level>>,Qlfr},'Persistent'},
	Trigger = {Yr, M, Mday, Wday, H, Min, Sec},
	protosize(Rest, [{Msg,Trigger}|Acc]).

split_schedules_by_max_schedule_allowed(Scheduless) when length(Scheduless) > ?Maxschedules ->
	lists:split(?Maxschedules, Scheduless);

split_schedules_by_max_schedule_allowed(Scheduless) ->
	{Scheduless,[]}.

%for testing purposes
send_light_force_command(Nodeid, Binary, Pid) ->		
	case is_pid(Pid) of
		true ->
		    lager:info("lighting_handler:send_light_schedule is called with Nodeid: ~s for wspid ~p~n",[Nodeid, Pid]),
			Pid ! {send_light_schedule, [Binary]};
		_-> lager:info("node not connected, wait until connect ~p~n",[Nodeid]),
			[]
	end.
