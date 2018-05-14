-module(erlcron_handler).
-define(DaysOfWeek,[sun,mon,tue,wed,thu,fri,sat]).
-compile(export_all).
-define(ClearSchedule,[<<194,1,0>>]).
-define(RunSchedule, [<<170,1,0>>]).

is_valid_trigger([]) -> none;

is_valid_trigger({Y,M,D,Wd,H,Min,S}) when Y=:=undefined,M=:=undefined,D=:=undefined,Wd=:= undefined ->
	Hr = zeroto24(H),
    Trigger = {daily, {Hr, Min, S}},
    Valid = erlcron:validate(Trigger),
    lager:debug("Trigger: ~p is ~p ~n",[Trigger, Valid]),
    {Valid,Trigger};

is_valid_trigger({_Y,_M,_D,Wd,H,Min,S}) when Wd =/= undefined ->
	Hr = zeroto24(H),
    Trigger = {weekly, lists:nth(Wd,?DaysOfWeek) , {Hr, Min, S}},
    Valid = erlcron:validate(Trigger),
    lager:debug("Trigger: ~p is ~p ~n",[Trigger, Valid]),
    {Valid,Trigger};
	
is_valid_trigger({Y,M,D,Wd,H,Min,S}) when Y =/= undefined , M =/= undefined, D =/=undefined ->
	Secsnow = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
	Secstrig = calendar:datetime_to_gregorian_seconds({{Y,M,D},{H, Min, S}}),
	InFutureSeconds = Secstrig - Secsnow,
	case InFutureSeconds < 0 of true ->
		lager:error("Can't have dates in the past, will not schedule ~p,~n", {Y,M,D,Wd,H,Min,S}),
		{invalid,{Y,M,D,Wd,H,Min,S}};
	_->
	    Trigger = {once, InFutureSeconds},
	    Valid = erlcron:validate(Trigger),
	    lager:debug("Trigger: ~p is ~p ~n",[Trigger, Valid]),
	    {Valid,Trigger}
	end.
	
zeroto24(H) ->
	case H =:= 0 of true -> 24; _ -> H end.

schedule_rest(_Nodeids, {[],[]},[], []) ->
	lager:info("nothing to cron schedule");

%fun() -> lighting_handler:send_light_schedule(Nodeid, Binary) end)
%{'LightingScheduledEvent',3,{'CalendarEvent',0,18,14,1,undefined,undefined,undefined},{'LightingCtrl',7,1,<<0>>,0}}
%{6,50,undefined,undefined,undefined,undefined,13,0,46,5,0,1}}
%{Id, Level,Yr,M, Mday, Wday,H, Sec, Min,Pri, Qlfr,Mask}
schedule_rest(Nodeids,{SendNows,Ddays},Dailies,Dateds) ->
	application:ensure_started(erlcron),
	erlcron:set_datetime(erlang:localtime()),
	lager:debug("Running cron schedule for Nodeids: ~p~n",[Nodeids]),
	ValidDailies = check_for_validity(Dailies),
	ValidDateds = check_for_validity(Dateds),
	schedule_dailies(SendNows,Ddays, Nodeids),
	schedule_valids(ValidDailies,Nodeids),
	schedule_valids(ValidDateds, Nodeids).

check_for_validity(Dates) ->
	lists:map(fun(Row) ->
		case Row of [] -> lager:debug("Empty row for scheduling, will pass");
		_ -> 
		     {_Id, _Level,Yr,M, Mday, Wday,H, Sec, Min,_Pri, _Qlfr,_Mask} = Row,
			 Trigger = {Yr, M, Mday, Wday, H, Min, Sec},
			 {Valid,Cronized} = is_valid_trigger(Trigger),
			 lager:debug("Valid, cronized ~p, ~p~n",[Valid, Cronized]),
			 case Valid of valid -> 
			 	lager:info("Schedule ~p is valid. Will be scheduled!!",[Row]),
			 	{Cronized, schedules:build_schedule(Row)} ;
			 	 _-> lager:info("Schedule ~p is not valid. Not scheduled!!",[Row]) 
			 end
		end
	end, Dates).

schedule_dailies(SendNows, DDays, Nodeids) ->
	lists:map(fun(D) -> 
		lager:debug("sending defaults for day not existing in weekly schedule ~p~n",[D]),
		Trigger = {undefined, undefined, undefined, D, 1, 10, 0},
		%Cronized = {{weekly, lists:nth(D,?DaysOfWeek), {12, am}}},
		{_Valid, Cronized} = is_valid_trigger(Trigger),
		Job = {Cronized,{lighting_handler, send_light_schedule, [{Nodeids, SendNows}]}},
		lager:debug("Scheduled erlcronJob: ~p~n",[Job]),
		JobRef = erlcron:cron(Job),
		lager:debug("Scheduled JobRef: ~p ~n",[JobRef]),
 		JobRef
	end, DDays).

schedule_valids([], _) -> lager:debug("No valid entries to schedule");

schedule_valids(Valids, Nodeids)  ->
    lager:debug("Valid job scheduling ~p, will add to quartz:~n",[Valids]),
    Binaries = [begin {_, Bin} = Z, Bin  end || Z <- Valids, Z =/= ok],	
    CronizedDates = [begin {Cr,_} = Z, Cr end || Z <- Valids, Z =/= ok],
	Crons = helpers:flatten3([?ClearSchedule,Binaries, ?RunSchedule]),
	lists:map(fun(Cronized) -> 
		Job = {Cronized,{lighting_handler, send_light_schedule, [{Nodeids, Crons}]}},
		lager:debug("Scheduled erlcronJob: ~p~n",[Job]),
		JobRef = erlcron:cron(Job),
		lager:debug("Scheduled JobRef: ~p ~n",[JobRef]),
 		JobRef
	end,CronizedDates).

test_valid_trigger_newyear() ->
	Trigger = {2016, 12, 31, undefined, 13, 31,0},
	is_valid_trigger(Trigger).
 