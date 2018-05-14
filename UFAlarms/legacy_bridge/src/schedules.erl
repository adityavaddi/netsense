-module(schedules).
-compile(export_all).
-define(Maxschedules, 24).
-define(DaysOfWeek,[sun,mon,tue,wed,thu,fri,sat]).
-define(ClearSchedule,[<<194,1,0>>]).
-define(RunSchedule, [<<170,1,0>>]).
-compile([{parse_transform, lager_transform}]).

%%--------------------Default Schedule Templates
%%No network -> 
%%     if OnTime == OffTime , they send a schedule with 0 id, that stays always on.  
%%                  id = 0, Priority =1, Mask - 0x1, Qualifier = NonetworkQualifier, MaxLevels, Calendar(0)
%%      Otherwise, they send an
%%                  OnTime schedule (id =0), Priority =1, Mask - 0x1, Qualifier = NoNetworkQualifier, MaxLevels , Calendar(OnTime)    
%%                  OffTime schedule (id=1),  Priority =1, Mask - 0x1, Qualifier = NoNetworkQualifier,  OffLevels , Calendar(OffTime)
%%     Ids are important because node searches through schedules with the id and compares if this is existing schedule or not. 
%%     2.  With network -> (main schedule)
%%     If OnTime == OffTime, they send a schedule using
%%                    id= 2, priority = 7,  Mask = 0, Qualifiers = 0x1, MaxLevels, Calendar(0)
%%    Otherwise 
%%                  OnTime schedule (id =2), Priority =7, Mask - 0x1, Qualifier =0, MaxLevels, Calendar(OnTime)     
%%                  OffTime schedule (id=3),  Priority =7, Mask - 0x1, Qualifier = 0,  OffLevels, Calendar(OffTime)
%% 
%%     In addition to this, they send
%%     3.  Dimming schedule  
%%     If  Dimtime == Undimtime ->
%%                  DimTime schedule (id =4), Priority =5, Mask - 0x1, Qualifier =0, MinLevels ,Calendar(0)   
%%     Otherwise
%%                 DimTime schedule (id =4), Priority =5, Mask - 0x1, Qualifier =0, MinLevels, Calendar(DimTime)       
%%                 UnDimTime schedule (id=5),  Priority =5, Mask - 0x1, Qualifier = 0,  OffLevels, Calendar(UnDimTime)
%%---------------------------  

clearSchedule() ->
	Msg = {'LightingClearSchedule'},
	unode_proto_handler:encode_msgs([Msg]).

runSchedule() ->
	Msg = {'LightingSetAuto'},
	unode_proto_handler:encode_msgs([Msg]).

build_schedules(Schedules) ->
	Builts = build_eight(Schedules, []),
	helpers:flatten3([schedules:clearSchedule(),Builts, schedules:runSchedule()]).

build_eight([], Acc) -> Acc;
build_eight([Sch|Rest], Acc) -> 
	Built = build_schedule(Sch),
	build_eight(Rest, [Built|Acc]).

%{3,0,undefined,undefined,undefined,1,14,0,18,7,0,1}
build_schedule_no_binary(Sch) ->
	{Id, Level,Yr,M, Mday, Wday,H, Sec, Min, Pri, Qlfr,Mask} = Sch,
	{'LightingScheduledEvent', Id , {'CalendarEvent', Sec, Min, H, Wday, Mday, M, Yr},
		{'LightingCtrl',Pri, Mask, <<Level>>, Qlfr}}.

build_schedule (Sch) ->
	{Id, Level,Yr,M, Mday, Wday,H, Sec, Min, Pri, Qlfr,Mask} = Sch,
	Msg = {'LightingScheduledEvent', Id , {'CalendarEvent', Sec, Min, H, Wday, Mday, M, Yr},
		{'LightingCtrl',Pri, Mask, <<Level>>, Qlfr}},
	unode_proto_handler:encode_msgs(Msg).

star_translations([]) -> none;

star_translations({Y,M,D,Wd,H,Min,Sec}) ->
  Yr = case  Y =:= undefined of true -> '*'; _-> Y end,
  Mo = case M =:= undefined of true -> '*'; _-> M end,
  Day = case D =:= undefined of true -> '*'; _-> D end,

  Hr = case H =:= '*' of true -> 0; _-> H end,
  Minn = case Min =:= '*' of true -> 0; _-> Min end,
  Secc = case Sec =:= '*' of true -> 0; _-> Sec end,
  case Wd =:= undefined of true ->
  	{{Yr, Mo, Day},{Hr, Minn, Secc}};
  	 _-> {{Yr, Mo, Day}, lists:nth(Wd, ?DaysOfWeek), {Hr, Minn, Secc}}
  end.

sch_fields_for_typing(Sch) ->
	Yr = case maps:is_key("year", Sch) of
	 	true -> biterizeyear(maps:get("year", Sch));
	 	_-> undefined
		end,
	M = case maps:is_key("mon", Sch) of
	 	true -> biterize(maps:get("mon", Sch));
	 	_-> undefined
	end,
	Mday = case maps:is_key("mday", Sch) of
	 	true -> biterize(maps:get("mday", Sch));
	 	_-> undefined
	end,
	Wday = case maps:is_key("wday", Sch) of
	 	true -> maps:get("wday", Sch);
	 	_-> undefined
	end,
	Id = maps:get("id",Sch),
	H = maps:get("hr", Sch),
	Sec = maps:get("sec", Sch),
	Min = maps:get("min", Sch),
	Qlfr = maps:get("qualifiers", Sch),
	Mask = maps:get("mask", Sch),
	Level = maps:get("level", Sch),
	Pri = maps:get("pri", Sch),
	{Id, Level,Yr,M, Mday, Wday,H, Sec, Min,Pri, Qlfr,Mask}.

%{6,0,undefined,undefined,undefined,8,0,0,0,2,0,1}
group_wkdays([],Grps) -> Grps;

group_wkdays([H|R],Grps) ->
	{_Id, Level,Yr,M, Mday, Wday,Hr, Sec, Min,Pri, Qlfr,Mask} = H,
	Key = {Level,Yr,M,Mday,Hr,Sec,Min,Pri,Qlfr,Mask},
	NGrps = case lists:keymember(Key,1, Grps) of 
		true -> 
		    Vals = [Wday |proplists:get_value(Key, Grps)],
			lists:keyreplace(Key,1,Grps,{Key,Vals});
		_ -> [{Key, [Wday]}| Grps]
	end,
	group_wkdays(R,NGrps).


group_weekdays_for_bitmask(Schedules) ->
	Weekday_groups = [sch_fields_for_typing(S) || S <- Schedules, maps:is_key("wday", S) == true],
	Rest = [sch_fields_for_typing(S) || S <- Schedules, maps:is_key("wday", S) == false],
	Grouped = group_wkdays(Weekday_groups,[]),
	Wklies = add_id_wkmask(Grouped, length(Schedules),[]),
	[Wklies | Rest].

test_weekday_bitmask_results() ->
	[[{28,0,undefined,undefined,undefined,28,0,0,0,2,0,1},
  {27,60,undefined,undefined,undefined,14,17,0,0,2,0,1},
  {26,0,undefined,undefined,undefined,14,19,0,0,2,0,1},
  {25,20,undefined,undefined,undefined,14,19,0,30,2,0,1},
  {24,50,undefined,undefined,undefined,16,17,0,0,2,128,1},
  {23,0,undefined,undefined,undefined,2,0,0,0,2,128,1}],
 {0,30,undefined,undefined,undefined,undefined,13,0,18,0,4,1},
 {1,80,undefined,undefined,undefined,undefined,3,0,3,0,4,1},
 {2,50,undefined,undefined,undefined,undefined,17,0,0,3,0,1},
 {3,0,undefined,undefined,undefined,undefined,19,0,0,3,0,1},
 {4,10,undefined,undefined,undefined,undefined,19,0,30,3,0,1},
 {5,0,undefined,undefined,undefined,undefined,0,0,0,3,0,1},
 {20,50,1,32,4194304,undefined,17,0,0,1,128,1},
 {21,30,1,32,4194304,undefined,17,0,10,1,0,1},
 {22,0,1,32,8388608,undefined,0,0,0,1,0,1}].

grouped_weekdays() -> [{{0,undefined,undefined,undefined,0,0,0,2,128,1},[2]},
 {{50,undefined,undefined,undefined,17,0,0,2,128,1},[5]},
 {{20,undefined,undefined,undefined,19,0,30,2,0,1},[3,2,4]},
 {{0,undefined,undefined,undefined,19,0,0,2,0,1},[3,2,4]},
 {{60,undefined,undefined,undefined,17,0,0,2,0,1},[3,2,4]},
 {{0,undefined,undefined,undefined,0,0,0,2,0,1},[3,5,4]}].

add_id_wkmask([],_LastId, Bitmasked) ->	
	Bitmasked;
	
add_id_wkmask([H|R],LastId, Bitmasked) ->	
	{{Level,Yr,M, Mday,Hr, Sec, Min, Pri, Qlfr,Mask}, Wdays} = H,
	Wkdaymask = trunc(lists:sum([math:pow(2,D-1) || D <- Wdays])),
	add_id_wkmask(R, LastId+1, [{LastId ,Level,Yr,M, Mday,Wkdaymask,Hr,Sec, Min, Pri, Qlfr,Mask}|Bitmasked]).
	
build_primary_schedules(Schedules) ->
    lists:map(fun(S) -> 
		Details = schedules:sch_fields_for_typing(S),
        build_schedule(Details)
	end, Schedules).


get_all_nows(Nows,Weeklies, Dateds) ->
    Len = length(Nows),
    WeeklyLen = length(Weeklies),
    %DatedLen = length(Dateds),
    case 
    	(Len + WeeklyLen) >= ?Maxschedules of
    	true -> 
    	{WSplits,_} = lists:split(?Maxschedules - Len, Weeklies),
    	WBins = [schedules:build_schedule(S) || S <- WSplits],
    	helpers:flatten3([Nows|WBins]);
    	_->
    	{Splits, _} = lists:split(?Maxschedules -(Len + WeeklyLen), Dateds),
    	WBins = [schedules:build_schedule(S) || S <- Weeklies],
    	DBins = [schedules:build_schedule(S) || S <- Splits],
    	helpers:flatten3([Nows|[WBins|DBins]])   	
    end.

build_all_schedules(Schedules) ->
	lists:map(fun(Sch) ->
		%Details = sch_fields_for_typing(Sch),
		build_schedule(Sch)
	end, Schedules).

combine_clear_run_24(Schedules1) ->
	All = build_all_schedules(Schedules1),
	helpers:flatten3([?ClearSchedule,All, ?RunSchedule]).

combine_clear_run_more_than_24(Schedules1, Nodeids)->
    {Togo ,Rest} = lists:split(?Maxschedules, Schedules1),
    lager:debug("we are shipping ~p and Rest to be scheduled: ~p~n",[Togo,Rest]),
    GoesNow = combine_clear_run_24(Togo),
	%[Dailies,Weeklies, Dateds] = separate_primary_schedules(Togo, [], [], []),
	%BinaryLaters = get_all_nows(Dailies,Weeklies, Dateds),
	%Ddays = set_default_days(Weeklies),
	%lager:debug("BinaryNows:~p, Dailies: ~p, Dateds: ~p~n",[BinaryLaters, Weeklies, Dateds]),
	%spawn(fun() ->erlcron_handler:schedule_rest(Nodeids,{BinaryLaters,Ddays},Weeklies,Dateds) end),
	GoesNow.

separate_primary_schedules([], Nows, Weeklies, Laters) -> [Nows,Weeklies, Laters];
separate_primary_schedules([Details|Rest], Nows, Weeklies, Laters) when length(Nows) =< ?Maxschedules -> 
	%Details = sch_fields_for_typing(HeadSch),
	lager:debug("Details bfore case:: ~p~n",[Details]),
	{_Id,_Level,_Yr,_M,_Mday,Wday,_H,_Sec,_Min,_Pri,_Qlfr,_Mask} = Details,
    case Details of
    	{_, _,undefined,undefined, undefined, undefined,_, _, _, _, _,_} ->
			lager:debug("Scheduling dailies to send now: ~p",[Details]),
			ProtoBin = build_schedule(Details),
			separate_primary_schedules(Rest, [ProtoBin|Nows], Weeklies, Laters);
		{_, _,undefined,undefined, undefined, Wday,_, _, _, _, _,_} ->
		    lager:debug("Scheduling weeklies to send now or later: ~p",[Details]),
			%Msg = build_schedule_no_binary(Details),
			separate_primary_schedules(Rest, Nows, [Details|Weeklies],Laters);
		_->
			lager:debug("Scheduling Dateds, definitely crontabbed: ~p~n",[Details]),
			%Msg = schedules:build_schedule_no_binary(Details),
			separate_primary_schedules(Rest, Nows, Weeklies, [Details|Laters])
	end;

separate_primary_schedules([Details|Rest], Nows, Weeklies, Laters) ->
    %Details = schedules:sch_fields_for_typing(HeadSch),
	lager:debug("Laters:~p~n",[Details]),
	Msg = build_schedule_no_binary(Details),
	separate_primary_schedules(Rest, Nows, Weeklies, [Msg|Laters]).


test_primary_schedules() ->
 Schedules = testschedules(),
 schedules:separate_primary_schedules(Schedules,[],[],[]).

set_default_days(Weeklies) ->
	Wdays = [element(6,W) || W <- Weeklies],
	DDays = [case lists:member(Y,Wdays) of false -> Y; _-> 0 end || Y <- lists:seq(1,7)],
	[Y || Y <- DDays , Y =/= 0].

set_default_schedule_days(Defaults, Weeklies) ->
    Protos = [build_schedule(W) || W <- Weeklies],
    Binaries = helpers:flatten3([?ClearSchedule,Protos, ?RunSchedule]),
    Size = length(Weeklies),
	[case lists:nth(S,Weeklies) of [] -> 
		{{undefined,undefined,undefined,S,24,10,0},Defaults}; 
		Grp -> 		     			 
			 [begin {_,_,_,_,_,Wday,H,Sec,Min,_,_,_} = W, {{undefined,undefined,undefined,Wday,H, Min,Sec}, Binaries} end || W <- Grp]
		end	
			 || S<-lists:seq(1,Size)].

%{'LightingScheduledEvent',3,{'CalendarEvent',0,18,14,1,undefined,undefined,undefined},{'LightingCtrl',7,1,<<0>>,0}}
group_weekly([],Groups) -> Groups;

group_weekly([H|R], [Day1,Day2,Day3,Day4,Day5,Day6,Day7]) ->    
	case H of 
	{_,_,_,_,_,1,_,_,_,_,_,_} -> group_weekly(R,[[H|Day1],Day2,Day3,Day4,Day5,Day6,Day7]);
  	{_,_,_,_,_,2,_,_,_,_,_,_} -> group_weekly(R,[Day1,[H|Day2],Day3,Day4,Day5,Day6,Day7]);
	{_,_,_,_,_,3,_,_,_,_,_,_} -> group_weekly(R,[Day1,Day2,[H|Day3],Day4,Day5,Day6,Day7]);
	{_,_,_,_,_,4,_,_,_,_,_,_} -> group_weekly(R,[Day1,Day2,Day3,[H|Day4],Day5,Day6,Day7]);
	{_,_,_,_,_,5,_,_,_,_,_,_} -> group_weekly(R,[Day1,Day2,Day3,Day4,[H|Day5],Day6,Day7]);
	{_,_,_,_,_,6,_,_,_,_,_,_} -> group_weekly(R,[Day1,Day2,Day3,Day4,Day5,[H|Day6],Day7]);
	{_,_,_,_,_,7,_,_,_,_,_,_} -> group_weekly(R,[Day1,Day2,Day3,Day4,Day5,Day6,[H|Day7]])
	end.

%[6,7,1,2,3,4,5]
get_day_order_by_today() ->
	{{Y,M,D},_} = calendar:universal_time(),
    [calendar:day_of_the_week(Y,M,D+DD) || DD <-lists:seq(1, 7)].

dateds() ->
	[{3,0,2016,12,31,undefined,15,0,22,7,0,1},
        {2,100,2017,1,1,undefined,1,0,1,7,0,1},
        {1,0,2016,12,31,undefined,15,0,22,1,4,1},
        {0,100,2017,1,1,undefined,1,0,1,1,4,1}].

group_dates(Dated) ->
	Protos = [build_schedule(S) || S <- Dated],
	Binaries = helpers:flatten3([schedules:clearSchedule(),Protos, schedules:runSchedule()]),
	[begin {_,_,Yr,M,Mday,_,H,Sec,Min,_,_,_} = S, {{Yr,M,Mday,H,Sec,Min}, Binaries} end || S <- Dated].

%%node deals with dates in bit integer, so can't send 6 for month of june, i.e we have to send 32 for June
%wd=ff, md=ffffffff, m=ffff, y=ffff
biterize(Num) ->
	 trunc(math:pow(2,Num-1)).

biterizeyear(Yr) ->
	 Bit = (Yr rem 16) bsl 1,
	 trunc(math:pow(2,Bit)).

test_group_weekly() ->
	Weeklies = weeklies_details(),
	group_weekly(Weeklies,[[],[],[],[],[],[],[]]).

test_dateds() ->
	Dateds = dateds(),
	group_dates(Dateds).

test_default_schedule_days() ->
	Primary_schedules = primary_schedules_results(),
	[Defaults, Weeklies, Dateds] = Primary_schedules,
	io:format("Dateds:~p~n",[Dateds]),
	%Weeklies = group_weekly(Weekliess,[[],[],[],[],[],[],[]]),
	set_default_schedule_days(Defaults,Weeklies).

erlcron_defaults() ->
	[{'LightingScheduledEvent',5,{'CalendarEvent',0,0,12,undefined,undefined,undefined,undefined},{'LightingCtrl',5,1,<<"d">>,128}},
 {'LightingScheduledEvent',4,{'CalendarEvent',0,0,6,undefined,undefined,undefined,undefined},{'LightingCtrl',5,1,<<"2">>,0}},
{'LightingScheduledEvent',3,{'CalendarEvent',0,16,14,undefined,undefined,undefined,undefined},{'LightingCtrl',7,1,<<0>>,0}},
{'LightingScheduledEvent',2,{'CalendarEvent',0,35,2,undefined,undefined,undefined,undefined},{'LightingCtrl',7,1,<<"d">>,0}},
{'LightingScheduledEvent',1,{'CalendarEvent',0,46,13,undefined,undefined,undefined,undefined},{'LightingCtrl',1,1,<<0>>,4}},
{'LightingScheduledEvent',0,{'CalendarEvent',0,35,2,undefined,undefined,undefined,undefined},{'LightingCtrl',1,1,<<"d">>,4}},
{'LightingClearSchedule'},
{'LightingSetAuto'}].

weeklies() ->
[
{'LightingScheduledEvent',3,{'CalendarEvent',0,18,14,1,undefined,undefined,undefined},{'LightingCtrl',7,1,<<0>>,0}},
{'LightingScheduledEvent',2,{'CalendarEvent',0,34,2,2,undefined,undefined,undefined},{'LightingCtrl',7,1,<<"d">>,0}},
{'LightingScheduledEvent',1,{'CalendarEvent',0,48,13,1,undefined,undefined,undefined},{'LightingCtrl',1,1,<<0>>,4}},
{'LightingScheduledEvent',0,{'CalendarEvent',0,34,2,2,undefined,undefined,undefined},{'LightingCtrl',1,1,<<"d">>,4}},
{'LightingScheduledEvent',3,{'CalendarEvent',0,9,14,7,undefined,undefined,undefined},{'LightingCtrl',7,1,<<0>>,0}},
{'LightingScheduledEvent',2,{'CalendarEvent',0,40,2,1,undefined,undefined,undefined},{'LightingCtrl',7,1,<<"d">>,0}},
{'LightingScheduledEvent',1,{'CalendarEvent',0,39,13,7,undefined,undefined,undefined},{'LightingCtrl',1,1,<<0>>,4}},
{'LightingScheduledEvent',0,{'CalendarEvent',0,40,2,1,undefined,undefined,undefined},{'LightingCtrl',1,1,<<"d">>,4}}].

testschedules() ->
[
#{<<"hr">> => 2,<<"id">> => 0,<<"level">> => 100,<<"mask">> => 1,<<"min">> => 35,<<"pri">> => 1,<<"qualifiers">> => 4,<<"sec">> => 0},
#{<<"hr">> => 13,<<"id">> => 1,<<"level">> => 0,<<"mask">> => 1,<<"min">> => 46,<<"pri">> => 1,<<"qualifiers">> => 4,<<"sec">> => 0},
#{<<"hr">> => 2,<<"id">> => 2,<<"level">> => 100,<<"mask">> => 1,<<"min">> => 35,<<"pri">> => 7,<<"qualifiers">> => 0,<<"sec">> => 0},
#{<<"hr">> => 14,<<"id">> => 3,<<"level">> => 0,<<"mask">> => 1,<<"min">> => 16,<<"pri">> => 7,<<"qualifiers">> => 0,<<"sec">> => 0},
#{<<"hr">> => 6,<<"id">> => 4,<<"level">> => 50,<<"mask">> => 1,<<"min">> => 0,<<"pri">> => 5,<<"qualifiers">> => 0,<<"sec">> => 0},
#{<<"hr">> => 12,<<"id">> => 5,<<"level">> => 100,<<"mask">> => 1,<<"min">> => 0,<<"pri">> => 5,<<"qualifiers">> => 128,<<"sec">> => 0},
#{<<"hr">> => 13,<<"id">> => 6,<<"level">> => 50,<<"mask">> => 1,<<"min">> => 46,<<"pri">> => 5,<<"qualifiers">> => 0,<<"sec">> => 0},
#{<<"hr">> => 2,<<"id">> => 7,<<"level">> => 50,<<"mask">> => 1,<<"min">> => 5,<<"pri">> => 5,<<"qualifiers">> => 128,<<"sec">> => 0},
#{<<"hr">> => 1,<<"id">> => 0,<<"level">> => 100,<<"mask">> => 1,<<"mday">> => 1,<<"min">> => 1,<<"mon">> => 1,<<"pri">> => 1,<<"qualifiers">> => 4,<<"sec">> => 0,<<"year">> => 2017},
#{<<"hr">> => 15,<<"id">> => 1,<<"level">> => 0,<<"mask">> => 1,<<"mday">> => 31,<<"min">> => 22,<<"mon">> => 12,<<"pri">> => 1,<<"qualifiers">> => 4,<<"sec">> => 0,<<"year">> => 2016},
#{<<"hr">> => 1,<<"id">> => 2,<<"level">> => 100,<<"mask">> => 1,<<"mday">> => 1,<<"min">> => 1,<<"mon">> => 1,<<"pri">> => 7,<<"qualifiers">> => 0,<<"sec">> => 0,<<"year">> => 2017},
#{<<"hr">> => 15,<<"id">> => 3,<<"level">> => 0,<<"mask">> => 1,<<"mday">> => 31,<<"min">> => 22,<<"mon">> => 12,<<"pri">> => 7,<<"qualifiers">> => 0,<<"sec">> => 0,<<"year">> => 2016},
#{<<"hr">> => 13,<<"id">> => 1,<<"level">> => 0,<<"mask">> => 1,<<"min">> => 39,<<"pri">> => 1,<<"qualifiers">> => 4,<<"sec">> => 0,<<"wday">> => 7},
#{<<"hr">> => 14,<<"id">> => 3,<<"level">> => 0,<<"mask">> => 1,<<"min">> => 9,<<"pri">> => 7,<<"qualifiers">> => 0,<<"sec">> => 0,<<"wday">> => 7},
#{<<"hr">> => 2,<<"id">> => 0,<<"level">> => 100,<<"mask">> => 1,<<"min">> => 40,<<"pri">> => 1,<<"qualifiers">> => 4,<<"sec">> => 0,<<"wday">> => 1},
#{<<"hr">> => 13,<<"id">> => 1,<<"level">> => 0,<<"mask">> => 1,<<"min">> => 48,<<"pri">> => 1,<<"qualifiers">> => 4,<<"sec">> => 0,<<"wday">> => 1},
#{<<"hr">> => 14,<<"id">> => 3,<<"level">> => 0,<<"mask">> => 1,<<"min">> => 18,<<"pri">> => 7,<<"qualifiers">> => 0,<<"sec">> => 0,<<"wday">> => 1},
#{<<"hr">> => 2,<<"id">> => 0,<<"level">> => 100,<<"mask">> => 1,<<"min">> => 34,<<"pri">> => 1,<<"qualifiers">> => 4,<<"sec">> => 0,<<"wday">> => 2}
#{<<"hr">> => 2,<<"id">> => 2,<<"level">> => 100,<<"mask">> => 1,<<"min">> => 34,<<"pri">> => 7,<<"qualifiers">> => 0,<<"sec">> => 0,<<"wday">> => 2}
].

test_group_weekdays() ->
    Schedules = tester:bitmask_schedules(),
 	group_weekdays_for_bitmask(Schedules).

primary_schedules_results() ->
[[{7,50,undefined,undefined,undefined,undefined,2,0,5,5,128,
   1},
  {6,50,undefined,undefined,undefined,undefined,13,0,46,5,0,1},
  {5,100,undefined,undefined,undefined,undefined,12,0,0,5,128,
   1},
  {4,50,undefined,undefined,undefined,undefined,6,0,0,5,0,1},
  {3,0,undefined,undefined,undefined,undefined,14,0,16,7,0,1},
  {2,100,undefined,undefined,undefined,undefined,2,0,35,7,0,1},
  {1,0,undefined,undefined,undefined,undefined,13,0,46,1,4,1},
  {0,100,undefined,undefined,undefined,undefined,2,0,35,1,4,
   1}],
 [{2,100,undefined,undefined,undefined,2,2,0,34,7,0,1},
  {3,0,undefined,undefined,undefined,1,14,0,18,7,0,1},
  {1,0,undefined,undefined,undefined,1,13,0,48,1,4,1},
  {0,100,undefined,undefined,undefined,1,2,0,40,1,4,1},
  {3,0,undefined,undefined,undefined,7,14,0,9,7,0,1},
  {1,0,undefined,undefined,undefined,7,13,0,39,1,4,1}],
 [{3,0,2016,12,31,undefined,15,0,22,7,0,1},
  {2,100,2017,1,1,undefined,1,0,1,7,0,1},
  {1,0,2016,12,31,undefined,15,0,22,1,4,1},
  {0,100,2017,1,1,undefined,1,0,1,1,4,1}]].

  weeklies_details () ->
  	[{2,100,undefined,undefined,undefined,2,2,0,34,7,0,1},
  {3,0,undefined,undefined,undefined,1,14,0,18,7,0,1},
  {1,0,undefined,undefined,undefined,1,13,0,48,1,4,1},
  {0,100,undefined,undefined,undefined,1,2,0,40,1,4,1},
  {3,0,undefined,undefined,undefined,7,14,0,9,7,0,1},
  {1,0,undefined,undefined,undefined,7,13,0,39,1,4,1}].