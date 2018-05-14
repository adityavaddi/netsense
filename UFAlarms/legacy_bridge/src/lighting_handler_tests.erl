-module(lighting_handler_tests).
-author("Gina Hagg <ghagg@ysensity.com").
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).
 
build_schedules_sorted_test() ->
    Schedules = tester:schedule_map(),
    Expected = <<194,1,0>>,
    Schedules1 = maps:get(<<"schedules">>, Schedules),
    Scheduless = lists:sort(fun(X,Y) -> maps:get(<<"id">>, X) < maps:get(<<"id">>, Y) end, Schedules1),
    {SendNow , _TobeScheduled } = lighting_handler:split_schedules_by_max_schedule_allowed(Scheduless),
    Binaries = schedules:build_schedules(SendNow),
    Return = lists:nth(1, Binaries),
    ?assertEqual(Expected, Return).


schedulerest_test() ->
    S = tester:exactly_6_schedules(),
    Nodeids = [<<"N01232ea5">>],
    lighting_handler:schedule_rest(Nodeids,S).

protosize_when_more_than_six_test() -> 
    B = tester:more_than_8_schedules(),
    Exp = {{'LightingForceState',{'LightingCtrl',5,1,<<0>>,0},'Persistent'},{{'*','*','*'},sun,{23,0,0}}},
    Ret = lists:nth(1,lighting_handler:protosize_when_more_than_six(B)),
    ?assertEqual(Exp,Ret).

build_schedules_test() ->
    Schedules = tester:more_than_8_schedules(),
    Ret = schedules:build_schedules(Schedules),
    ?assertEqual(is_list(Ret), true).

erlcron_schedule_test() ->
    application:start(erlcron),
    B = tester:just_two(),
    Nodeids = [<<"N01232ea5">>],
    TobeScheduled = lighting_handler:protosize_when_more_than_six(B),
    S = erlcron_handler:schedule_chron(Nodeids, TobeScheduled),
    ?assertEqual(is_list(S), true). 

just_two() ->
    [{{'LightingForceState',{'LightingCtrl',5,1,<<0>>,0},'Persistent'},
  {{2016,2,15},{7,23,0}}
  },
 {{'LightingForceState',{'LightingCtrl',5,1,<<"d">>,128},'Persistent'},
  {{2016,12,15},{0,0,0}}}].
    