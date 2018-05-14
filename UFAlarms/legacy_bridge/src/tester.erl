-module(tester).

-compile(export_all).

-compile([{parse_transform, lager_transform}]).

%%%----------------------------------------------------------------------------
%%						test functions for quick console testing. move out to unittest later
%%%------------------------------------------------------------------------------

testprimaryschedules() ->
    schedules:testprimaryschedules().

bitmask_schedules() ->
    [#{"hr" => 13, "id" => 0, "level" => 30, "mask" => 1,
       "min" => 18, "pri" => 0, "qualifiers" => 4, "sec" => 0},
     #{"hr" => 3, "id" => 1, "level" => 80, "mask" => 1,
       "min" => 3, "pri" => 0, "qualifiers" => 4, "sec" => 0},
     #{"hr" => 17, "id" => 2, "level" => 50, "mask" => 1,
       "min" => 0, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 19, "id" => 3, "level" => 0, "mask" => 1,
       "min" => 0, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 19, "id" => 4, "level" => 10, "mask" => 1,
       "min" => 30, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 0, "id" => 5, "level" => 0, "mask" => 1,
       "min" => 0, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 0, "id" => 6, "level" => 0, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 4},
     #{"hr" => 17, "id" => 7, "level" => 60, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 4},
     #{"hr" => 19, "id" => 8, "level" => 0, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 4},
     #{"hr" => 19, "id" => 9, "level" => 20, "mask" => 1,
       "min" => 30, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 4},
     #{"hr" => 17, "id" => 10, "level" => 50, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 128, "sec" => 0,
       "wday" => 5},
     #{"hr" => 0, "id" => 11, "level" => 0, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 5},
     #{"hr" => 0, "id" => 12, "level" => 0, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 128, "sec" => 0,
       "wday" => 2},
     #{"hr" => 17, "id" => 13, "level" => 60, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 2},
     #{"hr" => 19, "id" => 14, "level" => 0, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 2},
     #{"hr" => 19, "id" => 15, "level" => 20, "mask" => 1,
       "min" => 30, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 2},
     #{"hr" => 0, "id" => 16, "level" => 0, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 3},
     #{"hr" => 17, "id" => 17, "level" => 60, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 3},
     #{"hr" => 19, "id" => 18, "level" => 0, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 3},
     #{"hr" => 19, "id" => 19, "level" => 20, "mask" => 1,
       "min" => 30, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 3},
     #{"hr" => 17, "id" => 20, "level" => 50, "mask" => 1,
       "mday" => 23, "min" => 0, "mon" => 6, "pri" => 1,
       "qualifiers" => 128, "sec" => 0, "year" => 2016},
     #{"hr" => 17, "id" => 21, "level" => 30, "mask" => 1,
       "mday" => 23, "min" => 10, "mon" => 6, "pri" => 1,
       "qualifiers" => 0, "sec" => 0, "year" => 2016},
     #{"hr" => 0, "id" => 22, "level" => 0, "mask" => 1,
       "mday" => 24, "min" => 0, "mon" => 6, "pri" => 1,
       "qualifiers" => 0, "sec" => 0, "year" => 2016}].

very_long_schedules() ->
    #{<<"name">> => <<"LightingScheduledEvent">>,
      <<"nodeid">> => [<<"N0gina3">>],
      <<"schedules">> =>
	  [#{<<"hr">> => 2, <<"id">> => 0, <<"level">> => 100,
	     <<"mask">> => 1, <<"min">> => 35, <<"pri">> => 1,
	     <<"qualifiers">> => 4, <<"sec">> => 0},
	   #{<<"hr">> => 13, <<"id">> => 1, <<"level">> => 0,
	     <<"mask">> => 1, <<"min">> => 46, <<"pri">> => 1,
	     <<"qualifiers">> => 4, <<"sec">> => 0},
	   #{<<"hr">> => 2, <<"id">> => 2, <<"level">> => 100,
	     <<"mask">> => 1, <<"min">> => 35, <<"pri">> => 7,
	     <<"qualifiers">> => 0, <<"sec">> => 0},
	   #{<<"hr">> => 14, <<"id">> => 3, <<"level">> => 0,
	     <<"mask">> => 1, <<"min">> => 16, <<"pri">> => 7,
	     <<"qualifiers">> => 0, <<"sec">> => 0},
	   #{<<"hr">> => 6, <<"id">> => 4, <<"level">> => 50,
	     <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
	     <<"qualifiers">> => 0, <<"sec">> => 0},
	   #{<<"hr">> => 12, <<"id">> => 5, <<"level">> => 100,
	     <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
	     <<"qualifiers">> => 128, <<"sec">> => 0},
	   #{<<"hr">> => 13, <<"id">> => 6, <<"level">> => 50,
	     <<"mask">> => 1, <<"min">> => 46, <<"pri">> => 5,
	     <<"qualifiers">> => 0, <<"sec">> => 0},
	   #{<<"hr">> => 2, <<"id">> => 7, <<"level">> => 50,
	     <<"mask">> => 1, <<"min">> => 5, <<"pri">> => 5,
	     <<"qualifiers">> => 128, <<"sec">> => 0},
	   #{<<"hr">> => 1, <<"id">> => 0, <<"level">> => 100,
	     <<"mask">> => 1, <<"mday">> => 1, <<"min">> => 1,
	     <<"mon">> => 1, <<"pri">> => 1, <<"qualifiers">> => 4,
	     <<"sec">> => 0, <<"year">> => 2017},
	   #{<<"hr">> => 15, <<"id">> => 1, <<"level">> => 0,
	     <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 22,
	     <<"mon">> => 12, <<"pri">> => 1, <<"qualifiers">> => 4,
	     <<"sec">> => 0, <<"year">> => 2016},
	   #{<<"hr">> => 1, <<"id">> => 2, <<"level">> => 100,
	     <<"mask">> => 1, <<"mday">> => 1, <<"min">> => 1,
	     <<"mon">> => 1, <<"pri">> => 7, <<"qualifiers">> => 0,
	     <<"sec">> => 0, <<"year">> => 2017},
	   #{<<"hr">> => 15, <<"id">> => 3, <<"level">> => 0,
	     <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 22,
	     <<"mon">> => 12, <<"pri">> => 7, <<"qualifiers">> => 0,
	     <<"sec">> => 0, <<"year">> => 2016},
	   #{<<"hr">> => 2, <<"id">> => 0, <<"level">> => 100,
	     <<"mask">> => 1, <<"min">> => 40, <<"pri">> => 1,
	     <<"qualifiers">> => 4, <<"sec">> => 0, <<"wday">> => 1},
	   #{<<"hr">> => 13, <<"id">> => 1, <<"level">> => 0,
	     <<"mask">> => 1, <<"min">> => 39, <<"pri">> => 1,
	     <<"qualifiers">> => 4, <<"sec">> => 0, <<"wday">> => 7},
	   #{<<"hr">> => 2, <<"id">> => 2, <<"level">> => 100,
	     <<"mask">> => 1, <<"min">> => 40, <<"pri">> => 7,
	     <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 1},
	   #{<<"hr">> => 14, <<"id">> => 3, <<"level">> => 0,
	     <<"mask">> => 1, <<"min">> => 9, <<"pri">> => 7,
	     <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 7},
	   #{<<"hr">> => 2, <<"id">> => 0, <<"level">> => 100,
	     <<"mask">> => 1, <<"min">> => 34, <<"pri">> => 1,
	     <<"qualifiers">> => 4, <<"sec">> => 0, <<"wday">> => 2},
	   #{<<"hr">> => 13, <<"id">> => 1, <<"level">> => 0,
	     <<"mask">> => 1, <<"min">> => 48, <<"pri">> => 1,
	     <<"qualifiers">> => 4, <<"sec">> => 0, <<"wday">> => 1},
	   #{<<"hr">> => 2, <<"id">> => 2, <<"level">> => 100,
	     <<"mask">> => 1, <<"min">> => 34, <<"pri">> => 7,
	     <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 2},
	   #{<<"hr">> => 14, <<"id">> => 3, <<"level">> => 0,
	     <<"mask">> => 1, <<"min">> => 18, <<"pri">> => 7,
	     <<"qualifiers">> => 0, <<"sec">> => 0,
	     <<"wday">> => 1}]}.

new_default_schedules() ->
    #{<<"name">> => <<"LightingScheduledEvent">>,
      <<"nodeid">> => [<<"N0gina3">>],
      <<"schedules">> =>
	  [#{<<"hr">> => 20, <<"id">> => 2, <<"level">> => 60,
	     <<"mask">> => 1, <<"min">> => 11, <<"pri">> => 7,
	     <<"qualifiers">> => 0, <<"sec">> => 0},
	   #{<<"hr">> => 20, <<"id">> => 3, <<"level">> => 0,
	     <<"mask">> => 1, <<"min">> => 25, <<"pri">> => 7,
	     <<"qualifiers">> => 0, <<"sec">> => 0},
	   #{<<"hr">> => 20, <<"id">> => 4, <<"level">> => 20,
	     <<"mask">> => 1, <<"min">> => 15, <<"pri">> => 5,
	     <<"qualifiers">> => 0, <<"sec">> => 0},
	   #{<<"hr">> => 20, <<"id">> => 5, <<"level">> => 0,
	     <<"mask">> => 1, <<"min">> => 20, <<"pri">> => 5,
	     <<"qualifiers">> => 128, <<"sec">> => 0},
	   #{<<"hr">> => 20, <<"id">> => 1, <<"level">> => 0,
	     <<"mask">> => 1, <<"min">> => 25, <<"pri">> => 1,
	     <<"qualifiers">> => 4, <<"sec">> => 0},
	   #{<<"hr">> => 20, <<"id">> => 0, <<"level">> => 60,
	     <<"mask">> => 1, <<"min">> => 11, <<"pri">> => 1,
	     <<"qualifiers">> => 4, <<"sec">> => 0},
	   #{<<"hr">> => 23, <<"id">> => 2, <<"level">> => 75,
	     <<"mask">> => 1, <<"mday">> => 25, <<"min">> => 12,
	     <<"mon">> => 3, <<"pri">> => 7, <<"qualifiers">> => 0,
	     <<"sec">> => 0, <<"year">> => 2016},
	   #{<<"hr">> => 23, <<"id">> => 3, <<"level">> => 0,
	     <<"mask">> => 1, <<"mday">> => 25, <<"min">> => 50,
	     <<"mon">> => 3, <<"pri">> => 7, <<"qualifiers">> => 0,
	     <<"sec">> => 0, <<"year">> => 2016},
	   #{<<"hr">> => 23, <<"id">> => 1, <<"level">> => 0,
	     <<"mask">> => 1, <<"mday">> => 25, <<"min">> => 50,
	     <<"mon">> => 3, <<"pri">> => 1, <<"qualifiers">> => 4,
	     <<"sec">> => 0, <<"year">> => 2016},
	   #{<<"hr">> => 23, <<"id">> => 0, <<"level">> => 75,
	     <<"mask">> => 1, <<"mday">> => 25, <<"min">> => 12,
	     <<"mon">> => 3, <<"pri">> => 1, <<"qualifiers">> => 4,
	     <<"sec">> => 0, <<"year">> => 2016}]}.

defaultschedules() ->
    #{<<"name">> => <<"LightingScheduledEvent">>,
      <<"nodeid">> => [<<"N0gina3">>],
      <<"schedules">> =>
	  [#{<<"hr">> => 20, <<"id">> => 2, <<"level">> => 60,
	     <<"mask">> => 1, <<"min">> => 11, <<"pri">> => 7,
	     <<"qualifiers">> => 0, <<"sec">> => 0},
	   #{<<"hr">> => 20, <<"id">> => 3, <<"level">> => 0,
	     <<"mask">> => 1, <<"min">> => 25, <<"pri">> => 7,
	     <<"qualifiers">> => 0, <<"sec">> => 0},
	   #{<<"hr">> => 20, <<"id">> => 4, <<"level">> => 20,
	     <<"mask">> => 1, <<"min">> => 15, <<"pri">> => 5,
	     <<"qualifiers">> => 0, <<"sec">> => 0},
	   #{<<"hr">> => 20, <<"id">> => 5, <<"level">> => 0,
	     <<"mask">> => 1, <<"min">> => 20, <<"pri">> => 5,
	     <<"qualifiers">> => 128, <<"sec">> => 0},
	   #{<<"hr">> => 20, <<"id">> => 1, <<"level">> => 0,
	     <<"mask">> => 1, <<"min">> => 25, <<"pri">> => 1,
	     <<"qualifiers">> => 4, <<"sec">> => 0},
	   #{<<"hr">> => 20, <<"id">> => 0, <<"level">> => 60,
	     <<"mask">> => 1, <<"min">> => 11, <<"pri">> => 1,
	     <<"qualifiers">> => 4, <<"sec">> => 0},
	   #{<<"hr">> => 23, <<"id">> => 2, <<"level">> => 75,
	     <<"mask">> => 1, <<"mday">> => 25, <<"min">> => 12,
	     <<"mon">> => 4, <<"pri">> => 7, <<"qualifiers">> => 0,
	     <<"sec">> => 0, <<"year">> => 2016},
	   #{<<"hr">> => 23, <<"id">> => 3, <<"level">> => 0,
	     <<"mask">> => 1, <<"mday">> => 25, <<"min">> => 50,
	     <<"mon">> => 4, <<"pri">> => 7, <<"qualifiers">> => 0,
	     <<"sec">> => 0, <<"year">> => 2016},
	   #{<<"hr">> => 23, <<"id">> => 1, <<"level">> => 0,
	     <<"mask">> => 1, <<"mday">> => 25, <<"min">> => 50,
	     <<"mon">> => 4, <<"pri">> => 1, <<"qualifiers">> => 4,
	     <<"sec">> => 0, <<"year">> => 2016},
	   #{<<"hr">> => 23, <<"id">> => 0, <<"level">> => 75,
	     <<"mask">> => 1, <<"mday">> => 25, <<"min">> => 12,
	     <<"mon">> => 4, <<"pri">> => 1, <<"qualifiers">> => 4,
	     <<"sec">> => 0, <<"year">> => 2016}]}.

erlcron_long_schedules() ->
    [{'LightingScheduledEvent', 3,
      {'CalendarEvent', 0, 18, 14, 1, undefined, undefined,
       undefined},
      {'LightingCtrl', 7, 1, <<0>>, 0}},
     {'LightingScheduledEvent', 2,
      {'CalendarEvent', 0, 34, 2, 2, undefined, undefined,
       undefined},
      {'LightingCtrl', 7, 1, <<"d">>, 0}},
     {'LightingScheduledEvent', 1,
      {'CalendarEvent', 0, 48, 13, 1, undefined, undefined,
       undefined},
      {'LightingCtrl', 1, 1, <<0>>, 4}},
     {'LightingScheduledEvent', 0,
      {'CalendarEvent', 0, 34, 2, 2, undefined, undefined,
       undefined},
      {'LightingCtrl', 1, 1, <<"d">>, 4}},
     {'LightingScheduledEvent', 3,
      {'CalendarEvent', 0, 9, 14, 7, undefined, undefined,
       undefined},
      {'LightingCtrl', 7, 1, <<0>>, 0}},
     {'LightingScheduledEvent', 2,
      {'CalendarEvent', 0, 40, 2, 1, undefined, undefined,
       undefined},
      {'LightingCtrl', 7, 1, <<"d">>, 0}},
     {'LightingScheduledEvent', 1,
      {'CalendarEvent', 0, 39, 13, 7, undefined, undefined,
       undefined},
      {'LightingCtrl', 1, 1, <<0>>, 4}},
     {'LightingScheduledEvent', 0,
      {'CalendarEvent', 0, 40, 2, 1, undefined, undefined,
       undefined},
      {'LightingCtrl', 1, 1, <<"d">>, 4}},
     {'LightingScheduledEvent', 3,
      {'CalendarEvent', 0, 22, 15, undefined, 31, 12, 2016},
      {'LightingCtrl', 7, 1, <<0>>, 0}},
     {'LightingScheduledEvent', 2,
      {'CalendarEvent', 0, 1, 1, undefined, 1, 1, 2017},
      {'LightingCtrl', 7, 1, <<"d">>, 0}},
     {'LightingScheduledEvent', 1,
      {'CalendarEvent', 0, 22, 15, undefined, 31, 12, 2016},
      {'LightingCtrl', 1, 1, <<0>>, 4}},
     {'LightingScheduledEvent', 0,
      {'CalendarEvent', 0, 1, 1, undefined, 1, 1, 2017},
      {'LightingCtrl', 1, 1, <<"d">>, 4}},
     {'LightingScheduledEvent', 7,
      {'CalendarEvent', 0, 5, 2, undefined, undefined,
       undefined, undefined},
      {'LightingCtrl', 5, 1, <<"2">>, 128}},
     {'LightingScheduledEvent', 6,
      {'CalendarEvent', 0, 46, 13, undefined, undefined,
       undefined, undefined},
      {'LightingCtrl', 5, 1, <<"2">>, 0}}].

erlcron_schedules() ->
    [{'LightingScheduledEvent', 0,
      {'CalendarEvent', 0, 12, 23, undefined, 25, 4, 2016},
      {'LightingCtrl', 1, 1, <<"K">>, 4}},
     {'LightingScheduledEvent', 1,
      {'CalendarEvent', 0, 50, 23, undefined, 25, 4, 2016},
      {'LightingCtrl', 1, 1, <<0>>, 4}},
     {'LightingScheduledEvent', 3,
      {'CalendarEvent', 0, 50, 23, undefined, 25, 4, 2016},
      {'LightingCtrl', 7, 1, <<0>>, 0}},
     {'LightingScheduledEvent', 2,
      {'CalendarEvent', 0, 12, 23, undefined, 25, 4, 2016},
      {'LightingCtrl', 7, 1, <<"K">>, 0}}].

erlcron_defaults() ->
    [{'LightingScheduledEvent', 5,
      {'CalendarEvent', 0, 0, 12, undefined, undefined,
       undefined, undefined},
      {'LightingCtrl', 5, 1, <<"d">>, 128}},
     {'LightingScheduledEvent', 4,
      {'CalendarEvent', 0, 0, 6, undefined, undefined,
       undefined, undefined},
      {'LightingCtrl', 5, 1, <<"2">>, 0}},
     {'LightingScheduledEvent', 3,
      {'CalendarEvent', 0, 16, 14, undefined, undefined,
       undefined, undefined},
      {'LightingCtrl', 7, 1, <<0>>, 0}},
     {'LightingScheduledEvent', 2,
      {'CalendarEvent', 0, 35, 2, undefined, undefined,
       undefined, undefined},
      {'LightingCtrl', 7, 1, <<"d">>, 0}},
     {'LightingScheduledEvent', 1,
      {'CalendarEvent', 0, 46, 13, undefined, undefined,
       undefined, undefined},
      {'LightingCtrl', 1, 1, <<0>>, 4}},
     {'LightingScheduledEvent', 0,
      {'CalendarEvent', 0, 35, 2, undefined, undefined,
       undefined, undefined},
      {'LightingCtrl', 1, 1, <<"d">>, 4}},
     {'LightingClearSchedule'}, {'LightingSetAuto'}].

testerlcron() ->
    application:start(lager),
    S = erlcron_long_schedules(),
    D = erlcron_defaults(),
    erlcron_handler:schedule_rest([<<"N0gina3">>], D, S).

testdefaultschedules() ->
    lighting_handler:prepare_schedules(defaultschedules()).

loginreqmsg() ->
    {'LoginReq', "N01232ea5", 0, "unode-v4", "5549c61",
     "SensityDefault", "x", 48, "a8733d94", "10.20.108.92",
     1447373849755981}.

newlogin() ->
    Timestamp = os:system_time(micro_seconds),
    {'LoginReq', 0, "unode-v3", "6ab8efe", "SensityQA01",
     "x", "149", "2d80cd53", "192.168.64.213", Timestamp,
     "8A:15:54:CC:52:32", "AC:3F:A4:33:06:18", 3}.

blist() ->
    [{field, nodeId, 1, 2, string, required, []},
     {field, protocolVersion, 2, 3, uint64, required, []},
     {field, clientType, 3, 4, string, optional, []},
     {field, swVerId, 4, 5, string, optional, []},
     {field, netName, 5, 6, string, optional, []},
     {field, profileName, 6, 7, string, optional, []},
     {field, assocChannel, 7, 8, uint32, optional, []},
     {field, configToken, 8, 9, string, optional, []},
     {field, localIP, 9, 10, string, optional, []},
     {field, time, 10, 11, uint64, optional, []}].

basicmap() ->
    #{<<"name">> => <<"LoginReq">>,
      <<"nodeid">> => <<"N01232ea3">>,
      <<"protocolVersion">> => 0,
      "clientType" => <<"unode-v4">>,
      <<"swVerId">> => <<"2b9f02c">>,
      <<"netName">> => <<"XeraL2">>,
      <<"profileName">> => <<"x">>, <<"assocChannel">> => 48,
      <<"configToken">> => <<"f1b5d481">>,
      <<"localIP">> => <<"10.20.109.34">>,
      <<"time">> => 1444778562623260}.

nestedL() ->
    [{field, state, 1, 2, {msg, 'LightingCtrl'}, required,
      []},
     {field, ftype, 2, 3, {enum, 'LightingForceType'},
      optional, []}].

nestedmap() ->
    #{<<"name">> => <<"LightingForceState">>,
      <<"pri">> => 3, <<"mask">> => 1, <<"level">> => 0,
      <<"qualifiers">> => <<"undefined">>,
      <<"ftype">> => <<"Volatile">>}.

packedM() ->
    <<134, 149, 102, 116, 121, 112, 101, 1, 149, 108, 101,
      118, 101, 108, 161, 0, 148, 109, 97, 115, 107, 1, 148,
      110, 97, 109, 101, 220, 0, 18, 76, 105, 103, 104, 116,
      105, 110, 103, 70, 111, 114, 99, 101, 83, 116, 97, 116,
      101, 147, 112, 114, 105, 3, 154, 113, 117, 97, 108, 105,
      102, 105, 101, 114, 115, 153, 117, 110, 100, 101, 102,
      105, 110, 101, 100>>.

%'Messages':encode_msg(list_to_tuple(MM)).
%<<10,7,8,3,16,1,26,1,0,16,2>>
pbLfs() -> <<10, 7, 8, 3, 16, 1, 26, 1, 0, 16, 2>>.

%(legacy_bridge@127.0.0.1)2> helpers:testpb().
%{'LightingForceState',{'LightingCtrl',3,1,<<0>>,undefined},'Volatile'}
%<<10,7,8,3,16,1,26,1,0,16,2>>
testpb() ->
    legacy_msgpack_handler:unpack_msg(msgpack:pack(nestedmap())).

testup() ->
    %Fields = [nodeId,protocolVersion,clientType,swVerId,netName,profileName,assocChannel,configToken,localIP,time],
    legacy_msgpack_handler:build_msg_from_unpacked(blist(),
						   basicmap(), 'LoginReq').

%(legacy_bridge@127.0.0.1)2> helpers:testbup().
%N: 1, K: "nodeId"
%N: 2, K: "protocolVersion"
%N: 3, K: "clientType"
%N: 4, K: "swVerId"
%N: 5, K: "netName"
%N: 6, K: "profileName"
%N: 7, K: "assocChannel"
%N: 8, K: "configToken"
%N: 9, K: "localIP"
%N: 10, K: "time"
%['LoginReq',"N01232ea3",0,"unode-v4","2b9f02c","XeraL2","x",
% 48,"f1b5d481","10.20.109.34",1444778562623260]
testbup() ->
    legacy_msgpack_handler:build_msg_from_unpacked(bsc,
						   blist(), basicmap(),
						   'LoginReq').

%helpers:testnup().
%Fields: [state,ftype]
%N: 1, K: "state"
%N: 1, K: "pri"
%N: 2, K: "mask"
%N: 3, K: "level"
%N: 4, K: "qualifiers"
%N: 2, K: "ftype"
%['LightingForceState',['LightingCtrl',3,1,0,"undefined"],1]
testnup() ->
    legacy_msgpack_handler:build_msg_from_unpacked(nst,
						   nestedL(), nestedmap(),
						   'LightingForceState').

testenum() ->
    legacy_msgpack_handler:convertifenum('LightingForceType',
					 'Volatile').

testtup() ->
    legacy_msgpack_handler:unpack_msg(packedM()).

lc_test() ->
    L1 = lightingctrl:new(),
    L2 = lightingctrl:set_pri(L1, 3),
    L3 = lightingctrl:set_mask(L2, 1),
    L4 = lightingctrl:set_level(L3, <<0>>),
    lightingctrl:set_qualifiers(L4, undefined).

%%{'LightingForceState',{'LightingCtrl',3,1,<<0>>,undefined},
 %%                     'Volatile'}

%%(legacy_bridge@127.0.0.1)4> 'Messages':encode_msg(Lfs).
%%<<10,7,8,3,16,1,26,1,0,16,2>>

lfs_test() ->
    L1 = lightingforcestate:new(),
    L2 = lightingforcestate:set_state(L1, lc_test()),
    lightingforcestate:set_ftype(L2, 'Volatile').

testdomqtt() ->
    DecodedMsg = {'LightingForceState',
		  {'LightingCtrl', 3, 1, <<0>>, undefined}, 1},
    NodeId = "N13341e5",
    legacy_ws_handler:do_mqtt({DecodedMsg, NodeId}).

testtimereq() ->
    Map = #{<<"name">> => <<"TimeReq">>,
	    <<"nodeid">> => <<"N01232ea5">>},
    P = msgpack:pack(Map, [{format, map}]),
    {ok, D} = emqttc:start_link([{client_id, <<"tester">>}, {host,"127.0.0.1"},{port, 3002}]),
    emqttc:publish(D,
		   <<<<"TopicA/Cisco/Emeryville/">>/binary,
		     <<"N01232ea5">>/binary,
		     <<"/node/req/TimeReq">>/binary>>,
		   P, [{qos, 1}]).

one_schedule() ->
    [{type, other}, {id, 5},
     {calendar, {{'*', '*', '*'}, {'*', 17, 0}}}, {pri, 7},
     {mask, 0}, {level, 3}, {qualifiers, 0}].

testlighting() ->
    lighting_handler:find_deltas("N013341e5",
				 one_schedule(), self()).

testons() ->
    Types = schedule_types(),
    lighting_handler:all_ons(Types).

testoffs() ->
    Types = schedule_types(),
    lighting_handler:all_offs(Types).

testdims() ->
    Types = schedule_types(),
    lighting_handler:all_dims(Types).

testbuildschedules() ->
    S = exactly_6_schedules(),
    lighting_handler:build_by_type(S).

testprepareschedules() ->
    S = schedule_map(),
    lighting_handler:prepare_schedules(S).

schedule_types() ->
    [{dims,
      [{'LightingScheduledEvent', 4,
	{'CalendarEvent', 0, 0, 5, undefined, undefined,
	 undefined, undefined},
	{'LightingCtrl', 5, 1, <<"d">>, 128}}]},
     {dims,
      [{'LightingScheduledEvent', 4,
	{'CalendarEvent', 0, 23, 7, undefined, undefined,
	 undefined, undefined},
	{'LightingCtrl', 5, 1, <<"2">>, 0}}]},
     {offs,
      [{'LightingScheduledEvent', 0,
	{'CalendarEvent', 0, 53, 7, undefined, undefined,
	 undefined, undefined},
	{'LightingCtrl', 7, 1, <<0>>, 0}},
       {'LightingScheduledEvent', 2,
	{'CalendarEvent', 0, 53, 7, undefined, undefined,
	 undefined, undefined},
	{'LightingCtrl', 7, 1, <<0>>, 0}}]},
     {dims,
      [{'LightingScheduledEvent', 4,
	{'CalendarEvent', 0, 43, 16, undefined, undefined,
	 undefined, undefined},
	{'LightingCtrl', 5, 1, <<"2">>, 128}}]},
     {ons,
      [{'LightingScheduledEvent', 1,
	{'CalendarEvent', 0, 13, 17, undefined, undefined,
	 undefined, undefined},
	{'LightingCtrl', 7, 1, <<"d">>, 128}},
       {'LightingScheduledEvent', 3,
	{'CalendarEvent', 0, 13, 17, undefined, undefined,
	 undefined, undefined},
	{'LightingCtrl', 7, 1, <<"d">>, 128}}]},
     {dims,
      [{'LightingScheduledEvent', 4,
	{'CalendarEvent', 0, 0, 23, undefined, undefined,
	 undefined, undefined},
	{'LightingCtrl', 5, 1, <<"2">>, 0}}]},
     {rest,
      #{<<"hr">> => 0, <<"id">> => 5, <<"level">> => 100,
	<<"mask">> => 1, <<"mday">> => 31, <<"min">> => 0,
	<<"mon">> => 12, <<"pri">> => 5,
	<<"qualifiers">> => 128, <<"sec">> => 0,
	<<"year">> => 2016}},
     {rest,
      #{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
	<<"mask">> => 1, <<"mday">> => 31, <<"min">> => 23,
	<<"mon">> => 12, <<"pri">> => 5, <<"qualifiers">> => 0,
	<<"sec">> => 0, <<"year">> => 2016}},
     {rest,
      #{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
	<<"mask">> => 1, <<"mday">> => 31, <<"min">> => 1,
	<<"mon">> => 12, <<"pri">> => 5,
	<<"qualifiers">> => 128, <<"sec">> => 0,
	<<"year">> => 2016}},
     {rest,
      #{<<"hr">> => 5, <<"id">> => 5, <<"level">> => 100,
	<<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
	<<"qualifiers">> => 128, <<"sec">> => 0,
	<<"wday">> => 7}},
     {rest,
      #{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
	<<"mask">> => 1, <<"min">> => 51, <<"pri">> => 5,
	<<"qualifiers">> => 0, <<"sec">> => 0,
	<<"wday">> => 7}},
     {rest,
      #{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
	<<"mask">> => 1, <<"min">> => 17, <<"pri">> => 5,
	<<"qualifiers">> => 128, <<"sec">> => 0,
	<<"wday">> => 7}},
     {rest,
      #{<<"hr">> => 23, <<"id">> => 4, <<"level">> => 0,
	<<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
	<<"qualifiers">> => 0, <<"sec">> => 0,
	<<"wday">> => 7}},
     {rest,
      #{<<"hr">> => 5, <<"id">> => 5, <<"level">> => 100,
	<<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
	<<"qualifiers">> => 128, <<"sec">> => 0,
	<<"wday">> => 1}},
     {rest,
      #{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
	<<"mask">> => 1, <<"min">> => 53, <<"pri">> => 5,
	<<"qualifiers">> => 0, <<"sec">> => 0,
	<<"wday">> => 1}},
     {rest,
      #{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
	<<"mask">> => 1, <<"min">> => 11, <<"pri">> => 5,
	<<"qualifiers">> => 128, <<"sec">> => 0,
	<<"wday">> => 1}},
     {rest,
      #{<<"hr">> => 23, <<"id">> => 4, <<"level">> => 0,
	<<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
	<<"qualifiers">> => 0, <<"sec">> => 0,
	<<"wday">> => 1}}].

testschedulerest() ->
    S = exactly_6_schedules(),
    {Rest, _Binaries} = lighting_handler:build_by_type(S),
    lighting_handler:schedule_rest("N01232ea5", Rest,
				   remaining_after_6()).

testmorethan6() ->
    S = remaining_after_6(),
    lighting_handler:protosize_when_more_than_6(S).

tosend() ->
    [<<194, 1, 0>>,
     <<178, 1, 22, 8, 1, 18, 6, 8, 0, 16, 13, 24, 17, 26, 10,
       8, 7, 16, 1, 26, 1, 100, 32, 128, 1>>,
     <<178, 1, 22, 8, 3, 18, 6, 8, 0, 16, 13, 24, 17, 26, 10,
       8, 7, 16, 1, 26, 1, 100, 32, 128, 1>>,
     <<178, 1, 21, 8, 0, 18, 6, 8, 0, 16, 53, 24, 7, 26, 9,
       8, 7, 16, 1, 26, 1, 0, 32, 0>>,
     <<178, 1, 21, 8, 2, 18, 6, 8, 0, 16, 53, 24, 7, 26, 9,
       8, 7, 16, 1, 26, 1, 0, 32, 0>>,
     <<178, 1, 22, 8, 4, 18, 6, 8, 0, 16, 0, 24, 5, 26, 10,
       8, 5, 16, 1, 26, 1, 100, 32, 128, 1>>,
     <<178, 1, 21, 8, 4, 18, 6, 8, 0, 16, 23, 24, 7, 26, 9,
       8, 5, 16, 1, 26, 1, 50, 32, 0>>,
     <<178, 1, 22, 8, 4, 18, 6, 8, 0, 16, 43, 24, 16, 26, 10,
       8, 5, 16, 1, 26, 1, 50, 32, 128, 1>>,
     <<178, 1, 21, 8, 4, 18, 6, 8, 0, 16, 0, 24, 23, 26, 9,
       8, 5, 16, 1, 26, 1, 50, 32, 0>>,
     <<170, 1, 0>>].

formatted_rest() ->
    [{{'LightingForceState',
       {'LightingCtrl', 5, 1, <<"d">>, 128}, 'Persistent'},
      {2016, 12, 31, undefined, 0, 0, 0}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<0>>, 0}, 'Persistent'},
      {2016, 12, 31, undefined, 7, 23, 0}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<"d">>, 128}, 'Persistent'},
      {2016, 12, 31, undefined, 17, 1, 0}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<"d">>, 128}, 'Persistent'},
      {undefined, undefined, undefined, 7, 5, 0, 0}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<0>>, 0}, 'Persistent'},
      {undefined, undefined, undefined, 7, 7, 51, 0}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<"d">>, 128}, 'Persistent'},
      {undefined, undefined, undefined, 7, 17, 17, 0}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<0>>, 0}, 'Persistent'},
      {undefined, undefined, undefined, 7, 23, 0, 0}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<"d">>, 128}, 'Persistent'},
      {undefined, undefined, undefined, 1, 5, 0, 0}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<0>>, 0}, 'Persistent'},
      {undefined, undefined, undefined, 1, 7, 53, 0}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<"d">>, 128}, 'Persistent'},
      {undefined, undefined, undefined, 1, 17, 11, 0}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<0>>, 0}, 'Persistent'},
      {undefined, undefined, undefined, 1, 23, 0, 0}}].

rest() ->
    [{rest,
      [#{<<"hr">> => 0, <<"id">> => 5, <<"level">> => 100,
	 <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 0,
	 <<"mon">> => 12, <<"pri">> => 5,
	 <<"qualifiers">> => 128, <<"sec">> => 0,
	 <<"year">> => 2016}]},
     {rest,
      [#{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
	 <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 23,
	 <<"mon">> => 12, <<"pri">> => 5, <<"qualifiers">> => 0,
	 <<"sec">> => 0, <<"year">> => 2016}]},
     {rest,
      [#{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
	 <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 1,
	 <<"mon">> => 12, <<"pri">> => 5,
	 <<"qualifiers">> => 128, <<"sec">> => 0,
	 <<"year">> => 2016}]},
     {rest,
      [#{<<"hr">> => 5, <<"id">> => 5, <<"level">> => 100,
	 <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
	 <<"qualifiers">> => 128, <<"sec">> => 0,
	 <<"wday">> => 7}]},
     {rest,
      [#{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
	 <<"mask">> => 1, <<"min">> => 51, <<"pri">> => 5,
	 <<"qualifiers">> => 0, <<"sec">> => 0,
	 <<"wday">> => 7}]},
     {rest,
      [#{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
	 <<"mask">> => 1, <<"min">> => 17, <<"pri">> => 5,
	 <<"qualifiers">> => 128, <<"sec">> => 0,
	 <<"wday">> => 7}]},
     {rest,
      [#{<<"hr">> => 23, <<"id">> => 4, <<"level">> => 0,
	 <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
	 <<"qualifiers">> => 0, <<"sec">> => 0,
	 <<"wday">> => 7}]},
     {rest,
      [#{<<"hr">> => 5, <<"id">> => 5, <<"level">> => 100,
	 <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
	 <<"qualifiers">> => 128, <<"sec">> => 0,
	 <<"wday">> => 1}]},
     {rest,
      [#{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
	 <<"mask">> => 1, <<"min">> => 53, <<"pri">> => 5,
	 <<"qualifiers">> => 0, <<"sec">> => 0,
	 <<"wday">> => 1}]},
     {rest,
      [#{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
	 <<"mask">> => 1, <<"min">> => 11, <<"pri">> => 5,
	 <<"qualifiers">> => 128, <<"sec">> => 0,
	 <<"wday">> => 1}]},
     {rest,
      [#{<<"hr">> => 23, <<"id">> => 4, <<"level">> => 0,
	 <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
	 <<"qualifiers">> => 0, <<"sec">> => 0,
	 <<"wday">> => 1}]}].

testschedulequartz(Date) ->
    %{23,43,0}
    [Pid] = gproc:lookup_pids({p, l, keepalive}),
    Trigger = {{'*', '*', '*'}, Date},
    Binary = <<162, 1, 14, 10, 10, 8, 5, 16, 1, 26, 1, 100,
	       32, 128, 1, 16, 1>>,
    Nodeid = "N01232ea5",
    quartz:start_link(),
    quartz:schedule(Trigger,
		    fun () ->
			    lighting_handler:send_light_force_command(Nodeid,
								      Binary,
								      Pid)
		    end).

testquartzdate(Date) ->
    [Pid] = gproc:lookup_pids({p, l, keepalive}),
    Trigger = {{'*', '*', '*'}, Date},
    Binary = <<162, 1, 14, 10, 10, 8, 5, 16, 1, 26, 1, 100,
	       32, 128, 1, 16, 1>>,
    Nodeid = "N01232ea5",
    quartz:start_link(),
    quartz:schedule(Trigger,
		    fun () ->
			    lighting_handler:send_light_force_command(Nodeid,
								      Binary,
								      Pid)
		    end).

testquartz(Date, Nodeid) ->
    %Date = {23,43,0},
    quartz:start_link(),
    [Pids] = gproc:lookup_pids({p, l, keepalive}),
    case Pids of
      [] ->
	  lager:info("No Nodes are connected, can't send LightForce"
		     "Command ~p~n",
		     []);
      _ ->
	  %Date = {23,43,0},
	  Trigger = {{'*', '*', '*'}, Date},
	  Binary = <<162, 1, 14, 10, 10, 8, 5, 16, 1, 26, 1, 100,
		     32, 128, 1, 16, 1>>,
	  lager:info("Scheduling to send LightForceCommand "
		     "binary ~p~n",
		     [Binary]),
	  [quartz:schedule(Trigger,
			   fun () ->
				   lighting_handler:send_light_force_command(Nodeid,
									     Binary,
									     Pid)
			   end)
	   || Pid <- Pids]
    end.

schedule_map() ->
    #{<<"nodeid">> => [<<"N01232ea5">>],
      <<"name">> => <<"LightingScheduledEvent">>,
      <<"schedules">> => more_than_8_schedules()}.

remaining_after_6() ->
    [#{<<"hr">> => 0, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 0,
       <<"mon">> => 12, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"year">> => 2016},
     #{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
       <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 23,
       <<"mon">> => 12, <<"pri">> => 5, <<"qualifiers">> => 0,
       <<"sec">> => 0, <<"year">> => 2016},
     #{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 1,
       <<"mon">> => 12, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"year">> => 2016},
     #{<<"hr">> => 5, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"wday">> => 7},
     #{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 51, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 7},
     #{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 17, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"wday">> => 7},
     #{<<"hr">> => 23, <<"id">> => 4, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 7},
     #{<<"hr">> => 5, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"wday">> => 1},
     #{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 53, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 1},
     #{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 11, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"wday">> => 1},
     #{<<"hr">> => 23, <<"id">> => 4, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0,
       <<"wday">> => 1}].

exactly_6_schedules() ->
    [#{<<"hr">> => 5, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0},
     #{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 50,
       <<"mask">> => 1, <<"min">> => 23, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0},
     #{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 53, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0},
     #{<<"hr">> => 16, <<"id">> => 5, <<"level">> => 50,
       <<"mask">> => 1, <<"min">> => 43, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0},
     #{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 13, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0},
     #{<<"hr">> => 23, <<"id">> => 4, <<"level">> => 50,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0}].

just_two() ->
    [#{<<"hr">> => 20, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"mday">> => 15, <<"min">> => 36,
       <<"mon">> => 2, <<"pri">> => 5, <<"qualifiers">> => 128,
       <<"sec">> => 0, <<"year">> => 2016},
     #{<<"hr">> => 20, <<"id">> => 3, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 34, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0},
     #{<<"hr">> => 23, <<"id">> => 12, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 7},
     #{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
       <<"mask">> => 1, <<"mday">> => 15, <<"min">> => 23,
       <<"mon">> => 2, <<"pri">> => 5, <<"qualifiers">> => 0,
       <<"sec">> => 0, <<"year">> => 2016}].

%Rest = [
%{'LightingForceState',{'LightingCtrl',5,1,<<"d">>,128},'Persistent'},{2016,12,31,undefined,0,0,0},
%{'LightingForceState',{'LightingCtrl',5,1,<<0>>,0},'Persistent'},{2016,12,31,undefined,7,23,0},
%{'LightingForceState',{'LightingCtrl',5,1,<<"d">>,128},'Persistent'},{2016,12,31,undefined,17,1,0},
%{'LightingForceState',{'LightingCtrl',5,1,<<"d">>,128},'Persistent'},{undefined,undefined,undefined,7,5,0,0},
%{'LightingForceState',{'LightingCtrl',5,1,<<0>>,0},'Persistent'},{undefined,undefined,undefined,7,7,51,0},
%{'LightingForceState',{'LightingCtrl',5,1,<<"d">>,128},'Persistent'},{undefined,undefined,undefined,7,17,17,0},
%{'LightingForceState',{'LightingCtrl',5,1,<<0>>,0},'Persistent'},{undefined,undefined,undefined,7,23,0,0},
%{'LightingForceState',{'LightingCtrl',5,1,<<"d">>,128},'Persistent'},{undefined,undefined,undefined,1,5,0,0},
%{'LightingForceState',{'LightingCtrl',5,1,<<0>>,0},'Persistent'},{undefined,undefined,undefined,1,7,53,0},
%{'LightingForceState',{'LightingCtrl',5,1,<<"d">>,128},'Persistent'},{undefined,undefined,undefined,1,17,11,0},
%{'LightingForceState',{'LightingCtrl',5,1,<<0>>,0},'Persistent'},{undefined,undefined,undefined,1,23,0,0}]

more_than_8_schedules() ->
    [#{<<"hr">> => 5, <<"id">> => 3, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0},
     #{<<"hr">> => 7, <<"id">> => 2, <<"level">> => 50,
       <<"mask">> => 1, <<"min">> => 23, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0},
     #{<<"hr">> => 7, <<"id">> => 1, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 31, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0},
     #{<<"hr">> => 16, <<"id">> => 4, <<"level">> => 50,
       <<"mask">> => 1, <<"min">> => 43, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0},
     #{<<"hr">> => 17, <<"id">> => 0, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 13, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0},
     #{<<"hr">> => 23, <<"id">> => 5, <<"level">> => 50,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0},
     #{<<"hr">> => 0, <<"id">> => 6, <<"level">> => 100,
       <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 0,
       <<"mon">> => 12, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"year">> => 2016},
     #{<<"hr">> => 7, <<"id">> => 7, <<"level">> => 0,
       <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 23,
       <<"mon">> => 12, <<"pri">> => 5, <<"qualifiers">> => 0,
       <<"sec">> => 0, <<"year">> => 2016},
     #{<<"hr">> => 17, <<"id">> => 8, <<"level">> => 100,
       <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 1,
       <<"mon">> => 12, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"year">> => 2016},
     #{<<"hr">> => 5, <<"id">> => 9, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"wday">> => 7},
     #{<<"hr">> => 7, <<"id">> => 10, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 51, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 7},
     #{<<"hr">> => 17, <<"id">> => 11, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 17, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"wday">> => 7},
     #{<<"hr">> => 23, <<"id">> => 12, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 7},
     #{<<"hr">> => 5, <<"id">> => 13, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"wday">> => 1},
     #{<<"hr">> => 7, <<"id">> => 14, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 53, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 1},
     #{<<"hr">> => 17, <<"id">> => 15, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 11, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"wday">> => 1},
     #{<<"hr">> => 23, <<"id">> => 16, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0,
       <<"wday">> => 1}].

schedulerest_expected() ->
    [{{'LightingForceState',
       {'LightingCtrl', 5, 1, <<0>>, 0}, 'Persistent'},
      {{'*', '*', '*'}, sun, {23, 0, 0}}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<"d">>, 128}, 'Persistent'},
      {{'*', '*', '*'}, sun, {17, 11, 0}}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<0>>, 0}, 'Persistent'},
      {{'*', '*', '*'}, sun, {7, 53, 0}}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<"d">>, 128}, 'Persistent'},
      {{'*', '*', '*'}, sun, {5, 0, 0}}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<0>>, 0}, 'Persistent'},
      {{'*', '*', '*'}, sat, {23, 0, 0}}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<"d">>, 128}, 'Persistent'},
      {{'*', '*', '*'}, sat, {17, 17, 0}}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<0>>, 0}, 'Persistent'},
      {{'*', '*', '*'}, sat, {7, 51, 0}}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<"d">>, 128}, 'Persistent'},
      {{'*', '*', '*'}, sat, {5, 0, 0}}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<"d">>, 128}, 'Persistent'},
      {{2016, 12, 31}, {17, 1, 0}}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<0>>, 0}, 'Persistent'},
      {{2016, 12, 31}, {7, 23, 0}}},
     {{'LightingForceState',
       {'LightingCtrl', 5, 1, <<"d">>, 128}, 'Persistent'},
      {{2016, 12, 31}, {0, 0, 0}}}].

sensorsample() ->
    Tm = os:system_time(micro_seconds),
    S = #{<<"name">> => <<"SensorSample">>,
	  <<"nodeid">> => <<"N01232ea5">>,
	  <<"sensor">> => <<"l">>, <<"time">> => Tm,
	  <<"units">> => <<"ml">>, <<"value">> => 0},
    msgpack:pack(S, [{format, map}]).

tailtester() ->
    B = remaining_after_6(),
    lighting_handler:protosize_when_more_than_six(B).

testdcczmq(N) ->
    {ok, D} = emqttc:start_link([{client_id,
				  <<"zmqtester">>}]),
    Topic =
	<<"TopicA/Cisco/Emeryville/N01232ea5/node/evt/Se"
	  "nsorSample">>,
    lists:foreach(fun (I) ->
			  M = sensorsample(),
			  emqttc:publish(D, Topic, M, [{qos, 1}]),
			  io:format("[~3..0B] ------~n", [I]),
			  timer:sleep(1500)
		  end,
		  lists:seq(1, N)).

testgproc() ->
    application:start(gproc),
    process_flag(trap_exit, true),
    spawn_link(fun () ->
		       R = gproc:reg({n, l, <<"N01232ea5">>}),
		       io:format("gproc registering pid = ~p~n", [R]),
		       W = gproc:where({n, l, <<"N01232ea5">>}),
		       io:format("gproc where pid = ~p~n", [W]),
		       P = gproc:unreg({n, l, <<"N01232ea5">>}),
		       io:format("after unreg gproc unreg pid = ~p~n", [P]),
		       W2 = gproc:lookup_local_name({n, l, <<"N01232ea5">>}),
		       io:format("not registered N01232ea5 where gproc "
				 "for another node id ~p~n",
				 [W2]),
		       W3 = gproc:where({n, l, <<"N013341e5">>}),
		       io:format("not registered  N013341e5 where gproc "
				 "for another node id ~p~n",
				 [W3]),
		       W4 = gproc:unreg({n, l, <<"N013341e5">>}),
		       io:format("not registered N013341e5 gproc for another "
				 "node id ~p~n",
				 [W4])
	       end).

test_mnesia_add_schedules(Nodeid) ->
    legacy_db_server:add_schedules(Nodeid, undefined,
				   current_schedules(), quartzed_schedules()).

test_mnesia_read_schedules(Nodeid) ->
    legacy_db_server:read_schedules(Nodeid).

test_erlcron() ->
    application:start(erlcron),
    B = tester:just_two(),
    Nodeids = [<<"N01232ea5">>],
    %TobeScheduled = lighting_handler:protosize_when_more_than_six(B),
    %lager:info("TobeScheduled ~p~n",[TobeScheduled]),
    erlcron_handler:schedule_rest(Nodeids, B).

test_pool_init(Pools) ->
    %{ok, Pools} = application:get_env(legacy_bridge, pools),
    [test_pool_init_1(V1) || V1 <- Pools].

test_pool_init_1({Name, SizeArgs, WorkerArgs}) ->
    lager:debug("*********************************************"
		"*******************Name is ~n  ~p ~n "
		"",
		[Name]),
    lager:debug("*********************************************"
		"*******************SizeArgs is ~n ~p "
		"~n ",
		[SizeArgs]),
    lager:debug("*********************************************"
		"*******************WorkerArgs is ~n "
		"~p ~n ",
		[WorkerArgs]),
    case Name of
      legacy_zmq_pool ->
	  ZmqCArgs = [{name, {local, Name}},
		      {worker_module, legacy_zmq_client}]
		       ++ SizeArgs,
	  poolboy:child_spec(Name, ZmqCArgs, WorkerArgs);
      legacy_zmq_listener_pool ->
	  ZmQLArgs = [{name, {local, Name}},
		      {worker_module, legacy_zmq_listener}]
		       ++ SizeArgs,
	  poolboy:child_spec(Name, ZmQLArgs, WorkerArgs);
      legacy_mqtt_agent_pool ->
	  AgentArgs = [{name, {local, Name}},
		       {worker_module, legacy_mqtt_agent}]
			++ SizeArgs,
	  poolboy:child_spec(Name, AgentArgs, WorkerArgs)
    end.

quartzed_schedules() ->
    [#{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 53, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0},
     #{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 50,
       <<"mask">> => 1, <<"min">> => 23, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0},
     #{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 11, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"wday">> => 1},
     #{<<"hr">> => 5, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"wday">> => 1},
     #{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 17, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"wday">> => 7},
     #{<<"hr">> => 5, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"wday">> => 7},
     #{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 1,
       <<"mon">> => 12, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"year">> => 2016},
     #{<<"hr">> => 0, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 0,
       <<"mon">> => 12, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0,
       <<"year">> => 2016},
     #{<<"hr">> => 17, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 13, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0},
     #{<<"hr">> => 16, <<"id">> => 5, <<"level">> => 50,
       <<"mask">> => 1, <<"min">> => 43, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0},
     #{<<"hr">> => 5, <<"id">> => 5, <<"level">> => 100,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 128, <<"sec">> => 0}].

current_schedules() ->
    [#{<<"hr">> => 23, <<"id">> => 4, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 1},
     #{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 53, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 1},
     #{<<"hr">> => 23, <<"id">> => 4, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 7},
     #{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
       <<"mask">> => 1, <<"min">> => 51, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0, <<"wday">> => 7},
     #{<<"hr">> => 7, <<"id">> => 4, <<"level">> => 0,
       <<"mask">> => 1, <<"mday">> => 31, <<"min">> => 23,
       <<"mon">> => 12, <<"pri">> => 5, <<"qualifiers">> => 0,
       <<"sec">> => 0, <<"year">> => 2016},
     #{<<"hr">> => 23, <<"id">> => 4, <<"level">> => 50,
       <<"mask">> => 1, <<"min">> => 0, <<"pri">> => 5,
       <<"qualifiers">> => 0, <<"sec">> => 0}].

exact_twentyfour_slots() ->
    [#{"hr" => 13, "id" => 0, "level" => 30, "mask" => 1,
       "min" => 17, "pri" => 0, "qualifiers" => 4, "sec" => 0},
     #{"hr" => 2, "id" => 1, "level" => 80, "mask" => 1,
       "min" => 58, "pri" => 0, "qualifiers" => 4, "sec" => 0},
     #{"hr" => 12, "id" => 2, "level" => 100, "mask" => 1,
       "min" => 0, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 13, "id" => 3, "level" => 0, "mask" => 1,
       "min" => 17, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 12, "id" => 4, "level" => 50, "mask" => 1,
       "min" => 47, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 6, "id" => 5, "level" => 50, "mask" => 1,
       "min" => 0, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 12, "id" => 6, "level" => 0, "mask" => 1,
       "min" => 47, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 2, "id" => 7, "level" => 50, "mask" => 1,
       "min" => 58, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 4},
     #{"hr" => 3, "id" => 8, "level" => 100, "mask" => 1,
       "min" => 28, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 4},
     #{"hr" => 2, "id" => 9, "level" => 50, "mask" => 1,
       "min" => 58, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 5},
     #{"hr" => 3, "id" => 10, "level" => 100, "mask" => 1,
       "min" => 28, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 5},
     #{"hr" => 2, "id" => 11, "level" => 50, "mask" => 1,
       "min" => 59, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 6},
     #{"hr" => 3, "id" => 12, "level" => 100, "mask" => 1,
       "min" => 29, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 6},
     #{"hr" => 2, "id" => 13, "level" => 50, "mask" => 1,
       "min" => 59, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 7},
     #{"hr" => 3, "id" => 14, "level" => 100, "mask" => 1,
       "min" => 29, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 7},
     #{"hr" => 0, "id" => 15, "level" => 0, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 128, "sec" => 0,
       "wday" => 7},
     #{"hr" => 12, "id" => 16, "level" => 0, "mask" => 1,
       "min" => 47, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 7},
     #{"hr" => 13, "id" => 17, "level" => 0, "mask" => 1,
       "min" => 17, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 7},
     #{"hr" => 12, "id" => 18, "level" => 0, "mask" => 1,
       "min" => 47, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 1},
     #{"hr" => 13, "id" => 19, "level" => 0, "mask" => 1,
       "min" => 17, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 1},
     #{"hr" => 3, "id" => 20, "level" => 100, "mask" => 1,
       "min" => 29, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 1},
     #{"hr" => 12, "id" => 21, "level" => 100, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 128, "sec" => 0,
       "wday" => 2},
     #{"hr" => 3, "id" => 22, "level" => 100, "mask" => 1,
       "min" => 27, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 2}].

less_twentyfour_slots() ->
    [#{"hr" => 13, "id" => 0, "level" => 30, "mask" => 1,
       "min" => 17, "pri" => 0, "qualifiers" => 4, "sec" => 0},
     #{"hr" => 2, "id" => 1, "level" => 80, "mask" => 1,
       "min" => 58, "pri" => 0, "qualifiers" => 4, "sec" => 0},
     #{"hr" => 12, "id" => 2, "level" => 100, "mask" => 1,
       "min" => 0, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 13, "id" => 3, "level" => 0, "mask" => 1,
       "min" => 17, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 12, "id" => 4, "level" => 50, "mask" => 1,
       "min" => 47, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 6, "id" => 5, "level" => 50, "mask" => 1,
       "min" => 0, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 12, "id" => 6, "level" => 0, "mask" => 1,
       "min" => 47, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 2, "id" => 7, "level" => 50, "mask" => 1,
       "min" => 58, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 4},
     #{"hr" => 3, "id" => 8, "level" => 100, "mask" => 1,
       "min" => 28, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 4},
     #{"hr" => 2, "id" => 9, "level" => 50, "mask" => 1,
       "min" => 58, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 5},
     #{"hr" => 3, "id" => 10, "level" => 100, "mask" => 1,
       "min" => 28, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 5},
     #{"hr" => 2, "id" => 11, "level" => 50, "mask" => 1,
       "min" => 59, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 6},
     #{"hr" => 3, "id" => 12, "level" => 100, "mask" => 1,
       "min" => 29, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 6},
     #{"hr" => 2, "id" => 13, "level" => 50, "mask" => 1,
       "min" => 59, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 7},
     #{"hr" => 3, "id" => 14, "level" => 100, "mask" => 1,
       "min" => 29, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 7},
     #{"hr" => 0, "id" => 15, "level" => 0, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 128, "sec" => 0,
       "wday" => 7},
     #{"hr" => 12, "id" => 16, "level" => 0, "mask" => 1,
       "min" => 47, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 7},
     #{"hr" => 13, "id" => 17, "level" => 0, "mask" => 1,
       "min" => 17, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 7},
     #{"hr" => 12, "id" => 18, "level" => 0, "mask" => 1,
       "min" => 47, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 1},
     #{"hr" => 3, "id" => 22, "level" => 100, "mask" => 1,
       "min" => 27, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 2}].

twentyseven_slots() ->
    #{"name" => "LightingScheduledEvent",
      "nodeid" => ["N012323aa"],
      "schedules" => twentyfour_slots()}.

test_twentyseven() ->
    lighting_handler:prepare_schedules(twentyseven_slots()).

twentyfour_slots() ->
    [#{"hr" => 13, "id" => 0, "level" => 30, "mask" => 1,
       "min" => 17, "pri" => 0, "qualifiers" => 4, "sec" => 0},
     #{"hr" => 2, "id" => 1, "level" => 80, "mask" => 1,
       "min" => 58, "pri" => 0, "qualifiers" => 4, "sec" => 0},
     #{"hr" => 12, "id" => 2, "level" => 100, "mask" => 1,
       "min" => 0, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 13, "id" => 3, "level" => 0, "mask" => 1,
       "min" => 17, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 12, "id" => 4, "level" => 50, "mask" => 1,
       "min" => 47, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 12, "id" => 28, "level" => 60, "mask" => 1,
       "min" => 47, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 12, "id" => 29, "level" => 10, "mask" => 1,
       "min" => 47, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 12, "id" => 30, "level" => 15, "mask" => 1,
       "min" => 47, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 6, "id" => 5, "level" => 50, "mask" => 1,
       "min" => 0, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 12, "id" => 6, "level" => 0, "mask" => 1,
       "min" => 47, "pri" => 3, "qualifiers" => 0, "sec" => 0},
     #{"hr" => 2, "id" => 7, "level" => 50, "mask" => 1,
       "min" => 58, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 4},
     #{"hr" => 3, "id" => 8, "level" => 100, "mask" => 1,
       "min" => 28, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 4},
     #{"hr" => 2, "id" => 9, "level" => 50, "mask" => 1,
       "min" => 58, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 5},
     #{"hr" => 3, "id" => 10, "level" => 100, "mask" => 1,
       "min" => 28, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 5},
     #{"hr" => 2, "id" => 11, "level" => 50, "mask" => 1,
       "min" => 59, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 6},
     #{"hr" => 3, "id" => 12, "level" => 100, "mask" => 1,
       "min" => 29, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 6},
     #{"hr" => 2, "id" => 13, "level" => 50, "mask" => 1,
       "min" => 59, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 7},
     #{"hr" => 3, "id" => 14, "level" => 100, "mask" => 1,
       "min" => 29, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 7},
     #{"hr" => 0, "id" => 15, "level" => 0, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 128, "sec" => 0,
       "wday" => 7},
     #{"hr" => 12, "id" => 16, "level" => 0, "mask" => 1,
       "min" => 47, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 7},
     #{"hr" => 13, "id" => 17, "level" => 0, "mask" => 1,
       "min" => 17, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 7},
     #{"hr" => 12, "id" => 18, "level" => 0, "mask" => 1,
       "min" => 47, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 1},
     #{"hr" => 13, "id" => 19, "level" => 0, "mask" => 1,
       "min" => 17, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 1},
     #{"hr" => 3, "id" => 20, "level" => 100, "mask" => 1,
       "min" => 29, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 1},
     #{"hr" => 12, "id" => 21, "level" => 100, "mask" => 1,
       "min" => 0, "pri" => 2, "qualifiers" => 128, "sec" => 0,
       "wday" => 2},
     #{"hr" => 3, "id" => 22, "level" => 100, "mask" => 1,
       "min" => 27, "pri" => 2, "qualifiers" => 0, "sec" => 0,
       "wday" => 2},
     #{"hr" => 2, "id" => 23, "level" => 50, "mask" => 1,
       "min" => 57, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 3},
     #{"hr" => 3, "id" => 24, "level" => 100, "mask" => 1,
       "min" => 27, "pri" => 3, "qualifiers" => 0, "sec" => 0,
       "wday" => 3},
     #{"hr" => 12, "id" => 25, "level" => 0, "mask" => 1,
       "mday" => 31, "min" => 47, "mon" => 12, "pri" => 1,
       "qualifiers" => 128, "sec" => 0, "year" => 2016},
     #{"hr" => 15, "id" => 26, "level" => 0, "mask" => 1,
       "mday" => 31, "min" => 22, "mon" => 12, "pri" => 1,
       "qualifiers" => 0, "sec" => 0, "year" => 2016},
     #{"hr" => 1, "id" => 27, "level" => 100, "mask" => 1,
       "mday" => 1, "min" => 1, "mon" => 1, "pri" => 1,
       "qualifiers" => 0, "sec" => 0, "year" => 2017}].

get_legacy_queue() ->
    Lb = 'legacy_bridge@127.0.0.1',
    Ps = rpc:call(Lb, erlang, processes, []),
    [get_legacy_queue_1(V1, Lb) || V1 <- Ps].

get_legacy_queue_1(Pid, Lb) ->
    P = rpc:call(Lb, erlang, process_info, [Pid]),
    case P of
      undefined -> noproc;
      _ ->
	  Def = proplists:is_defined(registered_name, P),
	  case Def of
	    true ->
		Proc = proplists:get_value(registered_name, P),
		Ql = proplists:get_value(message_queue_len, P),
		case Proc of
		  legacy_zmq_client ->
		      io:format("Legacy_zmq_client: Queue length: ~p~n",
				[Ql]);
		  undefined -> ok;
		  _ -> ok
		end;
	    _ -> ok
	  end
    end.

test_gpssample() ->
  G = <<21,166,187,155,87,25,0,0,0,0,0,0,0,0,37,86,0,0,0,41,96,0,0,0,0,0,0,0>>,
  'Messages':decode_msg(G,'GpsSample').

shortconfig() ->
#{
  <<"name">>=> <<"ConfigResp">>,
  <<"nodeid">>=> [
    <<"N0gina3">>
  ],
 <<"kvpairs">>=> 
  #{<<"configid">>=> <<"52f74b90-f070-11e5-abe1-c124f900b420">>,
    <<"sensor.ai.dint">>=> 30000,
    <<"sensor.p.pint">>=> 1000,
    <<"sensor.T.mode">>=> 1,
    <<"sensor.t.dint">>=> 30000,
    <<"sensor.v.mode">>=> 1,
    <<"sensor.v.pint">>=> 3600000,
    <<"networkYSSID">>=> <<"XeraL4">>,
    <<"sensor.rf.mode">>=> 1,
    <<"networkXPasskey">>=> <<"kentspeed">>,
    <<"sensor.T.dint">>=> 30000,
    <<"sensor.t.pint">>=> 3600000,
    <<"sensor.aw.mode">>=> 1,
    <<"sensor.pc.mode">>=> 1,
    <<"sensor.mw.pint">>=> 600000,
    <<"sensor.l.dint">>=> 30000,
    <<"configToken">>=> <<"7c98c54e">>,
    <<"sensor.mt.pint">>=> 3600000
  }
}.

testwifiupdate(Nodeid) ->
  Map = #{
  <<"name">>=> <<"ConfigResp">>,
  <<"configtype">> => <<"wifiupdate">>,
  <<"nodeid">>=> [
    Nodeid
  ],
  <<"kvpairs">>=> #{
    <<"network.x.ssid">> => <<"XeraL2">>
    ,<<"token">> => <<"60d504aa">>
    }
  },
  legacy_mqtt_agent ! {dcc, "config", Map}.

testconfigupdate() ->
    Map = shortconfig(),
    legacy_mqtt_agent ! {dcc, "config", Map}.

decodegps() ->
  G1 = {'GpsSample',undefined,1473809825,1605502372237897746,86081,4254019812335834089},
  legacy_msgpack_handler:create_map('GpsSample', G1, <<"N01230b84">>).

testdirectfirmware() ->
legacy_mqtt_agent ! {dcc,"firmware",#{<<"firmwareid">> => <<"v4-c6665dd">>,<<"name">> => <<"AssignFirmware">>,<<"nodeid">> => [<<"N0gina3">>]}}.

testfwinrep(Nodeids, Firmwareid) ->
  Nodes = case is_list(Nodeids) of true -> Nodeids; _-> [Nodeids] end,
  Map = #{<<"nodeid">> => Nodes, <<"name">> => <<"AssignFirmware">>, <<"firmwareid">> => Firmwareid},
  lager:info("Ret:~p~n",[Map]),
  legacy_mqtt_agent ! {dcc, "firmware", Map}.

lt_off(Nodeid) ->
  Lf = #{<<"name">> => <<"LightingForceState">>,<<"pri">> => 3, <<"mask">> => 1,<<"level">> => 0,
  <<"qualifiers">> => <<"undefined">>,<<"ftype">> => <<"Volatile">>, <<"nodeid">> => [Nodeid]},
  legacy_mqtt_agent ! {dcc, "lighting", Lf}.

testdeviceaction(Nodeid, Action) ->
  Map = #{<<"name">> => <<"DeviceActionReq">>, <<"nodeid">> => [Nodeid], <<"action">> => Action},
  legacy_mqtt_agent ! {dcc, "device", Map}.

