-module(helpers).
-author("Gina Hagg ghagg@sensity.com").
-compile(export_all).
-compile([{parse_transform, lager_transform}]).

extract_topic_prefix(Topic, What) ->
    Tpc = binary_to_list(Topic),
    Pos = string:rstr(Tpc, What),
    Pre = string:substr(Tpc,1,Pos-1),
    Tks = string:tokens(Pre,"/"),
    Nodeid = lists:last(Tks),
    lager:info("helpers:extracted prefix ~p~n",[Pre]),
    {Nodeid,Pre}.

microsecsToDate(Microseconds) ->
   BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
   Seconds       = BaseDate + (Microseconds div 1000000),
   { Date,_Time} = calendar:gregorian_seconds_to_datetime(Seconds),
   Date.

timestamp() ->
    {M, S, U} = erlang:timestamp(),
    M * 1000000000 + S * 1000 + U div 1000.

now_us({MegaSecs,Secs,MicroSecs}) ->
  (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

%% setnth(Index, List, NewElement) -> List.

setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

%% Can add following caluse if you want to be kind and allow invalid indexes.
%% I wouldn't!
%% setnth(_, [], New) -> New.

binarizeliststrings(L) ->
  lists:map(fun(X) -> 
    S1 = element(1,X), S2 = element(2,X),  
    S = case is_list(S2) of true -> list_to_binary(S2); _-> S2 end,
    {list_to_binary(S1), S}
   end, L).

is_not_control_code( C ) when C > 127 -> true;
is_not_control_code( C ) when C < 32; C =:= 127 -> false;
is_not_control_code( _C ) -> true.
 
is_not_control_code_nor_extended_character( C ) when C > 127 -> false;
is_not_control_code_nor_extended_character( C ) -> is_not_control_code( C ).

task(String) ->
    %io:fwrite( "String (~p characters): ~s~n", [erlang:length(String), String] ),
    %String_without_cc = lists:filter( fun is_not_control_code/1, String ),
    %io:fwrite( "String without control codes (~p characters): ~s~n", [erlang:length(String_without_cc), String_without_cc] ),
    lists:filter( fun is_not_control_code_nor_extended_character/1, String ).
    %lager:info( "String without control codes nor extended characters (~p characters): ~s~n", [erlang:length(String_without_cc_nor_ec), String_without_cc_nor_ec] ),

topic_tokens(Topic) when is_binary(Topic) ->
    TT = binary_to_list(Topic),
    string:tokens(TT,"/"); 

topic_tokens(Topic)  ->
    string:tokens(Topic,"/").  

nodeid_from_topic(Topic) ->
    Tokens = topic_tokens(Topic),
    lists:nth(4,Tokens).

even(X) when is_integer(X), X >= 0 -> {ok,(X band 1) == 0};
even(X) -> {illegal_param,X}.
odd(X) when is_integer(X), X >= 0 -> {ok,(X band 1) == 1};
odd(X) -> {illegal_param,X}.

%{{2015,12,14},{6,19,10}}
utc_microsecs() ->
  (calendar:datetime_to_gregorian_seconds(calendar:universal_time()) * 1000000) - (719528*24*3600*1000*1000).

group_by_type(Nodeids) ->
  group_nodes(Nodeids, [], []).

group_nodes([], Legacies, Others) -> {Legacies, Others};

group_nodes([Nodeid|Rest], Legacies, Others) -> 
  Match = binary:match(Nodeid,[<<"N01">>],[]),
  case Match of nomatch -> group_nodes(Rest, Legacies, [Nodeid|Others]); _ -> group_nodes(Rest, [Nodeid|Legacies], Others) end.

find_subscriber(MsgType) ->
  Subs = [{<<"LightingForceState">>, "lighting"}, {"LightingForceState", "lighting"},
  {<<"LightingSetAuto">>, "lighting"},{"LightingSetAuto", "lighting"},{<<"LightingClearSchedule">>, "lighting"}, {"LightingClearSchedule", "lighting"},
  {<<"LightingScheduledEvent">>, "schedules"}, {"LightingScheduledEvent", "schedules"},
  {<<"SoftwareUpdateReq">>, "firmware"}, {"SoftwareUpdateReq", "firmware"},{<<"AssignFirmware">>, "firmware"},{"AssignFirmware", "firmware"},
  {<<"SensorSampleReq">>, "sensor"}, {"SensorSampleReq", "sensor"},{"VideoUploadReq", "video"},
  {<<"DeviceActionReq">>, "device"}, {"DeviceActionReq", "device"},
  {<<"VideoUploadReq">>, "video"},{<<"ConfigResp">>, "config"},{"ConfigResp", "config"}],
  proplists:get_value(MsgType, Subs).

get_prefix(Nodeid) ->
  lager:info("prefix for Nodeid ~p~n",[Nodeid]),
  "TopicA/Cisco/Emerywille/".

%% Work from the back and keep a tail of things done.
flatten3(L) -> flatten3(L, []).

flatten3([H|T], Tail) ->
    flatten3(H, flatten3(T, Tail));
flatten3([], Tail) -> Tail;
flatten3(E, Tail) -> [E|Tail].

%% Lift nested lists to the front of the list.
flatten1([[H|T1]|T2]) -> flatten1([H,T1|T2]);
flatten1([[]|T]) -> flatten1(T);
flatten1([E|T]) -> [E|flatten1(T)];
flatten1([]) -> [].

%% Keep a list of things todo and put tails onto it.
flatten2(L) -> flatten2(L, []).

flatten2([H|T], Todo) ->
    flatten2(H, [T|Todo]);
flatten2([], [H|Todo]) -> flatten2(H, Todo);
flatten2([], []) -> [];
flatten2(E, Todo) -> [E|flatten2(Todo, [])].

%{10,2,26,157} -> 10.2.26.157
get_host_addr() ->  
  {ok,Ip} =  application:get_env(device_service, host),
  lager:debug("Ip ~p",[Ip]),
  Ip.

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).
    