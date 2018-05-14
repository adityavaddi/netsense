-module(legacy_msgpack_handler).
-compile(export_all).
-author("Gina Hagg <ghagg@sensity.com").
-include("legacy.hrl").
-compile([{parse_transform, lager_transform}]).


%%{'LoginReq',"NS1_1",1,"unode-v3",undefined,undefined,undefined,undefined,undefined,undefined,undefined}
%% StartFrom: we want to start from element 2, since first one is the name of the envelope.
%The devices now sends protobuf messages that is something similar to this
%{msg_type: <message-type>, sensor: <sensor - e.g t/lt>, time: <time>, val: <value>, unit:<unit>}
%When it gets transformed into MQTT topic, it becomes like the follow.
%snsv1/Acme/GarageB/N020012ab/sub/sensor1/evt/temp
%And the payload becomes
%{time: <time>, value:<value>, unit:<unit>}
%Topic = [Prefix, "N01abc234", "/sub/", atom_to_list('SensorSample'), "/event/",element(2, lists:nth(2,M))].
%["snsv1/c/Acme/s/GarageB/nid/","N01abc234","/sub/",
 %"SensorSample","/event/","t"]
%10> list_to_binary(Topic).
%"Acme/GarageB/N01abc234/evt"

msgnames() ->[{'DeviceAlarm',bsc},{'LightingClearSchedule',nst},{'CalendarEvent',bsc},
 {'LightingCtrl',bsc},{'LightingScheduledEvent',nst},{'X509UpdateReq',bsc},
 {'LoginReq',bsc},{'VideoUploadReq',bsc},{'LoginResp',bsc},{'ErrorResp',bsc},
 {'AstronomicalEvent',nst},{'LightingAstronomicalEvent',bsc},
 {'LightingSetAuto',bsc},{'LightingForceState',nst},{'SensorSampleReq',bsc},
 {'TimeResp',bsc},{'TimeReq',bsc},{'VideoUploadResp',bsc},{'SensorSample',bsc},
 {'DeviceActionReq',bsc},{'SoftwareUpdateReq',bsc},{'ConfigResp',nst},{'ConfigRespDone',bsc},
 {'KVPair',bsc},{'ConfigResp',nst},{'UnusedConfigReq',bsc},{'Envelope',nst}].



enums() ->
    [{x509Update,'X509Update'},{ ecode,'EnvelopeMsgCode'},{ qualifier,'LightingCtrlQualifier'},
 {ftype,'LightingForceType'},{alarmType,'AlarmType'},{alarmSeverity,'AlarmSeverity'},
 {actionType,'ActionType'},{body,'AstronomicalBody'},{ atype,'AstronomicalEventType'},
 { zenith,'AstronomicalEventZenith'}].

is_nested(Flist) ->
    %Flist = 'Messages':find_msg_def(Mtype),
    L = lists:map(fun(X) -> Ftype = element(4,X),
        case is_tuple(Ftype) andalso element(1,Ftype) == msg of true -> nested; _-> basic end end, Flist),
    lists:member(nested,L).

%%when unpacked, this is what we get.
%%#{"ftype" => 1,"level" => 0,"mask" => 1,"name" => "LightingForceState","pri" => 3,"qualifiers" => "undefined"}
%%however erlang-msgpack doesn't give us what we put in. we put in a map, get a proplist
%%{ok,{[{"ftype",1},{"level",0},{"mask",1},{"name","LightingForceState"},{"pri",3},{"qualifiers","undefined"}]}}
%%'Messages':enum_value_by_symbol_LightingForceType('Volatile') -> 2.
%%'Messages':get_enum_names().
%%['X509Update','EnvelopeMsgCode','LightingCtrlQualifier',
%% 'LightingForceType','AlarmType','AlarmSeverity',
%% 'ActionType','AstronomicalBody','AstronomicalEventType',
%% 'AstronomicalEventZenith']
unpack_msg(Msgpacked) ->
    {ok,Unpacked} = msgpack:unpack(Msgpacked),
    Mtype = list_to_existing_atom(maps:get("name",Unpacked)),
    Flist = 'Messages':find_msg_def(Mtype),
    Isnested = proplists:get_value(Mtype,msgnames()),
    M = list_to_tuple(build_msg_from_unpacked(Isnested,Flist,Unpacked, Mtype)),
    lager:debug("unpack_msg: M: ~p~n",[M]),
   'Messages':encode_msg(M).

build_msg_from_unpacked(nst,Fieldlist, Upacked, Mtype) ->
    Fields = extract_field_names(Fieldlist),
    L = lists:map(fun(N) -> Fk = lists:nth(N,Fields),K = atom_to_list(Fk),
        case maps:is_key(K,Upacked) of
            true ->
                {ok, V} = maps:find(K,Upacked),
                VV = convertifundefined(V),
                convertifenum(Fk,VV);
            _-> {msg,Submsgname} = element(5,lists:nth(N,Fieldlist)),
                SubFldList = 'Messages':find_msg_def(Submsgname),
                Ks = ["name"],
                list_to_tuple(build_msg_from_unpacked(bsc,SubFldList,maps:without(Ks,Upacked),Submsgname))
        end
    end,
    lists:seq(1,length(Fields))),
    [Mtype | L ];

%%%%b-> no nested msgs
build_msg_from_unpacked(bsc,Fieldlist, Upacked, Mtype) ->
    Fields = extract_field_names(Fieldlist),
    L = lists:map(fun(N) -> Fk = lists:nth(N,Fields),K = atom_to_list(Fk),
        case maps:is_key(K,Upacked) of
            true ->
                {ok, V} = maps:find(K,Upacked),
                VV = convertifundefined(V),
                convertifenum(Fk,VV);
            _-> {ok, Nm} = maps:get("name",Upacked),
                binary_to_existing_atom(Nm,unicode)
        end
    end,
    lists:seq(1,length(Fields))),
    [Mtype | L ].

convertifundefined(Term) -> case Term == "undefined" of true -> undefined; _-> Term end.

convertifenum(K,Term) -> case proplists:is_defined(K,enums()) andalso not is_atom(Term) of true ->  binary_to_existing_atom(Term, unicode); _-> Term end.

get_nested_msgtype(Flist) ->
    L = lists:map(fun(X) -> Ftype = element(4,X),
        case is_tuple(Ftype) andalso element(1,Ftype) == msg of true -> nested; _-> basic end end, Flist),
    lists:member(nested,L).

msgpack_payload(MsgEnvlp, Nodeid) ->
    Msgtype = element(1, MsgEnvlp),
    lager:debug("got message ~p for Node:~p~n", [Msgtype, Nodeid]),
    MsgMap = filter_undefined(create_map(Msgtype, MsgEnvlp, Nodeid)),
    %NewMap = inject_site_org(Nodeid, Org),
    lager:debug("ready for msgpack: ~p", [MsgMap]),
    msgpack:pack(MsgMap).

filter_undefined(Map) ->
    maps:filter(
        fun(_Key, 'undefined') -> 'false';
           (_Key, "undefined") -> 'false';
           (_Key, <<"undefined">>) -> 'false';
           (_Key, _Val) -> 'true'
        end
        ,Map
    ).
create_map('LoginReq',Envlp, Nodeid) ->
    {'LoginReq', _Nid, Protov, Clitype, Swid, Wifi, Prf, Chan, ConfigT, Ip, Time, Bssid, Mac, Auth} = Envlp,
    #{
        <<"name">> => <<"LoginReq">>
        ,<<"nodeid">> => Nodeid
        ,<<"protocolVersion">> => helpers:binarize_if_string(Protov)
        ,<<"clientType">> => helpers:binarize_if_string(Clitype)
        ,<<"swVerId">> => helpers:binarize_if_string(Swid)
        ,<<"netName">> => helpers:binarize_if_string(Wifi)
        ,<<"profileName">> => helpers:binarize_if_string(Prf)
        ,<<"assocChannel">> => helpers:binarize_if_string(Chan)
        ,<<"configToken">> => helpers:binarize_if_string(ConfigT)
        ,<<"localIP">> => helpers:binarize_if_string(Ip)
        ,<<"time">> => helpers:binarize_if_string(Time)
        ,<<"bssid">> => helpers:binarize_if_string(Bssid)
        ,<<"mac">> => helpers:binarize_if_string(Mac)
        ,<<"auth">> => helpers:binarize_if_string(Auth)
    };
create_map('DeviceAlarm', Envlp, Nodeid) ->
    {'DeviceAlarm', AlrmType, Severity, Msg} = Envlp,
    AlarmCode = 'Messages':enum_value_by_symbol_AlarmType(AlrmType),
    lager:debug("deviceAlarm: Nodeid: ~p, Type:~p, Code:~p, Severity: ~p, Msg:~p", [Nodeid,AlrmType, AlarmCode, Severity, Msg]),
    #{
        <<"name">> => <<"DeviceAlarm">>
        ,<<"nodeid">> => Nodeid
        ,<<"alarmType">>=> helpers:binarize_if_string(AlrmType)
        ,<<"alarmSeverity">> => helpers:binarize_if_string(Severity)
        ,<<"msg">> => helpers:binarize_if_string(Msg)
    };
create_map('SensorSample', Envlp, Nodeid) ->
    {'SensorSample', Sensor, Time, Val, Units} = Envlp,
    create_sensor_sample_map(Sensor, Time, Val, Units, Nodeid);
create_map('TimeReq', Envlp, Nodeid) ->
    {'TimeReq'} = Envlp,
    #{
        <<"name">> => <<"TimeReq">>
        ,<<"nodeid">> => Nodeid
    };
create_map('GpsSample', Envlp, Nodeid) ->
    {'GpsSample', GpsVer, EpochSecs, LatLong, AltAndMisc, SnrAndMisc} = Envlp,
    <<Lat:32/signed-integer, Lon:32/signed-integer>> = <<LatLong:64/integer>>,
    <<Alt:20/signed-integer, Spare:5, RcvStatus:7>> = <<AltAndMisc:32/integer>>,
    <<SNR:20, PDOP:12, HDOP:12,VDOP:12,
    FixType:2,CtrlMode:1,NumSatellite:5>> = <<SnrAndMisc:64/integer>>,
    Snrmin = (SNR bsr 44) band 32#FFFFF,
    Snravg = (Snrmin div 100) rem 100,
    Snrmax = (Snrmin div 10000),
    Snrmin2 = Snrmin rem 100,
    #{
        <<"name">> => <<"GpsSample">>
        ,<<"nodeid">> => Nodeid
        ,<<"gpsver">> => case GpsVer of undefined -> <<"undefined">>; _-> GpsVer end
        ,<<"epochsecs">> => EpochSecs
        ,<<"lat">> => Lat * 0.0000001
        ,<<"lon">> => Lon * 0.0000001
        ,<<"altAndMisc">> => AltAndMisc
        ,<<"snrAndMisc">> => SnrAndMisc
        ,<<"snrmin">> => Snrmin2
        ,<<"snravg">> => Snravg
        ,<<"snrmax">> => Snrmax
        ,<<"altitude">> => Alt * 0.1
        ,<<"spare">> => Spare
        ,<<"rs">> => case RcvStatus of 65 -> "A"; _-> "V" end
        ,<<"snr">> => SNR
        ,<<"pdop">> => PDOP * 0.01
        ,<<"hdop">> => HDOP * 0.01
        ,<<"vdop">> => VDOP * 0.01
        ,<<"fixtype">> => FixType
        ,<<"ctrlmode">> => CtrlMode
        ,<<"numsatellite">> => NumSatellite
    };
create_map('GpsActionRsp', Envlp, Nodeid) ->
    {'GpsActionRsp', ActionType, IVal, SVal} = Envlp,
    #{
        <<"name">> => <<"GpsActionRsp">>
        ,<<"nodeid">> => Nodeid
        ,<<"actiontype">> => ActionType
        ,<<"ival">> => IVal
        ,<<"sval">> => SVal
    };
create_map(MsgType,Envlp, Nodeid) ->
    lager:debug("Msgtype:~p, Envlp: ~p",[MsgType, Envlp]),
    Fields = fetch_fields(MsgType),
    Elist = (
        lists:map(
            fun(X) ->
                case is_tuple(X) of
                    'true' -> tuple_to_list(X);
                    _ -> X
                end
            end
            ,tuple_to_list(Envlp)
        )
    ),
    L = [
        {<<"name">>, atom_to_binary(MsgType, 'unicode')}
        ,{<<"nodeid">>, Nodeid}
        | create_kv_tuple_list(Fields, Elist)
    ],
    maps:from_list(
        helpers:binarizeliststrings(
            helpers:flatten3(L)
        )
    ).

create_sensor_sample_map("rf"=Sensor, Time, Val, Units, Nodeid) ->
    lager:debug("creating maps for sensor: ~p with value: ~p", [Sensor, Val]),
    <<_:32, SN:16/signed-integer, RF:16/signed-integer>> = <<Val:64/integer>>,
    #{
        <<"name">> => <<"SensorSample">>
        ,<<"nodeid">> => Nodeid
        ,<<"sensor">>=> helpers:binarize_if_string(Sensor)
        ,<<"time">> => helpers:binarize_if_string(Time)
        ,<<"value">> => helpers:binarize_if_string(Val)
        ,<<"values">> => [RF, SN]
        ,<<"units">> => helpers:binarize_if_string(Units)
    };
create_sensor_sample_map("jt"=Sensor, Time, Val, Units, Nodeid) ->
    lager:debug("creating maps for sensor: ~p with value: ~p", [Sensor, Val]),
    <<M:16/signed-integer, Z:16/signed-integer, Y:16/signed-integer, X:16/signed-integer>> = <<Val:64/integer>>,
    #{
        <<"name">> => <<"SensorSample">>
        ,<<"nodeid">> => Nodeid
        ,<<"sensor">>=> helpers:binarize_if_string(Sensor)
        ,<<"time">> => helpers:binarize_if_string(Time)
        ,<<"value">> => helpers:binarize_if_string(Val)
        ,<<"values">> => [X, Y, Z, M]
        ,<<"units">> => helpers:binarize_if_string(Units)
    };
create_sensor_sample_map("bR"=Sensor, Time, Val, Units, Nodeid) ->
    lager:debug("creating maps for sensor: ~p with value: ~p", [Sensor, Val]),
    <<_:48, BR2:8, BR1:8>> = <<Val:64/integer>>,
    #{
        <<"name">> => <<"SensorSample">>
        ,<<"nodeid">> => Nodeid
        ,<<"sensor">>=> helpers:binarize_if_string(Sensor)
        ,<<"time">> => helpers:binarize_if_string(Time)
        ,<<"value">> => helpers:binarize_if_string(Val)
        ,<<"values">> => [BR1, BR2]
        ,<<"units">> => helpers:binarize_if_string(Units)
    };
create_sensor_sample_map(Sensor, Time, Val, Units, Nodeid) ->
    SignedInt64Sensors = ["v", "vp", "i", "mi", "mip", "ai", "aip", "w", "mw",
                        "mP", "aP", "mPF", "aPF"],
    Value =
        case lists:member(Sensor, SignedInt64Sensors) of
            'false' -> Val;
            'true' ->
                <<V:64/signed-integer>> = <<Val:64/integer>>,
                V
        end,
    lager:debug("creating maps for sensor: ~p with value: ~p", [Sensor, Val]),
    #{
        <<"name">> => <<"SensorSample">>
        ,<<"nodeid">> => Nodeid
        ,<<"sensor">>=> helpers:binarize_if_string(Sensor)
        ,<<"time">> => helpers:binarize_if_string(Time)
        ,<<"value">> => helpers:binarize_if_string(Value)
        ,<<"units">> => helpers:binarize_if_string(Units)
    }.

%%Fields for nested LightingCtrl would look like this.
%%[{field,state,1,2,{msg,'LightingCtrl'},required,[]},{field,ftype,2,3,{enum,'LightingForceType'},optional,[]}]
%%Fields = [state,ftype].
%[[#{pri => 3},#{mask => 1},#{level => 0},#{qualifiers => undefined}],#{ftype => 1}]
create_kv_tuple_list(_Fields, []) ->
    [];

create_kv_tuple_list(Fields, Elist) ->
    %lager:debug("Elist: ~p, Fields: ~pn",[Fields,Elist]),
    [MsgType| _R] = Elist,
    lager:debug("Fields: ~p, Elist: ~p, MsgType:~p~n",[Fields,Elist,MsgType]),
    Isnested = proplists:get_value(MsgType,msgnames()),
    Fieldlen = length(Fields),
    case Isnested of true ->
        lists:map(fun(X) ->
            K = atom_to_list(lists:nth(X,Fields)), V = lists:nth(X+1,Elist),
            lager:debug("K: ~p, V: ~p~n",[K, V]),
            K, V ,map_nested(atom_to_list(K),V) end, lists:seq(1,Fieldlen));
        _->
            lager:debug("not nested create_kv_tuple_list"),
            lists:map(fun(Y) ->
                K1 = atom_to_list(lists:nth(Y,Fields)), V1 = lists:nth(Y+1,Elist),
                lager:debug("K: ~p, V: ~p~n",[K1, V1]),
                {K1, V1} end, lists:seq(1,Fieldlen))
    end.

%%We can have nested messages, like below. However, it is no more than one level deep. We flatten them.
%%{'LightingForceState',{'LightingCtrl',3, 1,0:8,undefined},1}
%%erlang msgpack doesn't like packing atoms other than true | false. so convert everything to string.
map_nested(K,V) ->
    case is_list(V) andalso not io_lib:printable_list(V) of
        true ->
            [Subtype|_R] = V,
            NestedMsgFields = fetch_fields(Subtype),
            create_kv_tuple_list(NestedMsgFields,V);
        _-> V1 = case is_atom(V) of true -> atom_to_list(V); _-> V end,
            {K,V1}
    end.

%%when they aren't nested.
create_keyvalue_tuples_for_node(Envlp, MsgType) ->
    Fields = fetch_fields(MsgType),
    %[{X, Y} || X <- [1,2,3], Y <- [a,b]].
    EList = erlang:tuple_to_list(Envlp),
    L = lists:map(fun(X) -> {lists:nth(X,Fields), lists:nth(X+1,EList)} end, lists:seq(1,length(Fields))),
    lager:debug("L: ~p~n",[L]),
    [{msg_type, MsgType}|L].


extract_field_names(Fields)->
    extract_field_names(Fields,[]).

extract_field_names([H|T], Names) ->
    extract_field_names(T, [element(2,H)|Names]);

extract_field_names([], Names) -> lists:reverse(Names).


%%what we get from dcc: note state is missing(LightingCtrl)
%% since this is flattened, we need to add back the missing nested message names to fit into proto definition,
%[{"nodeId","NS1_1"},{"name","LightingForceState"},{"pri",3},{"mask",1},{"level",0},{"qualifiers","undefined"},{"ftype",1}]
%%it needs to look like this: {'LightingForceState',{'LightingCtrl',3, 1,0,undefined},1}
%%%%% msg def
%[{field,state,1,2,{msg,'LightingCtrl'},required,[]},
% {field,ftype,2,3,{enum,'LightingForceType'},optional,[]}]
%%LightingScheduledEvent example: what we get
%%[{"name","LightingScheduledEvent"},{"id",1},{"sec",0},{"min",1},{"hr",13},{"pri",0},
%%{"mask",3},{"level",0}, {"ftype","undefined"}]
%%#{"name" => "LoginResp","ok" => true,"time" => 1448066980724961}
%%what we want:
%%{'LightingScheduledEvent',1,{'CalendarEvent',0,1,13,undefined,undefined,undefined, undefined},{'LightingCtrl',1,3,0,undefined}}
build_proto_msg_from_msgpack(Respmapp) ->
  Respmap1 = case is_list(Respmapp) of true -> [Rmp] = Respmapp, Rmp; _ -> Respmapp end,
  Nm = maps:get("name", Respmap1),
  Msgname = case is_binary(Nm) of true -> binary_to_existing_atom(Nm,unicode); _-> list_to_existing_atom(Nm) end,
  Respmap = maps:to_list(Respmap1),
  match_map_to_fields(Msgname, Respmap).

%#{<<"ftype">> => <<"Volatile">>,<<"level">> => 60,<<"mask">> => 1,<<"name">> => <<"LightingForceState">>,
%<<"nodeid">> => [<<"N0gina3">>],<<"pri">> => 3,<<"qualifiers">> => <<"undefined">>}
match_map_to_fields('LightingForceState', Respmapp) ->
    Respmap = maps:from_list(Respmapp),
    lager:debug("msgname: LightingForceState, Respmap: ~p~n"),
    Qlfr1 = maps:get("qualifiers", Respmap),
    Qlfr = case is_binary(Qlfr1) of true -> binary_to_existing_atom(Qlfr1,unicode); _-> list_to_existing_atom(Qlfr1) end,
    Mask = maps:get("mask", Respmap),
    Level = maps:get("level", Respmap),
    Pri = maps:get("pri", Respmap),
    Ftype1 = maps:get("ftype", Respmap),
    Ftype = case is_binary(Ftype1) of true -> binary_to_atom(Ftype1,unicode); _-> list_to_atom(Ftype1) end,
    {'LightingForceState',{'LightingCtrl', Pri, Mask, <<Level>>, Qlfr}, Ftype};

match_map_to_fields('DeviceActionReq', Respmapp) ->
    Respmap = maps:from_list(Respmapp),
    lager:debug("msgname: DeviceActionReq, Respmap: ~p~n"),
    ActionType1 = maps:get("action", Respmap),
    ActionType = case is_binary(ActionType1) of true -> binary_to_atom(ActionType1,unicode); _-> list_to_atom(ActionType1) end,
    {'DeviceActionReq', ActionType};

match_map_to_fields('SensorSampleReq', Respmapp) ->
    Respmap = maps:from_list(Respmapp),
    lager:debug("msgname: SensorSampleReq, Respmap: ~p~n"),
    Sensor = maps:get("sensor", Respmap),
    {'SensorSampleReq', helpers:binarize_if_string(Sensor)};

match_map_to_fields(Msgname, Respmap) ->
    Flist = fetch_fielddefs(Msgname),
    lager:debug("msgname: ~p, Respmap: ~p~n"),
    M = lists:map(fun(FieldDef) ->
    Field = element(2,FieldDef),
    TypeTuple = element(5,FieldDef),
    Type = case is_tuple(TypeTuple) of true -> element(1,TypeTuple); _-> TypeTuple end,
    Val = proplists:get_value(atom_to_binary(Field, unicode), Respmap),
    case Val of
        undefined ->  F = element(5,FieldDef),
             extract_sub_values(F, Respmap);
        _ -> unbinarize(Val,Type)
    end
  end, Flist),
    M1 = [Msgname| M],
    list_to_tuple(M1).



  extract_sub_values({msg,Submsgname}, Respmap) ->
    SubMsgFList = fetch_fielddefs(Submsgname),
    Values = lists:map(fun(FieldDef) ->
        Field = element(2,FieldDef),
        TypeTuple = element(5,FieldDef),
        Type = case is_tuple(TypeTuple) of true -> element(1,TypeTuple); _-> TypeTuple end,
        lager:debug("Ftype: ~p, Field:~p ~n",[Type, Field]),
        unbinarize(proplists:get_value(atom_to_binary(Field, unicode),Respmap), Type)
    end, SubMsgFList),

    Values2 = [Submsgname| Values],
    list_to_tuple(Values2);

 extract_sub_values(_Other, _Respmap) ->
    lager:debug("build_proto_from_msgpack:must be enum field, nothing to do.").

 binarize_if_string(Val) when is_list(Val) ->
    list_to_binary(Val);

 binarize_if_string(Val) ->
    Val.

unbinarize(<<"undefined">>, _Ftype) ->
    undefined;

unbinarize(Val, Ftype) when is_binary(Val) ->
    lager:debug("binary val: ~p~n",[Val]),
    Sval = binary_to_list(Val),
    case io_lib:printable_list(Sval) of
        true ->
        case Ftype of
            enum -> list_to_atom(Sval);
            _-> Sval
        end;
        _ -> Sval
    end;

unbinarize(Val, bytes) ->
    <<Val>>;

unbinarize(Val, _Ftype) ->
    Val.


fetch_fielddefs(Msgname) when is_atom(Msgname) ->
    lager:debug("msg fielddefs for atom msgname: ~p~n",[Msgname]),
    F = proplists:get_value(Msgname, msgfielddefs()),
    lager:debug("fetchfielddefs-atom msgname:fielddefs is ~p~n",[F]),
    F;

%[{'LightingForceState',
%[{fielddefs,[{field,state,1,2,{msg,'LightingCtrl'},required,[]},{field,ftype,2,3,{enum,'LightingForceType'},optional,[]}]},{fields,[state,ftype]}]}]
%
fetch_fielddefs(Msgname) ->
    lager:debug("msg fielddefs for msgname: ~p~n",[Msgname]),
    F = proplists:get_value(list_to_atom(Msgname), msgfielddefs()),
    lager:debug("fetchfieldefs-string msgname:fielddefs is ~p~n",[F]),
    F.

 fetch_fields(Msgname) when is_atom(Msgname) ->
    lager:debug("msg fields for msgname is atom: ~p~n",[Msgname]),
    F = proplists:get_value(Msgname, msgfields()),
    lager:debug("fetchfields-string atom:fields is ~p~n",[F]),
    F;

 fetch_fields(Msgname) ->
    lager:debug("msg fields for msgname: ~p~n",[Msgname]),
    F = proplists:get_value(list_to_atom(Msgname), msgfields()),
    lager:debug("msg fields: ~p~n",[F]),
    F.

msgfielddefs() ->[
 {'DeviceAlarm',[{field,alarmType,1,2,{enum,'AlarmType'},required,[]},
 {field,alarmSeverity,2,3,{enum,'AlarmSeverity'},required,[]},
 {field,msg,3,4,string,optional,[]}]},

 {'LightingClearSchedule', []},

 {'CalendarEvent', [{field,sec,1,2,uint32,required,[]},
 {field,min,2,3,uint32,required,[]},
 {field,hr,3,4,uint32,required,[]},
 {field,wday,4,5,uint32,optional,[]},
 {field,mday,5,6,uint32,optional,[]},
 {field,mon,6,7,uint32,optional,[]},
 {field,year,7,8,uint32,optional,[]}]},

 {'LightingCtrl', [{field,pri,1,2,uint32,required,[]},
 {field,mask,2,3,uint32,required,[]},
 {field,level,3,4,bytes,required,[]},
 {field,qualifiers,4,5,uint32,optional,[]}]},

 {'LightingScheduledEvent', [{field,id,1,2,uint32,required,[]},
 {field,event,2,3,{msg,'CalendarEvent'},required,[]},
 {field,state,3,4,{msg,'LightingCtrl'},required,[]}]},

 {'LoginReq', [{field,nodeId,1,2,string,required,[]},
 {field,protocolVersion,2,3,uint64,required,[]},
 {field,clientType,3,4,string,optional,[]},
 {field,swVerId,4,5,string,optional,[]},
 {field,netName,5,6,string,optional,[]},
 {field,profileName,6,7,string,optional,[]},
 {field,assocChannel,7,8,uint32,optional,[]},
 {field,configToken,8,9,string,optional,[]},
 {field,localIP,9,10,string,optional,[]},
 {field,time,10,11,uint64,optional,[]}]},

 {'LoginResp',[{field,okay,1,2,bool,required,[]},
 {field,time,2,3,uint64,optional,[]}]},
 {'ErrorResp',[{field,message,1,2,string,required,[]}]},

 {'AstronomicalEvent',[{field,body,1,2,{enum,'AstronomicalBody'},required,[]},
 {field,atype,2,3,{enum,'AstronomicalEventType'},required,[]},
 {field,zenith,3,4,
        {enum,'AstronomicalEventZenith'},
        required,[]},
 {field,wday,4,5,uint32,optional,[]},
 {field,mday,5,6,uint32,optional,[]},
 {field,mon,6,7,uint32,optional,[]},
 {field,year,7,8,uint32,optional,[]}]},

 {'LightingAstronomicalEvent',[{field,id,1,2,uint32,required,[]},
 {field,event,2,3,{msg,'AstronomicalEvent'},required,[]},
 {field,state,3,4,{msg,'LightingCtrl'},required,[]}]},

 {'LightingSetAuto',[]},

 {'LightingForceState', [{field,state,1,2,{msg,'LightingCtrl'},required,[]},
 {field,ftype,2,3,{enum,'LightingForceType'},optional,[]}]},

 {'SensorSampleReq', [{field,sensor,1,2,string,required,[]}]},

 {'SensorSample', [{field,sensor,1,2,string,required,[]},
 {field,time,2,3,uint64,required,[]},
 {field,value,3,4,uint64,required,[]},
 {field,units,4,5,string,optional,[]}]},

 {'DeviceActionReq',[{field,actionType,1,2,{enum,'ActionType'},required,[]}]},
 {'KVPair',[{field,key,1,2,string,required,[]},
 {field,sValue,2,3,string,optional,[]},
 {field,lValue,3,4,int64,optional,[]},
 {field,bValue,4,5,bool,optional,[]}]},
 {'SoftwareUpdateReq', [{field,swUpdateURL,1,2,string,required,[]}]},
 {'TimeReq',[]},
 {'TimeResp',[{field,time,1,2,uint64,required,[]}]},
 {'LoginResp', [{field,okay,1,2,bool,required,[]},{field,time,2,3,uint64,optional,[]}]},
 {'ConfigResp',[{field,pair,1,2,{msg,'KVPair'},required,[]}]},
 {'ConfigRespDone',[]},
 {'UnusedConfigReq',[]}
 ].

 msgfields() ->
 [{'DeviceAlarm',[alarmType,alarmSeverity,msg]},
 {'LightingClearSchedule', []},
 {'CalendarEvent', [sec,min,hr,wday,mday,mon,year]},
 {'LightingCtrl', [pri,mask,level,qualifiers]},
 {'LightingScheduledEvent', [id,event,state]},
 {'LoginReq', [nodeId,protocolVersion,clientType,swVerId,netName,
 profileName,assocChannel,configToken,localIP,time]},
 {'LoginResp',[okay,time]},
 {'ErrorResp',[message]},
 {'AstronomicalEvent',[body,atype,zenith,wday,mday,mon,year]},
 {'LightingAstronomicalEvent',[id,event,state]},
 {'LightingSetAuto',[]},
 {'LightingForceState', [state,ftype]},
 {'SensorSampleReq', [sensor]},
 {'SensorSample', [sensor,time,value,units]},
 {'DeviceActionReq',[actionType]},
 {'SoftwareUpdateReq', [swUpdateURL]},
 {'TimeReq',[]},
 {'TimeResp',[time]},
 {'LoginResp',[ok,time]},
 {'KVPair',[key,sValue,lValue,bValue]},
 {'ConfigResp',[msg]},
 {'ConfigRespDone',[]},
 {'UnusedConfigReq',[]}
 ].


list_if_atom(Val) when is_atom(Val) ->
    atom_to_list(Val);

list_if_atom(Val) ->
    Val.

inject_site_org(Nodeid, Org, Site, MsgMap) ->
    NodeInfo = case Org of [] ->
        mnesia:ets(fun() -> mnesia:read({nodes, Nodeid}) end);
        _->
            [#nodes{org=Org, site=Site}]
    end,
    lager:debug("Nodeinfo from cache/or : ~p~n",[NodeInfo]),
    case NodeInfo of [] -> MsgMap;
        _-> [#nodes{org=Orgid, site=Siteid}] = NodeInfo,
        M1 = maps:put("orgid",Orgid,MsgMap),
        maps:put("siteid", Siteid, M1)
    end.
