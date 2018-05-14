-module(dcc_video_msg_transformer).
-author("Gina Hagg").
-compile(export_all).
-compile([{parse_transform, lager_transform}]).
-define(Org, "efe5bdb3-baac-5d8e-6cae57771c13").
-define(ParkingNames, [{"ndpark", "NonDemarcatedParking"},{"dpark", "DemarcatedParking"},{"linec", "LineCrossing"}, {"LineCrossing", "LineCrossing"},
{"NonDemarcatedParking", "NonDemarcatedParking"},{"DemarcatedParking", "DemarcatedParking"}, {"ParkingKeepOut", "ParkingKeepOut"},
{"ObjectEntered", "ObjectEntering"},{"ObjectExit","ObjectExiting"},{"pko", "ParkingKeepOut"},
{"objent", "ObjectEntering"},{"objlev", "ObjectLeaving"},{"inventory", "Inventory"},{"objdwl", "ObjectDwell"}]).

identifiying_params(Tkns, Payload) ->
  Nodeid = lists:nth(2, Tkns),
  EType = lists:nth(5, Tkns),
  Typ = lists:nth(length(Tkns), Tkns),
  Header = maps:get("h", Payload),
  Uuid = maps:get("uuid", Header),
  {Nodeid, EType, Typ, Uuid}.

transform_video_msg(Identifier, Payload,NewConfig, OldConfig) ->
    transform_msg(Identifier,Payload, NewConfig, OldConfig).

transform_msg({Nodeid, "cfg", Typ, Uuid}, Payload, NewConfig, OldConfig) ->
  M1 = maps:put("nodeid", Nodeid, Payload),
  M2 = maps:put("name", proplists:get_value(Typ, ?ParkingNames, "unknown") ++ "Config", M1),
  lager:debug("OLDCONFIG: ~p",[OldConfig]),
  lager:debug("NEWCONFIG: ~p",[NewConfig]),
  RemovedSpots = case NewConfig == OldConfig orelse OldConfig ==[] of true -> [];
    _->
    NewSpots = proplists:get_value(Uuid, NewConfig),
    NewwSpots = case NewSpots of undefined -> []; _-> NewSpots end,
    OldSpots = proplists:get_value(Uuid, OldConfig),
    OoldSpots = case OldSpots of undefined -> []; _-> OldSpots end,
    lager:debug("---------------OLDSPOTS:~p ANNNNND NEwSPOTS:~p",[OoldSpots,NewSpots]),
    case NewwSpots == OoldSpots of true -> []; _-> OoldSpots -- NewwSpots end
  end,
  M3 = maps:put("removed", RemovedSpots, M2),
  lager:debug("sending Map to DD ~p",[M3]),
  % spawn(fun() -> dcc_db:addConfig(Nodeid, Uuid, M3) end),
  M3;
transform_msg({Nodeid, "evt", "dpark"=Typ, Uuid}, Payload, NewConfig, _) ->
    M1 = maps:put("nodeid", Nodeid, Payload),
    M2 = maps:put("name", proplists:get_value(Typ, ?ParkingNames, "unknown") ++ "Event", M1),
    M3 = treat_occ(M2),
    M4 = add_spots(M3, Uuid, NewConfig),
    lager:debug("sending Map to DD ~p", [M4]),
    M4;
transform_msg({Nodeid, "evt", "ndpark"=Typ, UUID}, Payload, _NewConfig, _) ->
    LastObjects =
        case dcc_cache:lookup(UUID) of
            'undefined' -> [];
            Data -> Data
        end,
    CurrentObjects =maps:get("o", Payload, []),
    _ = dcc_cache:insert(UUID, CurrentObjects),

    Empty = calculate_empty(LastObjects, CurrentObjects),

    M1 = maps:put("nodeid", Nodeid, Payload),
    M2 = maps:put("name", proplists:get_value(Typ, ?ParkingNames, "unknown") ++ "Event", M1),
    M3 = maps:put("emptyspots", Empty, M2),
    lager:debug("sending Map to DD ~p", [M3]),
    M3;
transform_msg({Nodeid, "evt", Typ, _Uuid}, Payload, _NewConfig, _) ->
    M1 = maps:put("nodeid", Nodeid, Payload),
    M2 = maps:put("name", proplists:get_value(Typ, ?ParkingNames, "unknown") ++ "Event", M1),
    lager:debug("sending Map to DD ~p", [M2]),
    M2.

calculate_empty(Last, Current) ->
    CleanCurrent = cleanup_objects(Current),
    lists:foldl(
        fun(UUID, Acc) ->
            case lists:member(UUID, CleanCurrent) of
                'true' -> Acc;
                'false' ->
                    Object = find_object_by_id(UUID, Last),
                    [Object | Acc]
            end
        end
        ,[]
        ,cleanup_objects(Last)
    ).

find_object_by_id(_UUID, []) -> #{};
find_object_by_id(UUID, [Object|Objects]) ->
    case maps:get("uuid", Object, "") of
        UUID -> Object;
        _ -> find_object_by_id(UUID, Objects)
    end.

cleanup_objects(Objects) ->
    lists:map(
        fun(Obj) ->
            maps:get("uuid", Obj)
        end
        ,Objects
    ).


getSpotsFromRoi(Identifier,Payload, "cfg", "dpark") ->
  {_, _, _, Uuid} = Identifier,
  Roi = maps:get("roi", Payload),
  Spots = maps:get("spots", Roi),
  Uuids = lists:map(fun(S) ->
    maps:get("uuid", S)
  end, Spots),
  {Uuid, Uuids};

getSpotsFromRoi(_Identifier,_Payload, _Etype, _Typ) ->
  {}.

%%{"N02c00073","RoF{q]dV#^3EY$iGr{sF","G<n4-7=/<oXpx-AO(TE^",
%["HN%:}G4JX0tXDdh??9:U","m/vN-s0@OT8=r6m#>qHq","QTY&Qk%JNM>]bpJuY!Fs","FFQ?:ja:ZFH9WXH-a<Wb","VCiWHlEXU{0+bU/pw?R8","RM@2hr3k*-Ys.tkA.O(]","%]G)iMJHfyVMG)NQf>ZB","n$Ns]+aS$foSlQGtx<Gz","N*P7biUk)puA4n3JQ%SW","kFD?]3Nmxqp[!l!rqSC+"]}
add_spots(Map, Uuid, NewConfig) ->
  lager:debug("we are looking for Uuid: ~p",[Uuid]),
  Occ = maps:get("occ", Map),
  Uuids = maps:get("spotuuids", Occ),
  Ouuids = maps:get("objectuuids", Occ),
  S = proplists:get_value(Uuid, NewConfig),
  S1 = case S of undefined -> []; _-> S -- Uuids end,
  lager:debug("match uuids ~p, objectuuids; ~p, spots ~p ------ ~p ",[Uuids,Ouuids, S, S1]),
  %maps:put("numemptyspots", length(S1) - length(Uuids), Map),
  maps:put("emptyspots", S1, Map).


get_delta({_, "evt", "dpark", _} , CachedState, Nowss) ->
    lager:debug("CachedState: ~p, Now:~p",[CachedState, Nowss]),
    ThisSpots =
        case CachedState of
            {} -> [];
            undefined -> [];
            _ ->
                maps:keys(CachedState)
        end,
    OccnextSpots =
        case Nowss of
            {} -> [];
            undefined -> [];
            _ ->
                {_, Nows} = Nowss, maps:keys(Nows)
        end,
    lager:debug("nextspots:~p and Thisspots: ~p",[OccnextSpots, ThisSpots]),
    %Diff = ThisSpots -- OccnextSpots,
    Diff = OccnextSpots -- ThisSpots,
    lager:debug("Diff ~p",[Diff]),
    Diff;

get_delta(_, _Occ,_Occ2) -> [].

apply_spots({},[]) ->
    [];

apply_spots(Spots,[]) ->
    [Spots];

apply_spots({},ExistingConfig) ->
    ExistingConfig;

apply_spots(Spots, ExistingConfig) ->
    case lists:member(Spots, ExistingConfig) of true -> ExistingConfig; _-> [Spots | ExistingConfig] end.

figure_out_roi_spots(Identifier, UnpackedMsg, ExistingConfig) ->
    {_Nodeid, Etype, Typ, _Uuid} = Identifier,
    Spots = getSpotsFromRoi(Identifier, UnpackedMsg, Etype, Typ),
    lager:debug("we got spots for storage ~p with existing config: ~p",[Spots,ExistingConfig]),
    dcc_video_msg_transformer:apply_spots(Spots,ExistingConfig).

cache_last_event({_Nodeid,"evt", "dpark",Uuid}, UnpackedMsg) ->
    Occ = maps:get("occ", UnpackedMsg),
    {Uuid, Occ};

cache_last_event(_Identifier,_UnpackedMsg) ->
    {}.

getMap() ->
  #{"h" => #{"ch" => 0,
    "e" => true,
    "n" => "DemarcatedParking",
    "t" => 1472774411906091,
    "uuid" => "iIcFkC9hfck%Gr7k$DGn"},
  "o" => [#{"Object" => #{"c" => "car",
       "img" => #{"u" => [0.0,0.0],"v" => [0.0,0.0]},
       "iv" => [3.0,2.0],
       "uuid" => ">L-IU<u?bODECo*-Wgx%",
       "wh" => 3.0,
       "world" => #{"lat" => [0.0,0.0],"lon" => [0.0,0.0]},
       "wp" => 1.0,
       "wv" => [0.0,0.0]},
     "s" => 1472774411905831},
   #{"Object" => #{"c" => "car",
       "img" => #{"u" => [0.0,0.0],"v" => [0.0,0.0]},
       "iv" => [3.0,2.0],
       "uuid" => "Q(n@OV0Q9@DmLY<ihf4$",
       "wh" => 3.0,
       "world" => #{"lat" => [0.0,0.0],"lon" => [0.0,0.0]},
       "wp" => 1.0,
       "wv" => [0.0,0.0]},
     "s" => 1472774371904410},
   #{"Object" => #{"c" => "car",
       "img" => #{"u" => [0.0,0.0],"v" => [0.0,0.0]},
       "iv" => [3.0,2.0],
       "uuid" => "tz+z*%xKNOLLrVw*T=uf",
       "wh" => 3.0,
       "world" => #{"lat" => [0.0,0.0],"lon" => [0.0,0.0]},
       "wp" => 1.0,
       "wv" => [0.0,0.0]},
     "s" => 1472774391905015},
   #{"Object" => #{"c" => "car",
       "img" => #{"u" => [0.0,0.0],"v" => [0.0,0.0]},
       "iv" => [3.0,2.0],
       "uuid" => "VXGAv6#zShYiwZNUZSlB",
       "wh" => 3.0,
       "world" => #{"lat" => [0.0,0.0],"lon" => [0.0,0.0]},
       "wp" => 1.0,
       "wv" => [0.0,0.0]},
     "s" => 1472774351901128}],
  "occ" => #{"A3qTw-.mlGy+Z6mH3<V[" => ["Q(n@OV0Q9@DmLY<ihf4$"],
    "C+p&kURLv56Uz..4$qx8" => [">L-IU<u?bODECo*-Wgx%"],
    "Eqtg4/w$7KW{[DfnIPEw" => ["tz+z*%xKNOLLrVw*T=uf"],
    "kP8X>tTT6ZMs44(9r1?!" => ["VXGAv6#zShYiwZNUZSlB"]}}.
treat_occ(Map) ->
  Occs = maps:get("occ", Map),
  Keys = maps:keys(Occs),
  Values = maps:values(Occs),
  maps:update("occ",#{"spotuuids" => Keys, "objectuuids" => Values}, Map).

test_treat() ->
  treat_occ(getMap()).
