%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Legacy command handler ==
%% Handle commands received from datadealer via rabbitmq.
%% @end
%%%-------------------------------------------------------------------
-module(legacy_cmd_handler).

-export([
    handle/1
]).

-include("legacy.hrl").
-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%--------------------------------------------------------------------
%% @param Msg encoded message (msgpack)
%% @doc
%% Handle commands received from datadealer
%%
%% Take an encoded message, either send decoded message to other process
%% or create command to send to corresponding websocket Pid.
%% @end
%%--------------------------------------------------------------------
-spec handle(binary()) -> 'ok'.
handle(Msg) ->
    legacy_metrics:cmd_rate_incoming(1),
    legacy_metrics:cmd_timer(
        fun() ->
            {'ok', Map} = msgpack:unpack(Msg),
            MsgType = maps:get("name", Map),
            NodeIDs = maps:get("nodeid", Map, []),
            lager:info("rcvd ~p command for nodes: ~p with payload: ~p", [MsgType, NodeIDs, Map]),
            handle(MsgType, Map)
        end
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(string(), map()) -> 'ok'.
handle("AssignFirmware", Map) ->
    erlang:spawn('legacy_ota_sup', 'handle', [Map]),
    lager:info("transfering work to legacy_ota_sup", []);
handle("OTAStop", Map) ->
    erlang:spawn('legacy_ota_scheduler', 'stop', [Map]),
    lager:info("transfering work to legacy_ota_scheduler", []);
handle("OTAFaster", Map) ->
    erlang:spawn('legacy_ota_scheduler', 'faster', [Map]),
    lager:info("transfering work to legacy_ota_scheduler", []);
handle("OTASlower", Map) ->
    erlang:spawn('legacy_ota_scheduler', 'slower', [Map]),
    lager:info("transfering work to legacy_ota_scheduler", []);
handle(MsgType, Map) ->
    NodeIDs =
        case maps:get("nodeid", Map, []) of
            ['null'] -> [<<>>];
            [[_|_]|_]=L -> L;
            X -> [X]
        end,
    NodeIDPidProps = legacy_helpers:filter_offline_nodes(NodeIDs),
    ConnectedNode = [N || {N, _} <- NodeIDPidProps],

    lager:debug("for command ~p, found node(s) ~p are connected", [MsgType, ConnectedNode]),
    case [N || N <- NodeIDs, not(lists:member(N, ConnectedNode))] of
        [] -> 'ok';
        DisconnectedNode ->
            lager:warning("for command ~p, found node(s) ~p are disconnected", [MsgType, DisconnectedNode])
    end,

    case ConnectedNode of
        [] -> 'ok';
        _ ->
            Command = create_command(MsgType, Map),
            [Pid ! {'command', Command} || {_NodeID, Pid} <- NodeIDPidProps],
            'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_command(string(), map()) -> binary() | [binary(), ...].
create_command("LightingForceState", Map) ->
    StateMap = maps:with(["pri", "mask", "level", "qualifiers"], Map),
    Map1 = maps:put("state", StateMap, Map),
    Req = lightingForceState:to_record(Map1),
    lager:debug("LightingForceState ~p", [Req]),
    Envelope = envelope:create(Req),
    envelope:encode(Envelope);
create_command("LightingScheduledEvent", Map) ->
    Schedules =
        lists:foldl(
            fun(Schedule, Acc) ->
                StateMap = maps:with(["pri", "mask", "level", "qualifiers"], Schedule),
                EventMap = maps:with(["sec", "min", "hr", "wday", "mday", "mon", "year"], Schedule),
                Map1 = maps:put("state", StateMap, Schedule),
                Map2 = maps:put("event", EventMap, Map1),
                Req = lightingScheduledEvent:to_record(Map2),
                lager:debug("LightingScheduledEvent ~p", [Req]),
                Envelope = envelope:create(Req),
                [envelope:encode(Envelope)|Acc]
            end
            ,[]
            ,maps:get("schedules", Map, [])
        ),
    ClearCmd = create_command("LightingClearSchedule", #{}),
    AutoCmd = create_command("LightingSetAuto", #{}),
    lager:debug("LightingClearSchedule -> LightingScheduledEvent -> LightingSetAuto"),
    [ClearCmd] ++ Schedules ++ [AutoCmd];
create_command("ConfigResp", Map) ->
    KVPairs = maps:get("kvpairs", Map),
    Token = maps:get("token", KVPairs, 'undefined'),
    Envelopes =
        maps:fold(
            fun(Key, Val, Acc) ->
                Resp = configResp:create(Key, Val),
                lager:debug("ConfigResp ~p", [Resp]),
                Envelope = envelope:create(Resp),
                [envelope:encode(Envelope)|Acc]
            end
            ,[]
            ,maps:remove("token", KVPairs)
        ),
    TokenResp = configResp:create("token", Token),
    TokenEnv = envelope:create(TokenResp),
    ActionCmd = create_command("DeviceActionReq", #{"action" => 'ColdReset'}),
    lager:debug("ConfigResp(s) -> Token -> DeviceActionReq"),
    lists:reverse(Envelopes) ++ [
        envelope:encode(TokenEnv)
        ,ActionCmd
    ];
create_command("DeviceActionReq", Map) ->
    Action = legacy_helpers:symbol_to_atom(maps:get("action", Map)),
    Req = deviceActionReq:create(Action),
    lager:debug("DeviceActionReq ~p", [Req]),
    Envelope = envelope:create(Req),
    envelope:encode(Envelope);
create_command(Type, Map) ->
    Module = envelope:str_type_to_atom(Type),
    Req = Module:to_record(Map),
    lager:debug("~p ~p", [Type, Req]),
    Envelope = envelope:create(Req),
    envelope:encode(Envelope).

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

mock() ->
    meck:new('legacy_helpers', ['passthrough']),
    meck:new('exometer', ['no_link', 'passthrough']),
    meck:expect('legacy_helpers', 'filter_offline_nodes'
        ,fun(NodeIDs) ->
            lists:map(fun(NodeID) -> {NodeID, self()} end, NodeIDs)
        end
    ),
    meck:expect('exometer', 'new', fun(_, _) -> 'ok' end),
    meck:expect('exometer', 'update', fun(_, _) -> 'ok' end).


handle_sensor_sample_test() ->
    mock(),

    Req = sensorSampleReq:create(<<"l">>),
    Map = sensorSampleReq:to_map(Req),
    Map1 = maps:put("nodeid", ["J007"], Map),
    Packed = msgpack:pack(Map1),
    Envelope = envelope:create(Req),
    Encoded = envelope:encode(Envelope),

    ?assertMatch('ok', handle(Packed)),
    receive
        Msg ->
            ?assertMatch({'command', Encoded}, Msg)
    after 1000 ->
        ?assertMatch('ok', 'timeout')
    end,

    meck:unload(),
    'ok'.

handle_sensor_sample_test_with_string_test() ->
    mock(),

    Req = sensorSampleReq:create(<<"l">>),
    Map = sensorSampleReq:to_map(Req),
    Map1 = maps:put("nodeid", "J007", Map),
    Packed = msgpack:pack(Map1),
    Envelope = envelope:create(Req),
    Encoded = envelope:encode(Envelope),

    ?assertMatch('ok', handle(Packed)),
    receive
        Msg ->
            ?assertMatch({'command', Encoded}, Msg)
    after 1000 ->
        ?assertMatch('ok', 'timeout')
    end,

    meck:unload(),
    'ok'.

handle_device_action_req_test() ->
    mock(),

    Map = #{"name" => "DeviceActionReq", "action" => "ColdReset", "nodeid" => ["J007"]},
    Packed = msgpack:pack(Map),

    Req = deviceActionReq:create('ColdReset'),
    Envelope = envelope:create(Req),
    Encoded = envelope:encode(Envelope),

    ?assertMatch('ok', handle(Packed)),
    receive
        Msg ->
            ?assertMatch({'command', Encoded}, Msg)
    after 1000 ->
        ?assertMatch('ok', 'timeout')
    end,

    meck:unload(),
    'ok'.

handle_config_resp_test() ->
    mock(),

    Pairs = #{
        "token" => "c12345"
        ,"key1" => "value1"
        ,"key2" => 2
        ,"key3" => 'true'
        ,"key4" => 'false'
    },
    Map = #{"name" => "ConfigResp", "nodeid" => ["J007"], "kvpairs" => Pairs},
    Packed = msgpack:pack(Map),

    K1 = configResp:create("key1", "value1"),
    K2 = configResp:create("key2", 2),
    K3 = configResp:create("key3", 'true'),
    K4 = configResp:create("key4", 'false'),
    T = configResp:create("token", "c12345"),
    Reset = deviceActionReq:create('ColdReset'),
    Encode = fun(D) ->
        E = envelope:create(D),
        envelope:encode(E)
    end,

    Cmds = [
        Encode(K1), Encode(K2), Encode(K3), Encode(K4)
        ,Encode(T), Encode(Reset)
    ],

    ?assertMatch('ok', handle(Packed)),
    receive
        Msg ->
            ?assertMatch({'command', Cmds}, Msg)
    after 1000 ->
        ?assertMatch('ok', 'timeout')
    end,

    meck:unload(),
    'ok'.

handle_lighting_force_state_test() ->
    mock(),

    Map = #{
        "name" => "LightingForceState"
        ,"nodeid" => ["J007"]
        ,"pri" => 1
        ,"mask" => 1
        ,"level" => 1
        ,"qualifiers" => "undefined"
        ,"ftype" => "Volatile"
    },
    Packed = msgpack:pack(Map),

    State = lightingCtrl:create(1, 1, 1, 'undefined'),
    LFS = lightingForceState:create(State, 'Volatile'),
    Envelope = envelope:create(LFS),
    Encoded = envelope:encode(Envelope),

    ?assertMatch('ok', handle(Packed)),
    receive
        Msg ->
            ?assertMatch({'command', Encoded}, Msg)
    after 1000 ->
        ?assertMatch('ok', 'timeout')
    end,

    meck:unload(),
    'ok'.

handle_lighting_sch_event_test() ->
    mock(),

    Map = #{
        "name" => "LightingScheduledEvent"
        ,"nodeid" => ["J007"]
        ,"schedules" => [
            #{
                "id" => 1
                ,"pri" => 1
                ,"mask" => 1
                ,"level" => 1
                ,"qualifiers" => "undefined"
                ,"ftype" => "Volatile"
                ,"sec" => 1
                ,"min" => 1
                ,"hr" => 1
                ,"wday" => 1
                ,"mday" => 1
                ,"mon" => 1
                ,"year" => 2017
            }
        ]
    },
    Packed = msgpack:pack(Map),

    State = lightingCtrl:create(1, 1, 1, 'undefined'),
    Event = calendarEvent:create(1, 1, 1, 1, 1, 1, 2017),
    LSE = lightingScheduledEvent:create(1, Event, State),
    Encode = fun(D) ->
        E = envelope:create(D),
        envelope:encode(E)
    end,

    Cmds = [
        Encode(lightingClearSchedule:create())
        ,Encode(LSE)
        ,Encode(lightingSetAuto:create())
    ],

    ?assertMatch('ok', handle(Packed)),
    receive
        Msg ->
            ?assertMatch({'command', Cmds}, Msg)
    after 1000 ->
        ?assertMatch('ok', 'timeout')
    end,

    meck:unload(),
    'ok'.

-endif.
