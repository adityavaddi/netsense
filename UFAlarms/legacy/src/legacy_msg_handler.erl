%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Legacy message handler ==
%% Handle messages received from node.
%% @end
%%%-------------------------------------------------------------------
-module(legacy_msg_handler).

-export([
    handle/3
    ,connected_msg/1
    ,disconnected_msg/1
]).

-include("legacy.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%--------------------------------------------------------------------
%% @doc
%% Decode message, create map and publish it to rabbitmq
%% @end
%%--------------------------------------------------------------------
-spec handle(binary(), binary(), pid()) -> 'ok'.
handle(NodeID, Msg, WSPid) ->
    legacy_helpers:init_lager(NodeID),

    Envelope = envelope:decode(Msg),
    {Type, Data} = envelope:type(Envelope),
    lager:debug("received message from ~p of type: ~p from ~p", [NodeID, Type, WSPid]),
    lager:debug("paylaod ~p", [Data]),

    Map = Type:to_map(Data),
    Map1 = maps:put("nodeid", legacy_helpers:symbol_to_string(NodeID), Map),
    lager:debug("packing map: ~p", [Map1]),
    Packed = msgpack:pack(Map1),

    case should_respond(Type, Data, NodeID) of
        'undefined' -> 'ok';
        {'ok', Resp} ->
            WSPid ! {'command', Resp}
    end,

    rabbit_helper:publish(
        <<"node.events">>
        ,Type:key(NodeID, Data)
        ,Packed
    ).

%%--------------------------------------------------------------------
%% @doc
%% Publish a disconnect message to rabbitmq
%% @end
%%--------------------------------------------------------------------
-spec connected_msg(binary()) -> 'ok'.
connected_msg(NodeID) ->
    Data = connection_status:connected(),
    Map = connection_status:to_map(Data),
    Map1 = maps:put("nodeid", legacy_helpers:symbol_to_string(NodeID), Map),
    lager:info("sending connected message from node ~p", [NodeID]),
    rabbit_helper:publish(
        <<"node.events">>
        ,connection_status:key(NodeID, Data)
        ,msgpack:pack(Map1)
    ).

%%--------------------------------------------------------------------
%% @doc
%% Publish a disconnect message to rabbitmq
%% @end
%%--------------------------------------------------------------------
-spec disconnected_msg(binary()) -> 'ok'.
disconnected_msg(NodeID) ->
    Data = connection_status:disconnected(),
    Map = connection_status:to_map(Data),
    Map1 = maps:put("nodeid", legacy_helpers:symbol_to_string(NodeID), Map),
    lager:info("sending disconnected message from node ~p", [NodeID]),
    rabbit_helper:publish(
        <<"node.events">>
        ,connection_status:key(NodeID, Data)
        ,msgpack:pack(Map1)
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_respond(binary(), any(), binary()) -> 'undefined' | {'ok', [{'binary', binary()}]}.
should_respond('loginReq', LoginReq, NodeID) ->
    lager:info("sending login resp to ~p", [NodeID]),
    StrNodeID = legacy_helpers:symbol_to_string(NodeID),
    erlang:spawn('legacy_ota_worker', 'handle_login_req', [StrNodeID, LoginReq]),
    LoginResp = loginResp:create(legacy_helpers:time()),
    Envelope = envelope:create(LoginResp),
    Encoded = envelope:encode(Envelope),
    {'ok', Encoded};
should_respond('timeReq', _Data, NodeID) ->
    lager:info("sending time resp to ~p", [NodeID]),
    TimeResp = timeResp:create(legacy_helpers:time()),
    Envelope = envelope:create(TimeResp),
    Encoded = envelope:encode(Envelope),
    {'ok', Encoded};
should_respond(_Type, _Data, _NodeID) ->
    'undefined'.

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

handle_test() ->
    SensorSample = sensorSample:create(<<"lt">>, 0, 12, <<"lux">>),
    SSEnvelope = envelope:create(SensorSample),
    SSEncoded = envelope:encode(SSEnvelope),

    Map = sensorSample:to_map(SensorSample),
    Map1 = maps:put("nodeid", "JB007", Map),
    Packed = msgpack:pack(Map1),

    meck:new('rabbit_helper'),

    meck:expect('rabbit_helper', 'publish'
        ,fun(<<"node.events">>, <<"JB007.sensor.lt">>, Payload) ->
            Payload = Packed,
            'ok'
        end
    ),

    ?assertEqual('ok', handle(<<"JB007">>, SSEncoded, self())),
    ?assert(meck:validate(rabbit_helper)),

    meck:unload(rabbit_helper).

handle_resp_test() ->
    NodeID = <<"JB007">>,
    LoginReq = loginReq:create(NodeID, 2),
    ReqEnvelope = envelope:create(LoginReq),
    ReqEncoded = envelope:encode(ReqEnvelope),
    ReqMap = loginReq:to_map(LoginReq),
    PackedReqMap = msgpack:pack(ReqMap),

    LoginResp = loginResp:create(0),
    RespEnvelope = envelope:create(LoginResp),
    RespEncoded = envelope:encode(RespEnvelope),

    meck:new('legacy_helpers', ['passthrough']),
    meck:new('legacy_ota_worker'),
    meck:new('rabbit_helper'),

    meck:expect('legacy_helpers', 'time', fun() -> 0 end),
    meck:expect('legacy_ota_worker', 'handle_login_req', fun("JB007", Payload) ->
        ?assertMatch(LoginReq, Payload),
        'ok'
    end),
    meck:expect('rabbit_helper', 'publish'
        ,fun(<<"node.events">>, <<"JB007.login.req">>, Payload) ->
            ?assertMatch(PackedReqMap, Payload),
            'ok'
        end
    ),

    ?assertMatch('ok', handle(NodeID, ReqEncoded, self())),
    receive
        Msg ->
            ?assertMatch({'command', RespEncoded}, Msg)
    after 1000 ->
        ?assertMatch("ok", "timeout")
    end,

    ?assert(meck:validate('legacy_ota_worker')),
    ?assert(meck:validate('legacy_helpers')),

    meck:unload('legacy_ota_worker'),
    meck:unload('legacy_helpers'),
    meck:unload(rabbit_helper),
    'ok'.

connected_msg_test() ->
    meck:new('rabbit_helper'),

    meck:expect('rabbit_helper', 'publish', fun(<<"node.events">>, <<"JB007.login.connected">>, _) -> 'ok' end),

    ?assertEqual('ok', connected_msg(<<"JB007">>)),
    ?assert(meck:validate(rabbit_helper)),

    meck:unload(rabbit_helper).

disconnected_msg_test() ->
    meck:new('rabbit_helper'),

    meck:expect('rabbit_helper', 'publish', fun(<<"node.events">>, <<"JB007.login.disconnected">>, _) -> 'ok' end),

    ?assertEqual('ok', disconnected_msg(<<"JB007">>)),
    ?assert(meck:validate(rabbit_helper)),

    meck:unload(rabbit_helper).

-endif.
