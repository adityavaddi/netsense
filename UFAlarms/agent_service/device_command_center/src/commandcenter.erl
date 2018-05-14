-module(commandcenter).
-author("Chiradip Mandal").
-author("Gina Hagg ghagg@sensity.com").

-behaviour(gen_server).

-define(NEW_CFG_TOPIC, <<"v1/+/out/va-v1/cfg/#">>).
-define(NEW_EVT_TOPIC, <<"v1/+/out/va-v1/evt/#">>).
-define(NEW_CFGREQ_BATCH_UUID, <<"v1/{{NODE_ID}}/in/va-v1/cfgreq/batchuuid">>).
-define(NEW_CFGREQ_BATCH_TYPE, <<"v1/{{NODE_ID}}/in/va-v1/cfgreq/batchtype">>).

-define(NODE_ID_REPLACE, <<"{{NODE_ID}}">>).

-define(LB,get_host()).
-define(TIMEOUT, 5 * 1000).
-define(ParkingNames, [
    {"ndpark", "NonDemarcatedParking"}
    ,{"dpark", "DemarcatedParking"}
    ,{"linec", "LineCrossing"}
    ,{"NonDemarcatedParking", "NonDemarcatedParking"}
    ,{"DemarcatedParking", "DemarcatedParking"}
    ,{"ParkingKeepOut", "ParkingKeepOut"}
    ,{"pko", "ParkingKeepOut"}
    ,{"bcfgreq", "BatchConfigReq"}
    ,{"bcfgresp", "BatchConfigResp"}
    ,{"batchuuid", "BatchUuid"}
    ,{"objent", "ObjectEntering"}
    ,{"objlev", "ObjectLeaving"}
    ,{"inventory", "Inventory"}
    ,{"objdwl", "ObjectDwell"}
    ,{"stat", "Stats"}
    ,{"cfgreq", "ConfigRequest"}
]).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0
    ,stop/0
    ,q_length/1
    ,handle_mqtt_msg/3
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
    init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,terminate/2
    ,code_change/3
]).

-record(state, {mqttc, args}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    {'ok', Host} = application:get_env('host'),
    {'ok', MqttOpts} = application:get_env('mqtt'),

    MqttHost = proplists:get_value('host', MqttOpts),
    MqttPort = proplists:get_value('port', MqttOpts),
    Clientid = proplists:get_value('cmd_client_id', MqttOpts),
    Reconnect = proplists:get_value('reconnect', MqttOpts),
    KeepAlive = proplists:get_value('keepalive', MqttOpts),
    LogLevel = proplists:get_value('loglevel', MqttOpts),

    Username = proplists:get_value('username', MqttOpts),
    Pwd = proplists:get_value('pwd', MqttOpts),
    SSL = proplists:get_value('ssl', MqttOpts, []),

    Random = erlang:integer_to_list(crypto:rand_uniform(1, 9999999)),
    ClientId = erlang:iolist_to_binary([Clientid , "-" , Host, "-", Random]),
    Args = [
        {'host', MqttHost}
        ,{'port', MqttPort}
        ,{'keepalive', KeepAlive}
        ,{'client_id', ClientId}
        ,{'reconnect', Reconnect}
        ,{'logger', {'lager', LogLevel}}
        ,{'username', erlang:list_to_binary(Username)}
        ,{'password', erlang:list_to_binary(Pwd)}
    ],
    case erlang:length(SSL) of
        0 ->
            lager:info("connection to mqtt using username/pwd ~p", [Args]),
            gen_server:start_link({'local', ?MODULE}, ?MODULE, Args, [{'keepalive', 'true'}]);
        _ ->
            lager:info("connection to mqtt using SSL ~p", [SSL]),
            gen_server:start_link({'local', ?MODULE}, ?MODULE, Args ++ [{'ssl', SSL}], [{'keepalive', 'true'}])
    end.

stop() ->
    gen_server:call(?MODULE, 'stop').

q_length(Name) ->
    erlang:process_info(whereis(Name), 'message_queue_len').

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%%initialize emqttc and subscribe rightaway
init(Args) ->
    process_flag('trap_exit', 'true'),
    lager:info("init with ~p", [Args]),
    self() ! 'connect',
    {'ok', #state{args=Args}}.

handle_call('stop', _From, State) ->
    {'stop', 'normal', 'ok', State};
handle_call(_, _From, State) ->
    {'noreply', State}.

handle_cast(_Msg, State) ->
    {'noreply', State}.

handle_info('connect', #state{args=Args}=State) ->
    case emqttc:start_link(Args) of
        {'ok', C} ->
            lager:info("started, mqtt connection @ ~p", [C]),
            {'noreply', State#state{mqttc=C}};
        _E ->
            lager:error("fail to connect to mqtt: ~p using ~p, reconnecting...", [_E, Args]),
            erlang:send_after(5000, self(), 'connect'),
            {'noreply', State}
    end;
handle_info({'publish', Topic, Msg}, #state{mqttc=C}=State) ->
    _ = dcc_metrics:msg_rate_incoming(1),
    erlang:spawn(fun() ->
        lager:debug("rcvd ~p", [Topic]),
        dcc_metrics:msg_timer(
            ?MODULE
            ,'handle_mqtt_msg'
            ,[Topic, Msg, C]
        )
    end),
    {'noreply', State};
%%MQTT Client connected
handle_info({'mqttc', C, 'connected'}, #state{mqttc = C}=State) ->
    lager:info("mqtt client connected with ~p", [C]),
    emqttc:subscribe(C, ?NEW_CFG_TOPIC, 0),
    emqttc:subscribe(C, ?NEW_EVT_TOPIC, 0),
    {'noreply', State};
%% MQTT Client disconnected
handle_info({'mqttc', C,  'disconnected'}, #state{mqttc=C}=State) ->
    lager:warning("MQTT Client ~p is disconnected", [C]),
    emqttc:unsubscribe(C, ?NEW_CFG_TOPIC),
    emqttc:unsubscribe(C, ?NEW_EVT_TOPIC),
    {'noreply', State};
handle_info({'EXIT', C, _Reason}, #state{mqttc=C}=State) ->
    lager:error("mqtt connection ~p crashed ~p, reconnecting...", [C, _Reason]),
    erlang:send_after(5000, self(), 'connect'),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("rcv unknwon info message: ~p", [_Info]),
    {'noreply', State}.

terminate(Reason, _State) ->
    lager:warning("commandcenter terminated: ~p",[Reason]),
    'ok'.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%% ------------------------------------------------------------------
%%private functions
%% ------------------------------------------------------------------
handle_mqtt_msg(Topic, Msg, Conn) ->
    Tokens = string:tokens(erlang:binary_to_list(Topic), "/"),
    {MsgType, NodeId, EvtType} = splitTopic(Tokens),
    lager:md([{'nodeid', NodeId}]),
    case msgpack:unpack(Msg) of
        {'ok', UnpackedMsg} ->
            lager:debug("msg type: ~p evt type: ~p", [MsgType, EvtType]),
            lager:debug("msg: ~p", [UnpackedMsg]),

            CachedData = case dcc_cache:lookup(NodeId) of
                'undefined' ->
                    send_all_type_request(NodeId, Conn),
                    [];
                Data -> Data
            end,

            _ = handle_incoming(MsgType, EvtType, NodeId, UnpackedMsg, Tokens, Conn, CachedData);
        _Error ->
            lager:error("~p caused by ~p", [_Error, Topic]),
            lager:error("message: ~p", [Msg])
    end.

handle_incoming("cfgreq", "cfg", _NodeId, _UnpackedMsg, _Tokens, _Con, _CachedData) ->
    'ok';
handle_incoming("batchtype", _EvtType, NodeId, UnpackedMsg, _Tokens, _Con, CachedData) ->
    prev_current_single_batch_config_msg(NodeId, UnpackedMsg, CachedData),
    'ok';
handle_incoming("batchuuid", _EvtType, NodeId, UnpackedMsg, _Tokens, _Con, CachedData) ->
    prev_current_single_batch_config_msg(NodeId, UnpackedMsg, CachedData),
    'ok';
handle_incoming("inventory", _EvtType, NodeId, UnpackedMsg, _Tokens, Con, _CachedData) ->
    send_cfg_request(NodeId, UnpackedMsg, Con),
    'ok';
handle_incoming(_MsgType, "cfg", NodeId, UnpackedMsg, Tokens, _Con, CachedData) ->
    transform_msg(NodeId, UnpackedMsg, Tokens, CachedData);
handle_incoming(_MsgType, "evt", NodeId, UnpackedMsg, Tokens, _Con, CachedData) ->
    transform_msg(NodeId, UnpackedMsg, Tokens, CachedData);
handle_incoming(MsgType, EvtType, NodeId, UnpackedMsg, _Tokens, _Con, _CachedData) ->
    insert_name_and_send(MsgType, EvtType, NodeId, UnpackedMsg).

transform_msg(NodeId, UnpackedMsg, Tokens, Config) ->
    {_, _, _, _Uuid} = Identifier = dcc_video_msg_transformer:identifiying_params(Tokens, UnpackedMsg),
    NewConfig =
        case  dcc_video_msg_transformer:figure_out_roi_spots(Identifier, UnpackedMsg, Config) of
            Config -> Config;
            Config1 ->
                dcc_cache:insert(NodeId, Config1),
                Config1
        end,
    % Occps = dcc_video_msg_transformer:cache_last_event(Identifier, UnpackedMsg),

    % LastOCC = dcc_cache:lookup({'occ', Uuid}),
    % case Occps of
    %     {} -> 'ignore';
    %     {Uuid, OccData} ->
    %         dcc_cache:insert({'occ', Uuid}, OccData)
    % end,
    %
    % Diff = dcc_video_msg_transformer:get_delta(Identifier, LastOCC, Occps),
    NewPayload = dcc_video_msg_transformer:transform_video_msg(Identifier, UnpackedMsg, NewConfig, Config),
    % FinalPayload = maps:put("delta", Diff, NewPayload),
    zmqpushclient:zmqsend(msgpack:pack(NewPayload)),
    'ok'.

splitTopic(Tokens) ->
    MsgType = lists:nth(length(Tokens), Tokens),
    EvtType = lists:nth(5, Tokens),
    NodeId = lists:nth(2,Tokens),
    {MsgType, NodeId, EvtType}.

prev_current_single_batch_config_msg(NodeId, UnpackedMsg, PrevConfig) ->
    CurrentConfig = maps:get("cfgs", UnpackedMsg),
    dcc_cache:insert(NodeId, CurrentConfig),
    Map = #{
        "name" => "AllConfig"
        ,"nodeid" => NodeId
        ,"prev" => clean_configs(PrevConfig)
        ,"current" => CurrentConfig
    },
    lager:debug("sending ~p to DD", [Map]),
    zmqpushclient:zmqsend(msgpack:pack(Map)).

clean_configs(Configs) ->
    clean_configs(Configs, []).

clean_configs([], Clean) ->
    lists:reverse(Clean);
clean_configs([Cfg|Cfgs], Clean) when is_map(Cfg) ->
    clean_configs(Cfgs, [Cfg|Clean]);
clean_configs([_|Cfgs], Clean)->
    clean_configs(Cfgs, Clean).

send_all_type_request(Nodeid, Con) ->
    Rid = uuid:to_string(uuid:uuid3(uuid:uuid4(), Nodeid)),
    NewTopic = replace(?NEW_CFGREQ_BATCH_TYPE, Nodeid),
    CfgReqMap = #{"rid" => Rid, "type" => "all"},
    lager:info("send ~p to ~p", [CfgReqMap, NewTopic]),
    CfgReq = msgpack:pack(CfgReqMap),
    emqttc:publish(Con, NewTopic, CfgReq, 'qos1').

send_cfg_request(Nodeid, UnpackedMsg, Con) ->
    case maps:get("uuids", UnpackedMsg) of
        [] -> 'ok';
        InvUuids ->
            Rid = uuid:to_string(uuid:uuid3(uuid:uuid4(), Nodeid)),
            CfgReqMap = #{"rid" => Rid, "uuids" => InvUuids},
            NewTopic = replace(?NEW_CFGREQ_BATCH_UUID, Nodeid),
            CfgReq = msgpack:pack(CfgReqMap),
            lager:info("send ~p to ~p", [CfgReqMap, NewTopic]),
            emqttc:publish(Con, NewTopic, CfgReq, 'qos1')
    end.

insert_name_and_send(MsgType, EvtType, NodeId, UnpackedMsg) ->
    M1 = maps:put("nodeid", NodeId , UnpackedMsg),
    IsEvent =
        case EvtType of
            "cfg" -> "Config";
            _ -> "Event"
        end,
    M2 = maps:put("name", proplists:get_value(MsgType, ?ParkingNames) ++ IsEvent, M1),
    lager:debug("sending ~p to DD", [M2]),
    zmqpushclient:zmqsend(msgpack:pack(M2)).

replace(Topic, NodeID) when is_binary(NodeID) ->
    binary:replace(Topic, ?NODE_ID_REPLACE, NodeID);
replace(Topic, NodeID) ->
    replace(Topic, binary:list_to_bin(NodeID)).
