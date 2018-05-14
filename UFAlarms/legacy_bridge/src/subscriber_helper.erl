-module(subscriber_helper).

-compile(export_all).

-compile([{parse_transform, lager_transform}]).

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
send_to_nodes(ProtoBinary, MsgName, Nodeids, _UnpackedMsgs) ->
    lager:debug("node ids: ~p", [Nodeids]),
    Nodeids2 = [
        case is_binary(NodeId) of
            'true' -> NodeId;
            _ -> list_to_binary(NodeId)
        end
    || NodeId <- Nodeids],
    [send_to_nodes_1(V1, MsgName, ProtoBinary) || V1 <- Nodeids2].


send_to_nodes_1(Nodeid, MsgName, ProtoBinary) ->
    % Pid = helpers:fetch_WsPid_for_node(Nodeid),
    Pid = gproc:where({'n', 'l', Nodeid}),
    case Pid of
        'undefined' ->
            lager:debug("node ~p is not registered with this server.", [Nodeid]),
            helpers:notify_delivery_error(Nodeid, binary_to_list(Nodeid) ++ " is not connected to this server.", MsgName);
        _ ->
            lager:debug("sending protoed message to ~p, for node ~p: ~p", [Pid, Nodeid, MsgName]),
            Pid ! {'mqtt', MsgName, ProtoBinary}
    end.

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
unpack_protify_send(Topic, UnpackedMsgs) ->
    lager:debug("Received ~p message from DCC", [Topic]),
    %{ok, UnpackedMsgs} = msgpack:unpack(Payload),
    lager:debug("subscriber_helper:Message from DCC is: "
    "~p~n",
    [UnpackedMsgs]),
    Nodeids = maps:get("nodeid", UnpackedMsgs),
    ProtoBinary = build_proto_msg(UnpackedMsgs),
    send_to_nodes(ProtoBinary, Topic, Nodeids,
      UnpackedMsgs).

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
translatetoproto(UnpackedMsgs) ->
    EncodedMsgs =
    unode_proto_handler:encode_msgs(UnpackedMsgs),
    lager:debug("binary msg received: ~p", [EncodedMsgs]),
    EncodedMsgs.

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
connect_to_mqtt(ClientId) ->
    {ok, Host} = application:get_env(legacy_bridge, host),
    {ok, Reconnect} = application:get_env(legacy_bridge,
      mqttreconnect),
    {ok, Loggerlevel} = application:get_env(legacy_bridge,
        mqqtloglevel),
    {ok, KeepAlive} = application:get_env(legacy_bridge,
      keepalive),
    emqttc:start_link([auto_resub, {host, Host},
           {client_id, atom_to_binary(ClientId, unicode)},
           {keepalive, KeepAlive}, {reconnect, Reconnect},
           {logger, {lager, Loggerlevel}}]).

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
build_proto_msg(UnpackedMsgs) ->
    Protomsg =
    legacy_msgpack_handler:build_proto_msg_from_msgpack(UnpackedMsgs),
    lager:debug("Protomsg : ~p~n",[Protomsg]),
    EncodedMsgs =
    unode_proto_handler:encode_msgs([Protomsg]),
    lager:debug("handlecommand:legacy_mqtt encoded to "
           "proto: ~p",
           [EncodedMsgs]),
    EncodedMsgs.

build_proto_msg("LightingScheduledEvent",
    UnpackedMsgs) ->
    lighting_handler:prepare_schedules(UnpackedMsgs);
build_proto_msg(_Msgname, UnpackedMsgs) ->
    Protomsg =
    legacy_msgpack_handler:build_proto_msg_from_msgpack(UnpackedMsgs),
    EncodedMsgs =
    unode_proto_handler:encode_msgs([Protomsg]),
    lager:debug("handlecommand:legacy_mqtt encoded to "
           "proto: ~p",
           [EncodedMsgs]),
    EncodedMsgs.
