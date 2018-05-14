-module(legacy_config_worker).
-author("Gina Hagg ghagg@sensity.com").

-compile([{'parse_transform', 'lager_transform'}]).

-export([unpack_protify_send/1]).
%%Below is the ColdReset command to be sent to the node after the config command is sent for the node to reboot.
-define(ColdReset, <<66,2,8,0>>).

-type config_resp() :: {'ConfigResp', {'KVPair', binary(), string() | 'undefined', number() | 'undefined', boolean() | 'undefined'}}.

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
unpack_protify_send(UnpackedMsgs) ->
    lager:debug("Config Update command:  ~p~n", [UnpackedMsgs]),
    %{ok, UnpackedMsgs} = msgpack:unpack(Payload),
    Nodeids = maps:get("nodeid", UnpackedMsgs),
    ConfigMapp = maps:get("kvpairs", UnpackedMsgs),
    L = maps:to_list(ConfigMapp),
    L1 = lists:map(fun(X)-> {Y,Z} = X, case is_binary(Y) of true -> {binary_to_list(Y),Z}; _-> {Y,Z}  end end, L),
    ConfigMap = maps:from_list(L1),
    ConfigType = maps:get("configtype",UnpackedMsgs, "configupdate"),
    lager:debug("node ids: ~p, configtype: ~p~n", [Nodeids, ConfigType]),
    BatchConfigMsgs = convert_to_proto_config(ConfigMap),
    Batch = helpers:flatten3([set_token_as_last(ConfigMap, BatchConfigMsgs)| [?ColdReset]]),
    %lager:debug("Config Batch being sent to node: ~p~n",[Batch]),
    subscriber_helper:send_to_nodes(
        Batch
        ,"ConfigRespUpdate"
        ,Nodeids
        ,UnpackedMsgs
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec convert_to_proto_config(map())-> list().
convert_to_proto_config(ConfigMap) ->
    Keys = lists:delete("token", maps:keys(ConfigMap)),
    lists:map(
        fun(Key) ->
            Value = maps:get(Key, ConfigMap, 'undefined'),
            ConfigMsg = config_msg(Key, Value),
            unode_proto_handler:encode_env_by_type('ConfigResp', ConfigMsg, "All")
        end
        ,Keys
    ).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec set_token_as_last(map(), list())-> list().
set_token_as_last(ConfigMap, BatchConfigMsgs) ->
    Value = maps:get("token", ConfigMap, 'undefined'),
    ConfigMsg = config_msg("token", Value),
    Encoded = unode_proto_handler:encode_env_by_type('ConfigResp', ConfigMsg, "All"),
    BatchConfigMsgs ++ Encoded.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec config_msg(binary(), any())-> config_resp().
config_msg(Key, Value) when is_list(Value) ->
    {'ConfigResp'
        ,{'KVPair', Key, Value, 'undefined', 'undefined'}
    };
config_msg(Key, Value) when is_binary(Value) ->
    {'ConfigResp'
        ,{'KVPair', Key, binary:bin_to_list(Value), 'undefined', 'undefined'}
    };
config_msg(Key, Value) when is_number(Value) ->
    {'ConfigResp'
        ,{'KVPair', Key, 'undefined', Value, 'undefined'}
    };
config_msg(Key, 'true') ->
    {'ConfigResp'
        ,{'KVPair', Key, 'undefined', 'undefined', 'true'}
    };
config_msg(Key, 'false') ->
    {'ConfigResp'
        ,{'KVPair', Key, 'undefined', 'undefined', 'false'}
    }.