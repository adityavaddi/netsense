%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Login Request ==
%% Login Request message API
%% @end
%%%-------------------------------------------------------------------
-module(loginReq).

-export([
    to_map/1
    ,to_record/1
    ,create/2
    ,key/2
]).

-export([
    get_fw_id/2
    ,test_map/1 ,test_map/2
    ,test_record/1 ,test_record/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'LoginReq'() :: #'LoginReq'{}.

-export_type(['LoginReq'/0]).

%%--------------------------------------------------------------------
%% @doc
%% Convert LoginReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('LoginReq'()) -> map().
to_map(LR) ->
    {SubType, Voltage} = client_sub_type(LR#'LoginReq'.clientSubType),
    #{
        "name" => "LoginReq"
        ,"nodeid" => legacy_helpers:symbol_to_string(LR#'LoginReq'.nodeId)
        ,"protocolVersion" => LR#'LoginReq'.protocolVersion
        ,"clientType" => legacy_helpers:symbol_to_string(LR#'LoginReq'.clientType)
        ,"swVerId" => legacy_helpers:symbol_to_string(LR#'LoginReq'.swVerId)
        ,"netName" => legacy_helpers:symbol_to_string(LR#'LoginReq'.netName)
        ,"profileName" => legacy_helpers:symbol_to_string(LR#'LoginReq'.profileName)
        ,"assocChannel" => legacy_helpers:symbol_to_string(LR#'LoginReq'.assocChannel)
        ,"configToken" => legacy_helpers:symbol_to_string(LR#'LoginReq'.configToken)
        ,"localIP" => legacy_helpers:symbol_to_string(LR#'LoginReq'.localIP)
        ,"time" => LR#'LoginReq'.time
        ,"bssid" => legacy_helpers:symbol_to_string(LR#'LoginReq'.bssid)
        ,"mac" => legacy_helpers:symbol_to_string(LR#'LoginReq'.mac)
        ,"auth" => legacy_helpers:symbol_to_string(LR#'LoginReq'.auth)
        ,"subType" => SubType
        ,"voltageType" => Voltage
        ,"modemRevEd" => legacy_helpers:symbol_to_string(LR#'LoginReq'.modemRevEd)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert LoginReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'LoginReq'().
to_record(Map) ->
    SubType = maps:get("subType", Map, 'undefined'),
    VoltageType  = maps:get("voltageType", Map, 'undefined'),
    ClientSubType =
        case {SubType, VoltageType} of
            {'undefined', _} -> 'undefined';
            {_, 'undefined'} -> 'undefined';
            _ ->
                <<(integer_to_binary(SubType))/binary, ","
                  ,(integer_to_binary(VoltageType))/binary>>
        end,
    #'LoginReq'{
        nodeId = legacy_helpers:symbol_to_binary(maps:get("nodeid", Map))
        ,protocolVersion = legacy_helpers:symbol_to_binary(maps:get("protocolVersion", Map))
        ,clientType = legacy_helpers:symbol_to_binary(maps:get("clientType", Map, 'undefined'), 'undefined')
        ,swVerId = legacy_helpers:symbol_to_binary(maps:get("swVerId", Map, 'undefined'), 'undefined')
        ,netName = legacy_helpers:symbol_to_binary(maps:get("netName", Map, 'undefined'), 'undefined')
        ,profileName = legacy_helpers:symbol_to_binary(maps:get("profileName", Map, 'undefined'), 'undefined')
        ,assocChannel = legacy_helpers:symbol_to_binary(maps:get("assocChannel", Map, 'undefined'), 'undefined')
        ,configToken = legacy_helpers:symbol_to_binary(maps:get("configToken", Map, 'undefined'), 'undefined')
        ,localIP = legacy_helpers:symbol_to_binary(maps:get("localIP", Map, 'undefined'), 'undefined')
        ,time = maps:get("time", Map, 'undefined')
        ,bssid = legacy_helpers:symbol_to_binary(maps:get("bssid", Map, 'undefined'), 'undefined')
        ,mac = legacy_helpers:symbol_to_binary(maps:get("mac", Map, 'undefined'), 'undefined')
        ,auth = legacy_helpers:symbol_to_atom(maps:get("auth", Map, 'undefined'))
        ,clientSubType = ClientSubType
        ,modemRevEd  = legacy_helpers:symbol_to_binary(maps:get("modemRevEd", Map, 'undefined'), 'undefined')
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login request
%% @end
%%--------------------------------------------------------------------
-spec create(binary(), integer()) -> 'LoginReq'().
create(NodeID, Version) ->
    #'LoginReq'{
        nodeId = NodeID
        ,protocolVersion = Version
    }.

%%--------------------------------------------------------------------
%% @doc
%% Return routing key
%% @end
%%--------------------------------------------------------------------
-spec key(binary(), 'LoginReq'() | map()) -> binary().
key(_NodeID, Map) when is_map(Map) ->
    NodeID = legacy_helpers:to_binary(maps:get("nodeid", Map)),
    <<NodeID/binary, ".login.req">>;
key(_NodeID, #'LoginReq'{nodeId=NodeID}) ->
    <<NodeID/binary, ".login.req">>.

%%--------------------------------------------------------------------
%% @doc
%% Return firmware ID by type
%% @end
%%--------------------------------------------------------------------
-spec get_fw_id(string(), 'LoginReq'()) -> string().
get_fw_id("mcu", LoginReq) ->
    legacy_helpers:symbol_to_string(LoginReq#'LoginReq'.swVerId);
get_fw_id("modem", LoginReq) ->
    legacy_helpers:symbol_to_string(LoginReq#'LoginReq'.modemRevEd).

%%--------------------------------------------------------------------
%% @doc
%% Return test Login Req map
%% @end
%%--------------------------------------------------------------------
-spec test_map(binary() | string()) -> map().
test_map(NodeID) ->
    test_map(NodeID, []).

-spec test_map(binary() | string(), list()) -> map().
test_map(NodeID, Props) ->
    #{
        "name" => "LoginReq"
        ,"nodeid" => legacy_helpers:symbol_to_string(NodeID)
        ,"assocChannel" => proplists:get_value("assocChannel", Props, "120")
        ,"auth" => proplists:get_value("auth", Props, "WPA2_PSK")
        ,"bssid" => proplists:get_value("bssid", Props, "ba:6e:6f:80:e9:3e")
        ,"clientType" => proplists:get_value("clientType", Props, "unode-v4")
        ,"configToken" => proplists:get_value("configToken", Props, "ppk")
        ,"localIP" => proplists:get_value("localIP", Props, "10.0.0.7")
        ,"mac" => proplists:get_value("mac", Props, "cd:a4:b9:8a:b5:fa")
        ,"name" => proplists:get_value("name", Props, "LoginReq")
        ,"netName" => proplists:get_value("netName", Props, "GoldFinger")
        ,"profileName" => proplists:get_value("profileName", Props, "pName")
        ,"protocolVersion" => proplists:get_value("protocolVersion", Props, 1)
        ,"swVerId" => proplists:get_value("swVerId", Props,  "a3441a938")
        ,"time" => proplists:get_value("time", Props, 1485213091784285)
        ,"subType" => proplists:get_value("subType", Props, 1)
        ,"voltageType" => proplists:get_value("voltageType", Props, 2)
        ,"modemRevEd" => proplists:get_value("modemRevEd", Props,  "J001_v10")
    }.

%%--------------------------------------------------------------------
%% @doc
%% Return test Login Req map
%% @end
%%--------------------------------------------------------------------
-spec test_record(binary() | string()) -> 'LoginReq'().
test_record(NodeID) ->
    test_record(NodeID, []).

-spec test_record(binary() | string(), list()) -> 'LoginReq'().
test_record(NodeID, Props) ->
    #'LoginReq'{
        nodeId = legacy_helpers:symbol_to_binary(NodeID)
        ,assocChannel = proplists:get_value("assocChannel", Props, <<"120">>)
        ,auth = proplists:get_value("auth", Props, 'WPA2_PSK')
        ,bssid = proplists:get_value("bssid", Props, <<"ba:6e:6f:80:e9:3e">>)
        ,clientType = proplists:get_value("clientType", Props, <<"unode-v4">>)
        ,configToken = proplists:get_value("configToken", Props, <<"ppk">>)
        ,localIP = proplists:get_value("localIP", Props, <<"10.0.0.7">>)
        ,mac = proplists:get_value("mac", Props, <<"cd:a4:b9:8a:b5:fa">>)
        ,netName = proplists:get_value("netName", Props, <<"GoldFinger">>)
        ,profileName = proplists:get_value("profileName", Props, <<"pName">>)
        ,protocolVersion = proplists:get_value("protocolVersion", Props, 1)
        ,swVerId = proplists:get_value("swVerId", Props, <<"a3441a938">>)
        ,time = proplists:get_value("time", Props, 1485213091784285)
        ,clientSubType = proplists:get_value("clientSubType", Props, <<"1,2">>)
        ,modemRevEd = proplists:get_value("modemRevEd", Props, <<"J001_v10">>)
    }.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec client_sub_type(binary() | 'undefined') -> {integer() | string(), integer() | string()}.
client_sub_type(CST) when is_binary(CST) ->
    [SubType, Voltage] = binary:split(CST, <<",">>),
    {binary_to_integer(SubType), binary_to_integer(Voltage)};
client_sub_type(_CST) ->
    {"undefined", "undefined"}.

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

to_map_test() ->
    ?assertEqual(test_map(<<"J007">>), to_map(test_record(<<"J007">>))),
    'ok'.

to_record_test() ->
    ?assertEqual(test_record(<<"J007">>), to_record(test_map(<<"J007">>))),
    'ok'.

create_test() ->
    ?assertEqual(#'LoginReq'{
        nodeId = <<"J007">>
        ,protocolVersion = 2
    }, create(<<"J007">>, 2)).

key_test() ->
    ?assertEqual(<<"J007.login.req">>, key(<<>>, #'LoginReq'{nodeId= <<"J007">>})),
    ?assertEqual(<<"J007.login.req">>, key(<<>>, #{"nodeid" => "J007"})).

get_fw_id_test() ->
    LoginReq = test_record(<<"J007">>),
    ?assertEqual("a3441a938", get_fw_id("mcu", LoginReq)),
    ?assertEqual("J001_v10", get_fw_id("modem", LoginReq)).

client_sub_type_test() ->
    ?assertEqual({1, 2}, client_sub_type(<<"1,2">>)),
    ?assertEqual({"undefined", "undefined"}, client_sub_type('undefined')),
    ?assertEqual({"undefined", "undefined"}, client_sub_type("random")),
    'ok'.

-endif.
