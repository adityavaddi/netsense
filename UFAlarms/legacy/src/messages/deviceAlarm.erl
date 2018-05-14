%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Device Alarm ==
%% Device Alarm message API
%% @end
%%%-------------------------------------------------------------------
-module(deviceAlarm).

-export([
    to_map/1
    ,to_record/1
    ,create/1
    ,key/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'DeviceAlarm'() :: #'DeviceAlarm'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert DeviceAlarm record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('DeviceAlarm'()) -> map().
to_map(DA) ->
    #{
        "name" => "DeviceAlarm"
        ,"alarmType" => legacy_helpers:symbol_to_string(DA#'DeviceAlarm'.alarmType)
        ,"alarmSeverity" => legacy_helpers:symbol_to_string(DA#'DeviceAlarm'.alarmSeverity)
        ,"msg" => legacy_helpers:symbol_to_string(DA#'DeviceAlarm'.msg)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert DeviceAlarm record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'DeviceAlarm'().
to_record(Map) ->
    #'DeviceAlarm'{
        alarmType = legacy_helpers:symbol_to_atom(maps:get("alarmType", Map))
        ,alarmSeverity = legacy_helpers:symbol_to_atom(maps:get("alarmSeverity", Map))
        ,msg = legacy_helpers:symbol_to_binary(maps:get("msg", Map, 'undefined'), 'undefined')
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(atom()) -> 'DeviceAlarm'().
create(Severity) ->
    #'DeviceAlarm'{alarmSeverity=Severity}.

%%--------------------------------------------------------------------
%% @doc
%% Return routing key
%% @end
%%--------------------------------------------------------------------
-spec key(binary(), 'DeviceAlarm'() | map()) -> binary().
key(NodeID, Map) when is_map(Map) ->
    Type = legacy_helpers:symbol_to_binary(maps:get("alarmType", Map)),
    <<NodeID/binary, ".alarm.", Type/binary>>;
key(NodeID, #'DeviceAlarm'{alarmType=Type}) ->
    <<NodeID/binary, ".alarm.", (legacy_helpers:symbol_to_binary(Type))/binary>>.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

to_map_test() ->
    DeviceAlarm = #'DeviceAlarm'{
        alarmType = 'CommFail'
        ,alarmSeverity = 'Critical'
        ,msg = <<"Something went wrong">>
    },
    Map = #{
        "name" => "DeviceAlarm"
        ,"alarmType" => "CommFail"
        ,"alarmSeverity" => "Critical"
        ,"msg" => "Something went wrong"
    },
    ?assertEqual(Map, to_map(DeviceAlarm)),
    'ok'.

to_record_test() ->
    DeviceAlarm = #'DeviceAlarm'{
        alarmType = 'CommFail'
        ,alarmSeverity = 'Critical'
        ,msg = <<"Something went wrong">>
    },
    Map = #{
        "name" => "DeviceAlarm"
        ,"alarmType" => "CommFail"
        ,"alarmSeverity" => "Critical"
        ,"msg" => "Something went wrong"
    },
    ?assertEqual(DeviceAlarm, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'DeviceAlarm'{alarmSeverity='Clear'}, create('Clear')).

key_test() ->
    ?assertEqual(<<"J007.alarm.CommFail">>, key(<<"J007">>, #{"alarmType" => "CommFail"})),
    ?assertEqual(<<"J007.alarm.CommFail">>, key(<<"J007">>, #'DeviceAlarm'{alarmType='CommFail'})).

-endif.
