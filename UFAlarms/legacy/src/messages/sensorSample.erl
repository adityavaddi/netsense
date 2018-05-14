%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Config Response Done ==
%% Config Response Done message API
%% @end
%%%-------------------------------------------------------------------
-module(sensorSample).

-export([
    to_map/1
    ,to_record/1
    ,create/4
    ,key/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'SensorSample'() :: #'SensorSample'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert SensorSample record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('SensorSample'()) -> map().
to_map(#'SensorSample'{sensor= <<"rf">>, value=Val}=SS) ->
    <<_:32, SN:16/signed-integer, RF:16/signed-integer>> = <<Val:64/integer>>,
    #{
        "name" => "SensorSample"
        ,"sensor" => legacy_helpers:symbol_to_string(SS#'SensorSample'.sensor)
        ,"time" => legacy_helpers:symbol_to_string(SS#'SensorSample'.time)
        ,"value" => legacy_helpers:symbol_to_string(SS#'SensorSample'.value)
        ,"units" => legacy_helpers:symbol_to_string(SS#'SensorSample'.units)
        ,"values" => to_float([RF, SN])
    };
to_map(#'SensorSample'{sensor= <<"jt">>, value=Val}=SS) ->
    <<M:16/signed-integer, Z:16/signed-integer, Y:16/signed-integer, X:16/signed-integer>> = <<Val:64/integer>>,
    #{
        "name" => "SensorSample"
        ,"sensor" => legacy_helpers:symbol_to_string(SS#'SensorSample'.sensor)
        ,"time" => legacy_helpers:symbol_to_string(SS#'SensorSample'.time)
        ,"value" => legacy_helpers:symbol_to_string(SS#'SensorSample'.value)
        ,"units" => legacy_helpers:symbol_to_string(SS#'SensorSample'.units)
        ,"values" => to_float([X, Y, Z, M])
    };
to_map(#'SensorSample'{sensor= <<"bR">>, value=Val}=SS) ->
    <<_:48, BR2:8, BR1:8>> = <<Val:64/integer>>,
    #{
        "name" => "SensorSample"
        ,"sensor" => legacy_helpers:symbol_to_string(SS#'SensorSample'.sensor)
        ,"time" => legacy_helpers:symbol_to_string(SS#'SensorSample'.time)
        ,"value" => legacy_helpers:symbol_to_string(SS#'SensorSample'.value)
        ,"units" => legacy_helpers:symbol_to_string(SS#'SensorSample'.units)
        ,"values" => to_float([BR1, BR2])
    };
to_map(SS) ->
    #{
        "name" => "SensorSample"
        ,"sensor" => legacy_helpers:symbol_to_string(SS#'SensorSample'.sensor)
        ,"time" => legacy_helpers:symbol_to_string(SS#'SensorSample'.time)
        ,"value" => legacy_helpers:symbol_to_string(SS#'SensorSample'.value)
        ,"units" => legacy_helpers:symbol_to_string(SS#'SensorSample'.units)
    }.
%%--------------------------------------------------------------------
%% @doc
%% Convert SensorSample record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'SensorSample'().
to_record(Map) ->
    #'SensorSample'{
        sensor = legacy_helpers:symbol_to_binary(maps:get("sensor", Map))
        ,time = maps:get("time", Map)
        ,value = maps:get("value", Map)
        ,units = legacy_helpers:symbol_to_binary(maps:get("units", Map, 'undefined'), 'undefined')
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(binary(), integer(), any(), any()) -> 'SensorSample'().
create(Sensor, Time, Value, Units) ->
    #'SensorSample'{
        sensor = Sensor
        ,time = Time
        ,value = Value
        ,units = Units
    }.

%%--------------------------------------------------------------------
%% @doc
%% Return routing key
%% @end
%%--------------------------------------------------------------------
-spec key(binary(), any()) -> binary().
key(NodeID, Map) when is_map(Map) ->
    Sensor = legacy_helpers:symbol_to_binary(maps:get("sensor", Map)),
    <<NodeID/binary, ".sensor.", Sensor/binary>>;
key(NodeID, #'SensorSample'{sensor=Sensor}) ->
    <<NodeID/binary, ".sensor.", Sensor/binary>>.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec to_float([integer(), ...]) -> [float(), ...].
to_float(List) ->
    [Int + 0.0 || Int <- List].

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

to_map_test() ->
    LT = #'SensorSample'{
        sensor = <<"l">>
        ,time = 0
        ,value = 1
        ,units = <<"lux">>
    },
    LTMap = #{
        "name" => "SensorSample"
        ,"sensor" => "l"
        ,"time" => 0
        ,"value" => 1
        ,"units" => "lux"
    },
    RF = #'SensorSample'{
        sensor = <<"rf">>
        ,time = 0
        ,value = 18446744073703063472
        ,units = <<>>
    },
    RFMap = #{
        "name" => "SensorSample"
        ,"sensor" => "rf"
        ,"time" => 0
        ,"value" => 18446744073703063472
        ,"units" => []
        ,"values" => [-80.0, -100.0]
    },
    BR = #'SensorSample'{
        sensor = <<"bR">>
        ,time = 0
        ,value = 264
        ,units = <<>>
    },
    BRMap = #{
        "name" => "SensorSample"
        ,"sensor" => "bR"
        ,"time" => 0
        ,"value" => 264
        ,"units" => []
        ,"values" => [8.0, 1.0]
    },
    JT = #'SensorSample'{
        sensor = <<"jt">>
        ,time = 0
        ,value = 4684316664362221708
        ,units = <<>>
    },
    JTMap = #{
        "name" => "SensorSample"
        ,"sensor" => "jt"
        ,"time" => 0
        ,"value" => 4684316664362221708
        ,"units" => []
        ,"values" => [-16244.0, 2752.0, 2352.0, 16642.0]
    },
    ?assertEqual(LTMap, to_map(LT)),
    ?assertEqual(RFMap, to_map(RF)),
    ?assertEqual(BRMap, to_map(BR)),
    ?assertEqual(JTMap, to_map(JT)),
    'ok'.

to_record_test() ->
    SensorSample = #'SensorSample'{
        sensor = <<"l">>
        ,time = 0
        ,value = 1
        ,units = <<"lux">>
    },
    Map = #{
        "name" => "SensorSample"
        ,"sensor" => "l"
        ,"time" => 0
        ,"value" => 1
        ,"units" => "lux"
    },
    ?assertEqual(SensorSample, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'SensorSample'{
        sensor = <<"lt">>
        ,time = 0
        ,value = 12
        ,units = <<"lux">>
    }, create(<<"lt">>, 0, 12, <<"lux">>)).

key_test() ->
    SensorSample = #'SensorSample'{
        sensor = <<"l">>
        ,time = 0
        ,value = 1
        ,units = <<"lux">>
    },
    Map = #{
        "name" => "SensorSample"
        ,"sensor" => "l"
        ,"time" => 0
        ,"value" => 1
        ,"units" => "lux"
    },
    ?assertEqual(<<"J007.sensor.l">>, key(<<"J007">>, SensorSample)),
    ?assertEqual(<<"J007.sensor.l">>, key(<<"J007">>, Map)).

-endif.
