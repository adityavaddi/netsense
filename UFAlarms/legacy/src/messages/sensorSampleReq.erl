%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Sensor Sample Request ==
%% Sensor Sample Request message API
%% @end
%%%-------------------------------------------------------------------
-module(sensorSampleReq).

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

-type 'SensorSampleReq'() :: #'SensorSampleReq'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert SensorSampleReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('SensorSampleReq'()) -> map().
to_map(SSR) ->
    #{
        "name" => "SensorSampleReq"
        ,"sensor" => legacy_helpers:symbol_to_string(SSR#'SensorSampleReq'.sensor)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert SensorSampleReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'SensorSampleReq'().
to_record(Map) ->
    #'SensorSampleReq'{
        sensor = legacy_helpers:symbol_to_binary(maps:get("sensor", Map))
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(binary()) -> 'SensorSampleReq'().
create(Sensor) ->
    #'SensorSampleReq'{sensor = Sensor}.

%%--------------------------------------------------------------------
%% @doc
%% Return routing key
%% @end
%%--------------------------------------------------------------------
-spec key(binary(), any()) -> binary().
key(_NodeID, _) -> <<>>.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

to_map_test() ->
    SensorSampleReq = #'SensorSampleReq'{sensor = <<"l">>},
    Map = #{"name" => "SensorSampleReq", "sensor" => "l"},
    ?assertEqual(Map, to_map(SensorSampleReq)),
    'ok'.

to_record_test() ->
        SensorSampleReq = #'SensorSampleReq'{sensor = <<"l">>},
        Map = #{"name" => "SensorSampleReq", "sensor" => "l"},
    ?assertEqual(SensorSampleReq, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'SensorSampleReq'{sensor = <<"l">>}, create(<<"l">>)).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
