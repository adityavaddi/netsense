%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == GPS Sample ==
%% GPS Sample message API
%% @end
%%%-------------------------------------------------------------------
-module(gpsSample).

-export([
    to_map/1
    ,to_record/1
    ,create/0
    ,key/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'GpsSample'() :: #'GpsSample'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert GpsSample record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('GpsSample'()) -> map().
to_map(GPSS) ->
    GpsVer = GPSS#'GpsSample'.gpsVer,
    EpochSecs = GPSS#'GpsSample'.epochSecs,

    LatLong = GPSS#'GpsSample'.latAndLon,
    <<Lat:32/signed-integer, Lon:32/signed-integer>> = <<LatLong:64/integer>>,

    AltAndMisc = GPSS#'GpsSample'.altAndMisc,
    <<Alt:20/signed-integer, Spare:5, RcvStatus:7>> = <<AltAndMisc:32/integer>>,

    SnrAndMisc = GPSS#'GpsSample'.snrAndMisc,
    <<SNR:20, PDOP:12, HDOP:12,VDOP:12, FixType:2, CtrlMode:1, NumSatellite:5>> = <<SnrAndMisc:64/integer>>,
    Snrmin = (SNR bsr 44) band 32#FFFFF,
    Snravg = (Snrmin div 100) rem 100,
    Snrmax = (Snrmin div 10000),
    Snrmin2 = Snrmin rem 100,
    #{
        "name" => "GpsSample"
        ,"gpsver" => legacy_helpers:symbol_to_string(GpsVer)
        ,"epochsecs" => EpochSecs
        ,"lat" => Lat * 0.0000001
        ,"lon" => Lon * 0.0000001
        ,"altAndMisc" => AltAndMisc
        ,"snrAndMisc" => SnrAndMisc
        ,"snrmin" => Snrmin2
        ,"snravg" => Snravg
        ,"snrmax" => Snrmax
        ,"altitude" => Alt * 0.1
        ,"spare" => Spare
        ,"rs" => case RcvStatus of
            65 -> "A";
            _ -> "V"
        end
        ,"snr" => SNR
        ,"pdop" => PDOP * 0.01
        ,"hdop" => HDOP * 0.01
        ,"vdop" => VDOP * 0.01
        ,"fixtype" => FixType
        ,"ctrlmode" => CtrlMode
        ,"numsatellite" => NumSatellite
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert GpsSample record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'GpsSample'().
to_record(_Map) ->
    #'GpsSample'{}.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create() -> 'GpsSample'().
create() ->
    #'GpsSample'{}.

%%--------------------------------------------------------------------
%% @doc
%% Return routing key
%% @end
%%--------------------------------------------------------------------
-spec key(binary(), 'GpsSample'() | map()) -> binary().
key(NodeID, Map) when is_map(Map) ->
    <<NodeID/binary, ".gps.sample">>;
key(NodeID, #'GpsSample'{}) ->
    <<NodeID/binary, ".gps.sample">>.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

create_test() ->
    ?assertMatch(#'GpsSample'{}, create()).

key_test() ->
    ?assertEqual(<<"J007.gps.sample">>, key(<<"J007">>, #{})),
    ?assertEqual(<<"J007.gps.sample">>, key(<<"J007">>, #'GpsSample'{})).

-endif.
