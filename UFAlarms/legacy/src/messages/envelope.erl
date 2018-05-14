%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Envelope ==
%% Envelope message API
%% @end
%%%-------------------------------------------------------------------
-module(envelope).

-export([
    decode/1
    ,encode/1
    ,create/1
    ,type/1
    ,str_type_to_atom/1
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'Envelope'() :: #'Envelope'{}.

-export_type(['Envelope'/0]).

%%--------------------------------------------------------------------
%% @doc
%% Decode envelope binary to record
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> 'Envelope'().
decode(Binary) when is_binary(Binary) ->
    'messages_pb':decode_msg(Binary, 'Envelope').

%%--------------------------------------------------------------------
%% @doc
%% Encode envelope record to binary
%% @end
%%--------------------------------------------------------------------
-spec encode('Envelope'()) -> binary().
encode(#'Envelope'{}=Envelope) ->
    'messages_pb':encode_msg(Envelope).

%%--------------------------------------------------------------------
%% @doc
%% Create envelope from message
%% @end
%%--------------------------------------------------------------------
-spec create(any()) -> 'Envelope'().
create(#'LoginReq'{}=Msg) ->
    #'Envelope'{loginReq=Msg};
create(#'LoginResp'{}=Msg) ->
    #'Envelope'{loginResp=Msg};
create(#'UnusedConfigReq'{}=Msg) ->
    #'Envelope'{unusedConfigReq=Msg};
create(#'ConfigResp'{}=Msg) ->
    #'Envelope'{configResp=Msg};
create(#'ConfigRespDone'{}=Msg) ->
    #'Envelope'{configRespDone=Msg};
create(#'SoftwareUpdateReq'{}=Msg) ->
    #'Envelope'{softwareUpdateReq=Msg};
create(#'DeviceAlarm'{}=Msg) ->
    #'Envelope'{deviceAlarm=Msg};
create(#'DeviceActionReq'{}=Msg) ->
    #'Envelope'{deviceAction=Msg};
create(#'X509UpdateReq'{}=Msg) ->
    #'Envelope'{x509Update=Msg};
create(#'SensorSample'{}=Msg) ->
    #'Envelope'{sensorSample=Msg};
create(#'VideoUploadReq'{}=Msg) ->
    #'Envelope'{videoUploadReq=Msg};
create(#'VideoUploadResp'{}=Msg) ->
    #'Envelope'{videoUploadResp=Msg};
create(#'TimeReq'{}=Msg) ->
    #'Envelope'{timeReq=Msg};
create(#'TimeResp'{}=Msg) ->
    #'Envelope'{timeResp=Msg};
create(#'SensorSampleReq'{}=Msg) ->
    #'Envelope'{sensorSampleReq=Msg};
create(#'GpsSample'{}=Msg) ->
    #'Envelope'{gpsSample=Msg};
create(#'GpsActionReq'{}=Msg) ->
    #'Envelope'{gpsActionReq=Msg};
create(#'GpsActionRsp'{}=Msg) ->
    #'Envelope'{gpsActionRsp=Msg};
create(#'LightingForceState'{}=Msg) ->
    #'Envelope'{lightingForceState=Msg};
create(#'LightingSetAuto'{}=Msg) ->
    #'Envelope'{lightingSetAuto=Msg};
create(#'LightingScheduledEvent'{}=Msg) ->
    #'Envelope'{lightingScheduledEvent=Msg};
create(#'LightingAstronomicalEvent'{}=Msg) ->
    #'Envelope'{lightingAstronomicalEvent=Msg};
create(#'LightingClearSchedule'{}=Msg) ->
    #'Envelope'{lightingClearSchedule=Msg}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec type('Envelope'()) -> {atom(), any()}.
type(#'Envelope'{loginReq=#'LoginReq'{}}=E) ->
    {'loginReq', E#'Envelope'.loginReq};
type(#'Envelope'{loginResp=#'LoginResp'{}}=E) ->
    {'loginResp', E#'Envelope'.loginResp};
type(#'Envelope'{unusedConfigReq=#'UnusedConfigReq'{}}=E) ->
    {'unusedConfigReq', E#'Envelope'.unusedConfigReq};
type(#'Envelope'{configResp=#'ConfigResp'{}}=E) ->
    {'configResp', E#'Envelope'.configResp};
type(#'Envelope'{configRespDone=#'ConfigRespDone'{}}=E) ->
    {'configRespDone', E#'Envelope'.configRespDone};
type(#'Envelope'{softwareUpdateReq=#'SoftwareUpdateReq'{}}=E) ->
    {'softwareUpdateReq', E#'Envelope'.softwareUpdateReq};
type(#'Envelope'{deviceAlarm=#'DeviceAlarm'{}}=E) ->
    {'deviceAlarm', E#'Envelope'.deviceAlarm};
type(#'Envelope'{deviceAction=#'DeviceActionReq'{}}=E) ->
    {'deviceActionReq', E#'Envelope'.deviceAction};
type(#'Envelope'{x509Update=#'X509UpdateReq'{}}=E) ->
    {'x509UpdateReq', E#'Envelope'.x509Update};
type(#'Envelope'{sensorSample=#'SensorSample'{}}=E) ->
    {'sensorSample', E#'Envelope'.sensorSample};
type(#'Envelope'{videoUploadReq=#'VideoUploadReq'{}}=E) ->
    {'videoUploadReq', E#'Envelope'.videoUploadReq};
type(#'Envelope'{videoUploadResp=#'VideoUploadResp'{}}=E) ->
    {'videoUploadResp', E#'Envelope'.videoUploadResp};
type(#'Envelope'{timeReq=#'TimeReq'{}}=E) ->
    {'timeReq', E#'Envelope'.timeReq};
type(#'Envelope'{timeResp=#'TimeResp'{}}=E) ->
    {'timeResp', E#'Envelope'.timeResp};
type(#'Envelope'{sensorSampleReq=#'SensorSampleReq'{}}=E) ->
    {'sensorSampleReq', E#'Envelope'.sensorSampleReq};
type(#'Envelope'{gpsSample=#'GpsSample'{}}=E) ->
    {'gpsSample', E#'Envelope'.gpsSample};
type(#'Envelope'{gpsActionReq=#'GpsActionReq'{}}=E) ->
    {'gpsActionReq', E#'Envelope'.gpsActionReq};
type(#'Envelope'{gpsActionRsp=#'GpsActionRsp'{}}=E) ->
    {'gpsActionRsp', E#'Envelope'.gpsActionRsp};
type(#'Envelope'{lightingForceState=#'LightingForceState'{}}=E) ->
    {'lightingForceState', E#'Envelope'.lightingForceState};
type(#'Envelope'{lightingSetAuto=#'LightingSetAuto'{}}=E) ->
    {'lightingSetAuto', E#'Envelope'.lightingSetAuto};
type(#'Envelope'{lightingScheduledEvent=#'LightingScheduledEvent'{}}=E) ->
    {'lightingScheduledEvent', E#'Envelope'.lightingScheduledEvent};
type(#'Envelope'{lightingAstronomicalEvent=#'LightingAstronomicalEvent'{}}=E) ->
    {'lightingAstronomicalEvent', E#'Envelope'.lightingAstronomicalEvent};
type(#'Envelope'{lightingClearSchedule=#'LightingClearSchedule'{}}=E) ->
    {'lightingClearSchedule', E#'Envelope'.lightingClearSchedule}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec str_type_to_atom(string()) -> atom().
str_type_to_atom(String) ->
    F = string:substr(String, 1, 1),
    Rest = string:substr(String, 2, string:len(String)),
    Corrected = string:to_lower(F) ++ Rest,
    legacy_helpers:symbol_to_atom(Corrected).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

decode_encode_test() ->
    Encoded = encode(#'Envelope'{}),
    ?assertEqual(#'Envelope'{}, decode(Encoded)).

create_test() ->
    ?assertEqual(#'Envelope'{loginReq=#'LoginReq'{}}, create(#'LoginReq'{})),
    ?assertEqual(#'Envelope'{loginResp=#'LoginResp'{}}, create(#'LoginResp'{})),
    ?assertEqual(#'Envelope'{unusedConfigReq=#'UnusedConfigReq'{}}, create(#'UnusedConfigReq'{})),
    ?assertEqual(#'Envelope'{configResp=#'ConfigResp'{}}, create(#'ConfigResp'{})),
    ?assertEqual(#'Envelope'{configRespDone=#'ConfigRespDone'{}}, create(#'ConfigRespDone'{})),
    ?assertEqual(#'Envelope'{softwareUpdateReq=#'SoftwareUpdateReq'{}}, create(#'SoftwareUpdateReq'{})),
    ?assertEqual(#'Envelope'{deviceAlarm=#'DeviceAlarm'{}}, create(#'DeviceAlarm'{})),
    ?assertEqual(#'Envelope'{deviceAction=#'DeviceActionReq'{}}, create(#'DeviceActionReq'{})),
    ?assertEqual(#'Envelope'{x509Update=#'X509UpdateReq'{}}, create(#'X509UpdateReq'{})),
    ?assertEqual(#'Envelope'{sensorSample=#'SensorSample'{}}, create(#'SensorSample'{})),
    ?assertEqual(#'Envelope'{videoUploadReq=#'VideoUploadReq'{}}, create(#'VideoUploadReq'{})),
    ?assertEqual(#'Envelope'{videoUploadResp=#'VideoUploadResp'{}}, create(#'VideoUploadResp'{})),
    ?assertEqual(#'Envelope'{timeReq=#'TimeReq'{}}, create(#'TimeReq'{})),
    ?assertEqual(#'Envelope'{timeResp=#'TimeResp'{}}, create(#'TimeResp'{})),
    ?assertEqual(#'Envelope'{sensorSampleReq=#'SensorSampleReq'{}}, create(#'SensorSampleReq'{})),
    ?assertEqual(#'Envelope'{gpsSample=#'GpsSample'{}}, create(#'GpsSample'{})),
    ?assertEqual(#'Envelope'{gpsActionReq=#'GpsActionReq'{}}, create(#'GpsActionReq'{})),
    ?assertEqual(#'Envelope'{gpsActionRsp=#'GpsActionRsp'{}}, create(#'GpsActionRsp'{})),
    ?assertEqual(#'Envelope'{lightingForceState=#'LightingForceState'{}}, create(#'LightingForceState'{})),
    ?assertEqual(#'Envelope'{lightingSetAuto=#'LightingSetAuto'{}}, create(#'LightingSetAuto'{})),
    ?assertEqual(#'Envelope'{lightingScheduledEvent=#'LightingScheduledEvent'{}}, create(#'LightingScheduledEvent'{})),
    ?assertEqual(#'Envelope'{lightingAstronomicalEvent=#'LightingAstronomicalEvent'{}}, create(#'LightingAstronomicalEvent'{})),
    ?assertEqual(#'Envelope'{lightingClearSchedule=#'LightingClearSchedule'{}}, create(#'LightingClearSchedule'{})),

    ?assertError('function_clause', create(#'Envelope'{})),
    ?assertError('function_clause', create("random stuff")).

type_test() ->
    ?assertEqual({'loginReq', #'LoginReq'{}}, type(#'Envelope'{loginReq=#'LoginReq'{}})),
    ?assertEqual({'loginResp', #'LoginResp'{}}, type(#'Envelope'{loginResp=#'LoginResp'{}})),
    ?assertEqual({'unusedConfigReq', #'UnusedConfigReq'{}}, type(#'Envelope'{unusedConfigReq=#'UnusedConfigReq'{}})),
    ?assertEqual({'configResp', #'ConfigResp'{}}, type(#'Envelope'{configResp=#'ConfigResp'{}})),
    ?assertEqual({'configRespDone', #'ConfigRespDone'{}}, type(#'Envelope'{configRespDone=#'ConfigRespDone'{}})),
    ?assertEqual({'softwareUpdateReq', #'SoftwareUpdateReq'{}}, type(#'Envelope'{softwareUpdateReq=#'SoftwareUpdateReq'{}})),
    ?assertEqual({'deviceAlarm', #'DeviceAlarm'{}}, type(#'Envelope'{deviceAlarm=#'DeviceAlarm'{}})),
    ?assertEqual({'deviceActionReq', #'DeviceActionReq'{}}, type(#'Envelope'{deviceAction=#'DeviceActionReq'{}})),
    ?assertEqual({'x509UpdateReq', #'X509UpdateReq'{}}, type(#'Envelope'{x509Update=#'X509UpdateReq'{}})),
    ?assertEqual({'sensorSample', #'SensorSample'{}}, type(#'Envelope'{sensorSample=#'SensorSample'{}})),
    ?assertEqual({'videoUploadReq', #'VideoUploadReq'{}}, type(#'Envelope'{videoUploadReq=#'VideoUploadReq'{}})),
    ?assertEqual({'videoUploadResp', #'VideoUploadResp'{}}, type(#'Envelope'{videoUploadResp=#'VideoUploadResp'{}})),
    ?assertEqual({'timeReq', #'TimeReq'{}}, type(#'Envelope'{timeReq=#'TimeReq'{}})),
    ?assertEqual({'timeResp', #'TimeResp'{}}, type(#'Envelope'{timeResp=#'TimeResp'{}})),
    ?assertEqual({'sensorSampleReq', #'SensorSampleReq'{}}, type(#'Envelope'{sensorSampleReq=#'SensorSampleReq'{}})),
    ?assertEqual({'gpsSample', #'GpsSample'{}}, type(#'Envelope'{gpsSample=#'GpsSample'{}})),
    ?assertEqual({'gpsActionReq', #'GpsActionReq'{}}, type(#'Envelope'{gpsActionReq=#'GpsActionReq'{}})),
    ?assertEqual({'gpsActionRsp', #'GpsActionRsp'{}}, type(#'Envelope'{gpsActionRsp=#'GpsActionRsp'{}})),
    ?assertEqual({'lightingForceState', #'LightingForceState'{}}, type(#'Envelope'{lightingForceState=#'LightingForceState'{}})),
    ?assertEqual({'lightingSetAuto', #'LightingSetAuto'{}}, type(#'Envelope'{lightingSetAuto=#'LightingSetAuto'{}})),
    ?assertEqual({'lightingScheduledEvent', #'LightingScheduledEvent'{}}, type(#'Envelope'{lightingScheduledEvent=#'LightingScheduledEvent'{}})),
    ?assertEqual({'lightingAstronomicalEvent', #'LightingAstronomicalEvent'{}}, type(#'Envelope'{lightingAstronomicalEvent=#'LightingAstronomicalEvent'{}})),
    ?assertEqual({'lightingClearSchedule', #'LightingClearSchedule'{}}, type(#'Envelope'{lightingClearSchedule=#'LightingClearSchedule'{}})),

    ?assertError('function_clause', type(#'Envelope'{})),
    ?assertError('function_clause', type("random stuff")).

-endif.
