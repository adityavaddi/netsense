%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 3.27.0

-ifndef(messages_pb).
-define(messages_pb, true).

-define(messages_pb_gpb_version, "3.27.0").

-ifndef('KVPAIR_PB_H').
-define('KVPAIR_PB_H', true).
-record('KVPair',
        {key                    :: binary() | iolist(), % = 1
         sValue                 :: binary() | iolist() | undefined, % = 2
         lValue                 :: integer() | undefined, % = 3, 32 bits
         bValue                 :: boolean() | 0 | 1 | undefined % = 4
        }).
-endif.

-ifndef('CONFIGRESP_PB_H').
-define('CONFIGRESP_PB_H', true).
-record('ConfigResp',
        {pair                   :: #'KVPair'{}      % = 1
        }).
-endif.

-ifndef('X509UPDATEREQ_PB_H').
-define('X509UPDATEREQ_PB_H', true).
-record('X509UpdateReq',
        {x509UpdateURL          :: binary() | iolist(), % = 1
         x509Update             :: 'X509ClientOnly' | 'X509ServerOnly' | 'X509Both' | integer(), % = 2, enum X509Update
         httpsPort              :: non_neg_integer() | undefined % = 3, 32 bits
        }).
-endif.

-ifndef('LOGINREQ_PB_H').
-define('LOGINREQ_PB_H', true).
-record('LoginReq',
        {nodeId                 :: binary() | iolist(), % = 1
         protocolVersion        :: non_neg_integer(), % = 2, 32 bits
         clientType             :: binary() | iolist() | undefined, % = 3
         swVerId                :: binary() | iolist() | undefined, % = 4
         netName                :: binary() | iolist() | undefined, % = 5
         profileName            :: binary() | iolist() | undefined, % = 6
         assocChannel           :: binary() | iolist() | undefined, % = 7
         configToken            :: binary() | iolist() | undefined, % = 8
         localIP                :: binary() | iolist() | undefined, % = 9
         time                   :: non_neg_integer() | undefined, % = 10, 32 bits
         bssid                  :: binary() | iolist() | undefined, % = 11
         mac                    :: binary() | iolist() | undefined, % = 12
         auth                   :: 'Other' | 'Open' | 'WEP' | 'WPA2_PSK' | 'WPA2_EAP_TLS' | integer() | undefined, % = 13, enum WiFiAuthType
         clientSubType          :: binary() | iolist() | undefined, % = 14
         modemRevEd             :: binary() | iolist() | undefined % = 15
        }).
-endif.

-ifndef('LIGHTINGCTRL_PB_H').
-define('LIGHTINGCTRL_PB_H', true).
-record('LightingCtrl',
        {pri                    :: non_neg_integer(), % = 1, 32 bits
         mask                   :: non_neg_integer(), % = 2, 32 bits
         level                  :: binary(),        % = 3
         qualifiers             :: non_neg_integer() | undefined % = 4, 32 bits
        }).
-endif.

-ifndef('LIGHTINGCLEARSCHEDULE_PB_H').
-define('LIGHTINGCLEARSCHEDULE_PB_H', true).
-record('LightingClearSchedule',
        {
        }).
-endif.

-ifndef('ASTRONOMICALEVENT_PB_H').
-define('ASTRONOMICALEVENT_PB_H', true).
-record('AstronomicalEvent',
        {body                   :: 'Sun' | 'Moon' | integer(), % = 1, enum AstronomicalBody
         atype                  :: 'Rise' | 'Set' | 'Noon' | integer(), % = 2, enum AstronomicalEventType
         zenith                 :: 'Official' | 'Civil' | 'Nautical' | 'Astronomical' | integer(), % = 3, enum AstronomicalEventZenith
         wday                   :: non_neg_integer() | undefined, % = 4, 32 bits
         mday                   :: non_neg_integer() | undefined, % = 5, 32 bits
         mon                    :: non_neg_integer() | undefined, % = 6, 32 bits
         year                   :: non_neg_integer() | undefined % = 7, 32 bits
        }).
-endif.

-ifndef('LIGHTINGASTRONOMICALEVENT_PB_H').
-define('LIGHTINGASTRONOMICALEVENT_PB_H', true).
-record('LightingAstronomicalEvent',
        {id                     :: non_neg_integer(), % = 1, 32 bits
         event                  :: #'AstronomicalEvent'{}, % = 2
         state                  :: #'LightingCtrl'{} % = 3
        }).
-endif.

-ifndef('CALENDAREVENT_PB_H').
-define('CALENDAREVENT_PB_H', true).
-record('CalendarEvent',
        {sec                    :: non_neg_integer(), % = 1, 32 bits
         min                    :: non_neg_integer(), % = 2, 32 bits
         hr                     :: non_neg_integer(), % = 3, 32 bits
         wday                   :: non_neg_integer() | undefined, % = 4, 32 bits
         mday                   :: non_neg_integer() | undefined, % = 5, 32 bits
         mon                    :: non_neg_integer() | undefined, % = 6, 32 bits
         year                   :: non_neg_integer() | undefined % = 7, 32 bits
        }).
-endif.

-ifndef('LIGHTINGSCHEDULEDEVENT_PB_H').
-define('LIGHTINGSCHEDULEDEVENT_PB_H', true).
-record('LightingScheduledEvent',
        {id                     :: non_neg_integer(), % = 1, 32 bits
         event                  :: #'CalendarEvent'{}, % = 2
         state                  :: #'LightingCtrl'{} % = 3
        }).
-endif.

-ifndef('LIGHTINGSETAUTO_PB_H').
-define('LIGHTINGSETAUTO_PB_H', true).
-record('LightingSetAuto',
        {
        }).
-endif.

-ifndef('LIGHTINGFORCESTATE_PB_H').
-define('LIGHTINGFORCESTATE_PB_H', true).
-record('LightingForceState',
        {state                  :: #'LightingCtrl'{}, % = 1
         ftype                  :: 'Persistent' | 'Volatile' | integer() | undefined % = 2, enum LightingForceType
        }).
-endif.

-ifndef('GPSACTIONRSP_PB_H').
-define('GPSACTIONRSP_PB_H', true).
-record('GpsActionRsp',
        {actionType             :: 'SendSample' | 'SetCtlMode' | 'SetOpSchedule' | 'GetOpSchedule' | integer(), % = 1, enum GpsActionType
         iValue                 :: integer() | undefined, % = 2, 32 bits
         sValue                 :: binary() | iolist() | undefined % = 3
        }).
-endif.

-ifndef('GPSACTIONREQ_PB_H').
-define('GPSACTIONREQ_PB_H', true).
-record('GpsActionReq',
        {actionType             :: 'SendSample' | 'SetCtlMode' | 'SetOpSchedule' | 'GetOpSchedule' | integer(), % = 1, enum GpsActionType
         iValue                 :: integer() | undefined, % = 2, 32 bits
         sValue                 :: binary() | iolist() | undefined % = 3
        }).
-endif.

-ifndef('GPSSAMPLE_PB_H').
-define('GPSSAMPLE_PB_H', true).
-record('GpsSample',
        {gpsVer                 :: non_neg_integer() | undefined, % = 1, 32 bits
         epochSecs              :: non_neg_integer() | undefined, % = 2, 32 bits
         latAndLon              :: non_neg_integer() | undefined, % = 3, 32 bits
         altAndMisc             :: non_neg_integer() | undefined, % = 4, 32 bits
         snrAndMisc             :: non_neg_integer() | undefined % = 5, 32 bits
        }).
-endif.

-ifndef('SENSORSAMPLEREQ_PB_H').
-define('SENSORSAMPLEREQ_PB_H', true).
-record('SensorSampleReq',
        {sensor                 :: binary() | iolist() % = 1
        }).
-endif.

-ifndef('TIMERESP_PB_H').
-define('TIMERESP_PB_H', true).
-record('TimeResp',
        {time                   :: non_neg_integer() % = 1, 32 bits
        }).
-endif.

-ifndef('TIMEREQ_PB_H').
-define('TIMEREQ_PB_H', true).
-record('TimeReq',
        {
        }).
-endif.

-ifndef('VIDEOUPLOADRESP_PB_H').
-define('VIDEOUPLOADRESP_PB_H', true).
-record('VideoUploadResp',
        {sensorId               :: binary() | iolist(), % = 1
         url                    :: binary() | iolist() % = 2
        }).
-endif.

-ifndef('VIDEOUPLOADREQ_PB_H').
-define('VIDEOUPLOADREQ_PB_H', true).
-record('VideoUploadReq',
        {sensorId               :: binary() | iolist(), % = 1
         startTime              :: non_neg_integer() | undefined, % = 2, 32 bits
         endTime                :: non_neg_integer() | undefined, % = 3, 32 bits
         fileType               :: binary() | iolist() | undefined % = 4
        }).
-endif.

-ifndef('SENSORSAMPLE_PB_H').
-define('SENSORSAMPLE_PB_H', true).
-record('SensorSample',
        {sensor                 :: binary() | iolist(), % = 1
         time                   :: non_neg_integer(), % = 2, 32 bits
         value                  :: non_neg_integer(), % = 3, 32 bits
         units                  :: binary() | iolist() | undefined % = 4
        }).
-endif.

-ifndef('DEVICEACTIONREQ_PB_H').
-define('DEVICEACTIONREQ_PB_H', true).
-record('DeviceActionReq',
        {actionType             :: 'ColdReset' | 'ResetFactory' | 'ResetProvisioning' | 'ChangeFWPartition' | integer() % = 1, enum ActionType
        }).
-endif.

-ifndef('DEVICEALARM_PB_H').
-define('DEVICEALARM_PB_H', true).
-record('DeviceAlarm',
        {alarmType              :: 'CommFail' | 'SimFail' | 'NotTested' | 'DownrevSoftware' | 'BadSensorData' | 'ConfigFail' | 'DegradedNetwork' | 'SoftwareUpdateFail' | 'ScheduleFail' | 'PreRuninFail' | 'PostRuninFail' | 'USPFail' | 'PMACFail' | 'DriverFail' | 'FarmUSPFail' | 'SensorFail' | 'StrangeReboot' | 'Assert' | 'X509ClientFail' | 'X509ServerFail' | 'UnderPower' | 'OverPower' | 'HardFault' | 'HWFail_generic' | 'HWFail_HIH6131' | 'HWFail_ISL29023' | 'HWFail_SE95' | 'HWFail_ZMotion' | 'HWFail_MMA8451' | 'HWFail_TSC3414' | 'HWFail_UrbanUSP' | 'HWFail_RTC' | 'HWFail_EEPROM' | 'HWFail_NIGHTHAWK' | 'SWUpdateFail_SENSORPOD' | 'HWFail_STUCK_RELAY' | 'HWFail_PCT2075' | 'HWFAIL_SIHAWK' | 'HWFAIL_GPS' | 'HWFail_PodBus' | 'Epic_Fail' | integer(), % = 1, enum AlarmType
         alarmSeverity          :: 'Clear' | 'Warning' | 'Minor' | 'Major' | 'Critical' | integer(), % = 2, enum AlarmSeverity
         msg                    :: binary() | iolist() | undefined % = 3
        }).
-endif.

-ifndef('SOFTWAREUPDATEREQ_PB_H').
-define('SOFTWAREUPDATEREQ_PB_H', true).
-record('SoftwareUpdateReq',
        {swUpdateURL            :: binary() | iolist(), % = 1
         cellFwUpdateURL        :: binary() | iolist() | undefined, % = 2
         has_cellFwUpdateURL    :: boolean() | 0 | 1 | undefined % = 3
        }).
-endif.

-ifndef('CONFIGRESPDONE_PB_H').
-define('CONFIGRESPDONE_PB_H', true).
-record('ConfigRespDone',
        {
        }).
-endif.

-ifndef('UNUSEDCONFIGREQ_PB_H').
-define('UNUSEDCONFIGREQ_PB_H', true).
-record('UnusedConfigReq',
        {
        }).
-endif.

-ifndef('LOGINRESP_PB_H').
-define('LOGINRESP_PB_H', true).
-record('LoginResp',
        {okay                   :: boolean() | 0 | 1, % = 1
         time                   :: non_neg_integer() | undefined % = 2, 32 bits
        }).
-endif.

-ifndef('ENVELOPE_PB_H').
-define('ENVELOPE_PB_H', true).
-record('Envelope',
        {loginReq               :: #'LoginReq'{} | undefined, % = 1
         loginResp              :: #'LoginResp'{} | undefined, % = 2
         unusedConfigReq        :: #'UnusedConfigReq'{} | undefined, % = 3
         configResp             :: #'ConfigResp'{} | undefined, % = 4
         configRespDone         :: #'ConfigRespDone'{} | undefined, % = 5
         softwareUpdateReq      :: #'SoftwareUpdateReq'{} | undefined, % = 6
         deviceAlarm            :: #'DeviceAlarm'{} | undefined, % = 7
         deviceAction           :: #'DeviceActionReq'{} | undefined, % = 8
         x509Update             :: #'X509UpdateReq'{} | undefined, % = 9
         sensorSample           :: #'SensorSample'{} | undefined, % = 10
         videoUploadReq         :: #'VideoUploadReq'{} | undefined, % = 11
         videoUploadResp        :: #'VideoUploadResp'{} | undefined, % = 12
         timeReq                :: #'TimeReq'{} | undefined, % = 13
         timeResp               :: #'TimeResp'{} | undefined, % = 14
         sensorSampleReq        :: #'SensorSampleReq'{} | undefined, % = 15
         gpsSample              :: #'GpsSample'{} | undefined, % = 16
         gpsActionReq           :: #'GpsActionReq'{} | undefined, % = 17
         gpsActionRsp           :: #'GpsActionRsp'{} | undefined, % = 18
         lightingForceState     :: #'LightingForceState'{} | undefined, % = 20
         lightingSetAuto        :: #'LightingSetAuto'{} | undefined, % = 21
         lightingScheduledEvent :: #'LightingScheduledEvent'{} | undefined, % = 22
         lightingAstronomicalEvent :: #'LightingAstronomicalEvent'{} | undefined, % = 23
         lightingClearSchedule  :: #'LightingClearSchedule'{} | undefined % = 24
        }).
-endif.

-ifndef('ERRORRESP_PB_H').
-define('ERRORRESP_PB_H', true).
-record('ErrorResp',
        {message                :: binary() | iolist() % = 1
        }).
-endif.

-endif.
