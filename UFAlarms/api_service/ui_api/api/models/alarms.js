/**
 * Request payloads of all alarms APIs.
 */
"use strict";
// Export all the functions to use it in controller
module.exports = {
    getAllUFAlarmsReq,
    getUFAlarmReq,
    createUFAlarmReq,
    createBulkUFAlarmsReq,
    updateUFAlarmReq,
    deleteUFAlarmReq,
    resetUFAlarmReq
}

// refer main.conf.js for the actual request topic name
const alarmsReqTopicKey = "alarmrequest";

function getAllUFAlarmsReq(reqid) {
    return {
        requestid: reqid,
        type: 'getAllUFAlarms',
        model: 'UFAlarmModel',
        action: 'CAN_READ',
        service: alarmsReqTopicKey
    }
}

function getUFAlarmReq(reqid, mappingid) {
    return {
        requestid: reqid,
        type: 'getUFAlarm',
        model: 'UFAlarmModel',
        action: 'CAN_READ',
        ufalarmprops: {
            mappingid: mappingid
        },
        service: alarmsReqTopicKey
    }
}

function createUFAlarmReq(reqid, alarm) {
    return {
        requestid: reqid,
        type: 'createUFAlarm',
        model: 'UFAlarmModel',
        action: 'CAN_CREATE',
        ufalarmprops: alarm,
        service: alarmsReqTopicKey
    }
}

function createBulkUFAlarmsReq(reqid, ufalarmslist) {
    return {
        requestid: reqid,
        type: 'createBulkUFAlarms',
        model: 'UFAlarmModel',
        action: 'CAN_CREATE',
        ufalarmprops: {
            ufalarmslist: ufalarmslist
        },
        service: alarmsReqTopicKey
    }
}

function updateUFAlarmReq(reqid, alarm) {
    return {
        requestid: reqid,
        type: 'updateUFAlarm',
        model: 'UFAlarmModel',
        action: 'CAN_UPDATE',
        ufalarmprops: alarm,
        service: alarmsReqTopicKey
    }
}

function deleteUFAlarmReq(reqid, mappingid) {
    return {
        requestid: reqid,
        type: 'deleteUFAlarm',
        model: 'UFAlarmModel',
        action: 'CAN_DELETE',
        ufalarmprops: {
            mappingid: mappingid
        },
        service: alarmsReqTopicKey
    }
}

function resetUFAlarmReq(reqid, mappingid) {
    return {
        requestid: reqid,
        type: 'resetUFAlarm',
        model: 'UFAlarmModel',
        action: 'CAN_UPDATE',
        ufalarmprops: {
            mappingid: mappingid
        },
        service: alarmsReqTopicKey
    }
}
