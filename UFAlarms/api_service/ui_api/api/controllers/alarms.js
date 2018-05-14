'use strict';

const encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid'),
    alarmsModel = require('./../models/alarms.js'),
    response = require('./../helpers/response.js'),
    helpers = require('./../helpers/helpers.js');

// Export all the functions to use it in swagger.yaml
module.exports = {
    getAllUFAlarms,
    getUFAlarm,
    createUFAlarm,
    createBulkUFAlarms,
    updateUFAlarm,
    deleteUFAlarm,
    resetUFAlarm    
};

// GET /manage/alarms
function getAllUFAlarms(req, res) {    
    const reqPayload = alarmsModel.getAllUFAlarmsReq(req.request_id);
    sendRequest(reqPayload, req, res);
}

// GET /manage/alarms/{mappingid}
function getUFAlarm(req, res) {
    const mappingid = req.swagger.params.mappingid.value;
    const reqPayload = alarmsModel.getUFAlarmReq(req.request_id, mappingid);
    sendRequest(reqPayload, req, res);
}

// POST /manage/alarms
function createUFAlarm(req, res) {
    const id = uuid.v1();
    let alarm = req.body;
    const result = validateAlarm(alarm, false);
    if (result.error) return response.Done(result, null, res, req);
    else alarm = result;
    alarm.mappingid = id;
    const reqPayload = alarmsModel.createUFAlarmReq(req.request_id, alarm);
    sendRequest(reqPayload, req, res);
}

// POST /manage/bulkalarms
function createBulkUFAlarms(req, res) {
    /* // For CSV file upload
    let data = null, alarms = [];
    if(req.files) {
        data = req.files[0].buffer.toString('utf-8');
    } else {
        data = req.body.csvAlarmList;        
    }

    let alarmsArray = data.split(/\r\n|\r|\n/g),
        alarmArray,
        alarm,
        alarmAttributes = alarmsArray[0].split(",");
    for (var i = 1; i < alarmsArray.length; i++) {
        alarmArray = alarmsArray[i].split(",");
        alarm = {};
        for (var j = 0; j < alarmAttributes.length; j++) {
            if(alarmArray[j]) {
                if(alarmAttributes[j] === 'nodemodels') {
                    let nodeModels = alarmArray[j];
                    if(nodeModels) {
                        // remove first `[` and last element `]`
                        if(nodeModels.includes('[')) nodeModels = nodeModels.substring(1, nodeModels.length-1);
                        nodeModels = nodeModels.split(';'); // to form array of strings
                        alarm[alarmAttributes[j]] = nodeModels;
                    }
                } else  {           
                    alarm[alarmAttributes[j]] = alarmArray[j];
                }
                alarm['mappingid'] = uuid.v1();                
            }           
        }
        if(alarm['mappingid']) alarms.push(alarm);    
    }
    const reqPayload = alarmsModel.createBulkUFAlarmsReq(req.request_id, alarms);*/
    let alarms = [];
    if(req.files) {
        const data = req.files[0].buffer.toString('utf-8');
        try {
            alarms = JSON.parse(data);
            alarms = Array.isArray(alarms) ? alarms : [alarms];
            if(alarms.length) {
                for (let i = 0; i < alarms.length; i++){
                    const result = addMappingIdToAlarm(alarms[i]);
                    if(result.error) response.Done(result, null, res, req);
                    else alarms[i] = result;
                }                
            }
        } catch (e) {
            response.Done({ error: true, message: 'Provided file does not contain a valid JSON', status: 400 } , null, res, req);
        }
    } else {    
        alarms = req.body;        
        for (let i = 0; i < alarms.length; i++){
            const result = addMappingIdToAlarm(alarms[i]);
            if(result.error) response.Done(result, null, res, req);
            else alarms[i] = result;
        }       
    }   

    const reqPayload = alarmsModel.createBulkUFAlarmsReq(req.request_id, alarms);
    sendRequest(reqPayload, req, res);
}

// PUT /manage/alarms/{mappingid}
function updateUFAlarm(req, res) {
    let alarm = req.body;
    const result = validateAlarm(alarm, true);
    if (result.error) return response.Done(result, null, res, req);
    else alarm = result;
    alarm.mappingid = req.swagger.params.mappingid.value;
    const reqPayload = alarmsModel.updateUFAlarmReq(req.request_id, alarm);
    sendRequest(reqPayload, req, res);
}

// DELETE /manage/alarms/{mappingid}
function deleteUFAlarm(req, res) {
    const mappingid = req.swagger.params.mappingid.value;
    const reqPayload = alarmsModel.deleteUFAlarmReq(req.request_id, mappingid);
    sendRequest(reqPayload, req, res);
}

// PUT /manage/alarms/{mappingid}/reset
function resetUFAlarm(req, res) {
    const mappingid = req.swagger.params.mappingid.value;
    const reqPayload = alarmsModel.resetUFAlarmReq(req.request_id, mappingid);
    sendRequest(reqPayload, req, res);
}

/**
 * Add mapping id to alarm for createBulkAlarms
 * @param {*} alarm 
 */
function addMappingIdToAlarm(alarm) {   
    let result = validateAlarm(alarm, false);
    if (result.error) return result;
    else result.mappingid = uuid.v1();
    return result;
}

/*
 * Check for required fields in alarm payload & 
 * set default values 
 * @param {*} alarm 
 * @param {*} isUpdate - 'true' if it's for update ufalarm api
 */
function validateAlarm(alarm, isUpdate) {
    let msg = null;
    if(!isUpdate && !alarm.alarmtype) msg = `Required field 'alarmtype' is missing in the request`;
    if(!alarm.nodemodels) alarm.nodemodels=[];
    if(!alarm.ufname) msg = `Required field 'ufname' is missing in the request`;
    if(!alarm.displaytopartner) alarm.displaytopartner = false;
    if(!alarm.displaytocustomer) alarm.displaytocustomer = false;
    
    if(msg) return { error: true, msg: msg, status: 400 };
    else return alarm;
}

/**
 * Common function for all above functions
 * @param {*} user - logged in user
 * @param {*} reqPayload - request payload
 * @param {*} req - request
 * @param {*} res - response
 */
function sendRequest(reqPayload, req, res) {
    encoder.encodeMSRequest(req.getCurrentUser(), reqPayload, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}