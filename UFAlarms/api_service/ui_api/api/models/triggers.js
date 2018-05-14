/**
 * Request payloads of all Trigger APIs.
 */
'use strict';
// Export all the functions to use it in controller
module.exports = {
    getAllTriggersReq,
    getAllFilteredTriggersReq,
    getTriggerReq,
    createTriggerReq,
    updateTriggerReq,
    deleteTriggerReq
}

// refer main.conf.js for the actual request topic name
const parkingTriggerReqTopicKey = "triggerrequest";

// TODO -- Request models for all APIs has to be modified accordingly when doing actual implementation
function getAllTriggersReq(reqid, userid, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'getAllTriggers',
        model: 'TriggerModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: parkingTriggerReqTopicKey
    }
}

function getAllFilteredTriggersReq(reqid, userid, filterQuery, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'filterTrigger',
        model: 'TriggerModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        }, 
        triggerprops: {
            search: filterQuery
        },       
        service: parkingTriggerReqTopicKey
    }
}

function getTriggerReq(reqid, userid, triggerid, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'getTrigger',
        model: 'TriggerModel',
        action: 'CAN_READ',
        user: userid,
        triggerprops: {
            triggerId: triggerid
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: parkingTriggerReqTopicKey
    }
}

function createTriggerReq(reqid, userid, trigger, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'createTrigger',
        model: 'TriggerModel',
        action: 'CAN_CREATE',
        user: userid,
        triggerprops: {
            trigger: trigger
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: parkingTriggerReqTopicKey
    }
}

function updateTriggerReq(reqid, userid, trigger, orgid, siteid,triggerid) {
    return {
        requestid: reqid,
        type: 'updateTrigger',
        model: 'TriggerModel',
        action: 'CAN_UPDATE',
        user: userid,
        triggerprops: {
            triggerId: triggerid,
            trigger: trigger
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: parkingTriggerReqTopicKey
    }
}

function deleteTriggerReq(reqid, userid, triggerid, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'deleteTrigger',
        model: 'TriggerModel',
        action: 'CAN_DELETE',
        user: userid,
        triggerprops: {
            triggerId: triggerid
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: parkingTriggerReqTopicKey
    }
}