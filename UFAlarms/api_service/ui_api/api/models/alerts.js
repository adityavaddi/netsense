/**
 * Request payloads of all alerts APIs.
 */
"use strict";
// Export all the functions to use it in controller
module.exports = {
    getAllAlertsReq,
    getAlertsForNodeReq,
    getAlertReq,
    deleteAlertReq,
    dismissAlertReq,
    createAlertReq,
    updateAlertReq,
    getAlertSysReq
}

// refer main.conf.js for the actual request topic name
const alarmReqTopicKey = "alarmrequest";

function getAllAlertsReq(reqid, userid, orgid, siteid, usertype) {
    return {
        requestid: reqid,
        type: 'getAllAlerts',
        model: 'AlertModel',
        action: 'CAN_READ',
        user: userid,
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        userprops: {
            usertype: usertype
        },
        service: alarmReqTopicKey
    }
}

function getAlertsForNodeReq(reqid, userid, orgid, siteid, nodeid, usertype) {
    return {
        requestid: reqid,
        type: 'getAlertsForNode',
        model: 'AlertModel',
        action: 'CAN_READ',
        user: userid,
        nodeprops: {
            nodeid: nodeid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        userprops: {
            usertype: usertype
        },
        service: alarmReqTopicKey
    }
}

function getAlertReq(reqid, userid, alertid, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'getAlert',
        model: 'AlertModel',
        action: 'CAN_READ',
        user: userid,
        alertprops: {
            alertid: alertid
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: alarmReqTopicKey
    }
}

function deleteAlertReq(reqid, userid, alertid, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'deleteAlert',
        model: 'AlertModel',
        action: 'CAN_DELETE',
        user: userid,
        alertprops: {
            alertid: alertid
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: alarmReqTopicKey
    }
}

function dismissAlertReq(reqid, userid, alertid, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'dismissAlert',
        model: 'AlertModel',
        action: 'CAN_DELETE',
        user: userid,
        alertprops: {
            alertid: alertid
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: alarmReqTopicKey
    }
}

function createAlertReq(reqid, userid, alert, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'createAlert',
        model: 'AlertModel',
        action: 'CAN_CREATE',
        user: userid,
        alertprops: alert,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: alarmReqTopicKey
    }
}

function updateAlertReq(reqid, userid, alert, orgid, siteid, callback) {
    return {
        requestid: reqid,
        type: 'updateAlert',
        model: 'AlertModel',
        action: 'CAN_UPDATE',
        user: userid,
        alertprops: alert,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: alarmReqTopicKey
    }
}

/**
 * Used for SMS/email feature. Refer /encoder/Encoder.js
 * @param {*} userid 
 * @param {*} alertid 
 * @param {*} siteid 
 */
function getAlertSysReq(userid, alertid, orgid, siteid) {
    return {
        type: 'getAlertSys',
        model: 'AlertModel',
        action: 'CAN_READ',
        userid: 'root',
        alertprops: {
            alertid: alertid
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: alarmReqTopicKey
    }
}