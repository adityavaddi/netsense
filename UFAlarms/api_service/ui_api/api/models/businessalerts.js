/**
 * Request payloads of all Business Alerts APIs
 */
'use strict';
// Export all the functions to use it in controller
module.exports = {
    getAllBusinessAlertsReq,
    getAllFilteredBusinessAlertsReq,
    getBusinessAlertReq,
    createBusinessAlertReq,
    updateBusinessAlertReq,
    deleteOrDismissBusinessAlertReq,
    getBusinessAlertSysReq
}

// refer main.conf.js for the actual request topic name
const businessAlertsReqTopicKey = "businessalertrequest";

function getAllBusinessAlertsReq(reqid, userid, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'getAllBusinessAlerts',
        model: 'BusinessAlertModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },        
        service: businessAlertsReqTopicKey
    }
}

function getAllFilteredBusinessAlertsReq(reqid, userid, filterQuery, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'filterBusinessAlert',
        model: 'BusinessAlertModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        }, 
        businessalertprops: {
            search: filterQuery
        },       
        service: businessAlertsReqTopicKey
    }
}

function getBusinessAlertReq(reqid, userid, businessAlertId, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'getBusinessAlert',
        model: 'BusinessAlertModel',
        action: 'CAN_READ',
        user: userid,
        businessalertprops: {
            businessAlertId: businessAlertId
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: businessAlertsReqTopicKey
    }
}

function createBusinessAlertReq(reqid, userid, businessalert, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'createBusinessAlert',
        model: 'BusinessAlertModel',
        action: 'CAN_CREATE',
        user: userid,
        businessalertprops: businessalert,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: businessAlertsReqTopicKey
    }
}

function updateBusinessAlertReq(reqid, userid, businessalert, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'updateBusinessAlert',
        model: 'BusinessAlertModel',
        action: 'CAN_UPDATE',
        user: userid,
        businessalertprops: businessalert,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: businessAlertsReqTopicKey
    }
}

function deleteOrDismissBusinessAlertReq(reqid, userid, businessAlertId, orgid, siteid, isDeleteBusinessAlert) {
    const type = isDeleteBusinessAlert ? 'deleteBusinessAlert' : 'dismissBusinessAlert';
    return {
        requestid: reqid,
        type: type,
        model: 'BusinessAlertModel',
        action: 'CAN_DELETE',
        user: userid,
        businessalertprops: {
            businessAlertId: businessAlertId
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: businessAlertsReqTopicKey
    }
}

/**
 * Used for SMS/email feature. Refer /encoder/Encoder.js
 * @param {*} userid
 * @param {*} business alert id
 * @param {*} siteid
 */
function getBusinessAlertSysReq(userid, businessAlertId, orgid, siteid) {
    return {
        type: 'getBusinessAlertSys',
        model: 'BusinessAlertModel',
        action: 'CAN_READ',
        userid: 'root',
        businessalertprops: {
            businessAlertId: businessAlertId
        },
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: businessAlertsReqTopicKey
    }
}