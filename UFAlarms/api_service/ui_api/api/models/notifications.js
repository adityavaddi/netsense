/**
 * Request payloads of all notifications APIs.
 */
"use strict";
// Export all the functions to use it in controller
module.exports = {
    getNotificationReq,
    getAllNotificationsForUserReq,
    getAllNotificationsForSiteReq,
    createNotificationReq,
    updateNotificationReq,
    deleteNotificationReq,
    activateNotificationReq,
    getAllNotificationsByNameReq,
    getNotificationSysReq,
    getNotificationTypesReq    
}

// refer main.conf.js for the actual request topic name
const alarmReqTopicKey = "alarmrequest";

function getNotificationReq(reqid, userid, notificationid, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'getNotification',
        model: 'NotificationModel',
        action: 'CAN_READ',
        user: userid,
        notificationprops: {
            notificationid: notificationid
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

// Not used
function getAllNotificationsForUserReq(reqid, userid, orgid) {
    return {
        requestid: reqid,
        type: 'getAllNotificationsForUser',
        model: 'NotificationModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        userprops: {
            userid: userid.id
        },
        service: alarmReqTopicKey
    }
}

function getAllNotificationsForSiteReq(reqid, userid, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'getAllNotificationsForSite',
        model: 'NotificationModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: alarmReqTopicKey
    }
}

function createNotificationReq(reqid, userid, notification, orgid, siteid, usertype) {
    return {
        requestid: reqid,
        type: 'createNotification',
        model: 'NotificationModel',
        action: 'CAN_CREATE',
        user: userid,
        notificationprops: notification,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        userprops: {
            usertype: usertype
        },
        service: alarmReqTopicKey
    }
}

function updateNotificationReq(reqid, userid, notification, orgid, siteid, usertype) {
    return {
        requestid: reqid,
        type: 'updateNotification',
        model: 'NotificationModel',
        action: 'CAN_UPDATE',
        user: userid,
        notificationprops: notification,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        userprops: {
            usertype: usertype
        },
        service: alarmReqTopicKey
    }
}

function deleteNotificationReq(reqid, userid, notificationid, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'deleteNotification',
        model: 'NotificationModel',
        action: 'CAN_DELETE',
        user: userid,
        notificationprops: {
            notificationid: notificationid
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

function activateNotificationReq(reqid, userid, notificationid, act, orgid, siteid) {
    const type = act ? 'activateNotification' : 'deactivateNotification';
    const action = act ? 'CAN_UPDATE' : 'CAN_UPDATE';
    return {
        requestid: reqid,
        type: type,
        model: 'NotificationModel',
        action: action,
        user: userid,
        notificationprops: {
            notificationid: notificationid
        }, orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: alarmReqTopicKey
    }
}

function getAllNotificationsByNameReq(userid, name, orgid, siteid) {
    return {
        type: 'getAllNotificationsByName',
        model: 'NotificationModel',
        action: 'CAN_READ',
        userid: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        notificationprops: {
            name: name
        },
        service: alarmReqTopicKey
    }
}

function getNotificationSysReq(userid, notificationid, orgid, siteid) {
    return {
        type: 'getNotificationSys',
        model: 'NotificationModel',
        action: 'CAN_READ',
        userid: 'root',
        notificationprops: {
            notificationid: notificationid
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

function getNotificationTypesReq(reqid, usertype) {
    return {
        requestid: reqid,
        type: 'getNotificationTypes',
        model: 'NotificationModel',
        action: 'CAN_READ',
        userprops: {
            usertype: usertype
        },
        service: alarmReqTopicKey
    }
}