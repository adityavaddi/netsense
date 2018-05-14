/**
 * Request payloads of all Parking UserData APIs.
 */
'use strict';
// Export all the functions to use it in controller
module.exports = {
    getAllAppUserDataReq,
    getAppUserDataReq,
    createAppUserDataReq,
    updateAppUserDataReq,
    deleteAppUserDataReq
}
// refer main.conf.js for the actual request topic name
const parkUserDataReqTopicKey = "parkinguserdatarequest";

function getAllAppUserDataReq(appid, userid) {
    return {
        type: 'getAllAppUserData',
        model: 'ParkingPolicyModel', //There is no AppUserData Model.So, using Parking Policy Model to pass IS. It shouldn't affect any functionality
        action: 'CAN_READ',
        appuserdataprops: {
            appid: appid,
            userid: userid
        },
        service: parkUserDataReqTopicKey
    }
}

function getAppUserDataReq(appid, userid, userdataid) {
    return {
        type: 'getAppUserData',
        model: 'ParkingPolicyModel',
        action: 'CAN_READ',
        appuserdataprops: {
            appid: appid,
            userid: userid,
            userdataid: userdataid
        },
        service: parkUserDataReqTopicKey
    }
}

function createAppUserDataReq(appid, userid, datavalue) {
    return {
        type: 'createAppUserData',
        model: 'ParkingPolicyModel',
        action: 'CAN_CREATE',
        appuserdataprops: {
            appid: appid,
            userid: userid,
            datavalue: datavalue
        },
        service: parkUserDataReqTopicKey
    }
}

function updateAppUserDataReq(appid, userid, userdataid, datavalue) {
    return {
        type: 'updateAppUserData',
        model: 'ParkingPolicyModel',
        action: 'CAN_UPDATE',
        appuserdataprops: {
            appid: appid,
            userid: userid,
            userdataid: userdataid,
            datavalue: datavalue
        },
        service: parkUserDataReqTopicKey
    }
}

function deleteAppUserDataReq(appid, userid, userdataid) {
    return {
        type: 'deleteAppUserData',
        model: 'ParkingPolicyModel',
        action: 'CAN_DELETE',
        appuserdataprops: {
            appid: appid,
            userid: userid,
            userdataid: userdataid
        },
        service: parkUserDataReqTopicKey
    }
}