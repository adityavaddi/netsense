/**
 * Request payloads of all alerts APIs.
 */
"use strict";
// Export all the functions to use it in controller
module.exports = {
    getGpsByNodeIdReq,
    getGpsForOrgIdAndSiteIdReq
}

// refer main.conf.js for the actual request topic name
const gpsReqTopicKey = "gpsrequest";

function getGpsByNodeIdReq(reqid, userid, orgid, siteid, nodeid) {
    return {
        requestid: reqid,
        type: 'getGpsByNodeId',
        model: 'GpsModel',
        action: 'CAN_READ',
        user: userid,       
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        "nodeprops": {
            "nodeid": nodeid
        },
        service: gpsReqTopicKey
    }
}

function getGpsForOrgIdAndSiteIdReq(reqid, userid, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'getGpsForOrgIdAndSiteId',
        model: 'GpsModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: gpsReqTopicKey
    }
}