"use strict";

/**
 * Request payloads of all what-if analysis APIs.
 */

// Export all the functions to use it in controller
module.exports = {
    getAllWhatIfJobsReq,
    getAllFilteredWhatIfJobsReq,
    getWhatIfJobReq,
    createWhatIfJobReq,
    updateWhatIfJobReq,
    abortWhatIfJobReq,
    deleteWhatIfJobReq
};

// refer main.conf.js for the actual request topic name
const whatIfReqTopicKey = "whatifrequest";

function getAllWhatIfJobsReq(reqid, orgid, siteid) {
    return {
        requestid: reqid,
        type: "getAllWhatIfJobs",
        model: "WhatIfAnalysisModel",
        action: "CAN_READ",
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: whatIfReqTopicKey
    };
}

function getAllFilteredWhatIfJobsReq(reqid, filterQuery, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'searchWhatIfJob',
        model: 'WhatIfAnalysisModel',
        action: 'CAN_READ',
        whatifprops: {
            search: filterQuery
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },       
        service: whatIfReqTopicKey
    }
}

function getWhatIfJobReq(reqid, jobid, orgid, siteid) {
    return {
        requestid: reqid,
        type: "getWhatIfJob",
        model: "WhatIfAnalysisModel",
        action: "CAN_READ",
        whatifprops: {
            jobid: jobid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: whatIfReqTopicKey
    };
}

function createWhatIfJobReq(reqid, whatIfJob, orgid, siteid, userEmail) {
    return {
        requestid: reqid,
        type: "createWhatIfJob",
        model: "WhatIfAnalysisModel",
        action: "CAN_CREATE",
        whatifprops: {
            whatIfJobRequest: whatIfJob
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        userprops: {
            userEmail: userEmail
        },
        service: whatIfReqTopicKey
    };
}

function updateWhatIfJobReq(reqid, jobid, whatIfJob, orgid, siteid, userEmail) {    
    return {
        requestid: reqid,
        type: "updateWhatIfJob",
        model: "WhatIfAnalysisModel",
        action: "CAN_UPDATE",
        whatifprops: {
            jobid: jobid,
            whatIfJobRequest: whatIfJob
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        userprops: {
            userEmail: userEmail
        },
        service: whatIfReqTopicKey
    };
}

function abortWhatIfJobReq(reqid, jobid, orgid, siteid) {
    return {
        requestid: reqid,
        type: "abortWhatIfJob",
        model: "WhatIfAnalysisModel",
        action: "CAN_UPDATE",
        whatifprops: {
            jobid: jobid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: whatIfReqTopicKey
    };
}

function deleteWhatIfJobReq(reqid, jobid, orgid, siteid) {
    return {
        requestid: reqid,
        type: "deleteWhatIfJob",
        model: "WhatIfAnalysisModel",
        action: "CAN_DELETE",
        whatifprops: {
            jobid: jobid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: whatIfReqTopicKey
    };
}