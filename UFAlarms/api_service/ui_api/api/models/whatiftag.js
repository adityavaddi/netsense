"use strict";

/**
 * Request payloads of all what-if tag APIs.
 */

// Export all the functions to use it in controller
module.exports = {
    getAllWhatIfTagsReq,
    getWhatIfTagReq,
    createWhatIfTagReq,
    updateWhatIfTagReq,
    deleteWhatIfTagReq
};

// refer main.conf.js for the actual request topic name
const parkingTagReqTopicKey = "parkingtagrequest";

function getAllWhatIfTagsReq(reqid, orgid, siteid) {
    return {
        requestid: reqid,
        type: "getAllWhatIfTags",
        model: "WhatIfTagModel",
        action: "CAN_READ",
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: parkingTagReqTopicKey
    };
}

function getWhatIfTagReq(reqid, tagid, orgid, siteid) {
    return {
        requestid: reqid,
        type: "getWhatIfTag",
        model: "WhatIfTagModel",
        action: "CAN_READ",
        configprops: {
            tagid: tagid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: parkingTagReqTopicKey
    };
}

function createWhatIfTagReq(reqid, whatIfTag, orgid, siteid) {
    return {
        requestid: reqid,
        type: "createWhatIfTag",
        model: "WhatIfTagModel",
        action: "CAN_CREATE",
        configprops: {
            tag: whatIfTag
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: parkingTagReqTopicKey
    };
}

function updateWhatIfTagReq(reqid, tagid, whatIfTag, orgid, siteid) {
    return {
        requestid: reqid,
        type: "updateWhatIfTag",
        model: "WhatIfTagModel",
        action: "CAN_UPDATE",
        configprops: {
            tagid: tagid,
            tag: whatIfTag
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: parkingTagReqTopicKey
    };
}


function deleteWhatIfTagReq(reqid, tagid, orgid, siteid) {
    return {
        requestid: reqid,
        type: "deleteWhatIfTag",
        model: "WhatIfTagModel",
        action: "CAN_DELETE",
        configprops: {
            tagid: tagid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: parkingTagReqTopicKey
    };
}