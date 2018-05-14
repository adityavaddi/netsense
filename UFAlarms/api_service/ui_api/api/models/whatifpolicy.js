"use strict";

/**
 * Request payloads of all what-if policy APIs.
 */

// Export all the functions to use it in controller
module.exports = {
    getAllWhatIfPoliciesReq,
    getWhatIfPolicyReq,
    createWhatIfPolicyReq,
    updateWhatIfPolicyReq,
    deleteWhatIfPolicyReq,
    whatIfPolicyTagsAssociationReq,
    whatIfPolicyTagsDisassociationReq
};

// refer main.conf.js for the actual request topic name
const parkingPolicyTagReqTopicKey = "parkingpolicytagrequest";

function getAllWhatIfPoliciesReq(reqid, orgid, siteid) {
    return {
        requestid: reqid,
        type: "getAllWhatIfPolicies",
        model: "WhatIfPolicyModel",
        action: "CAN_READ",
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: parkingPolicyTagReqTopicKey
    };
}

function getWhatIfPolicyReq(reqid, policyid, orgid, siteid) {
    return {
        requestid: reqid,
        type: "getWhatIfPolicy",
        model: "WhatIfPolicyModel",
        action: "CAN_READ",
        configprops: {
            policyid: policyid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: parkingPolicyTagReqTopicKey
    };
}

function createWhatIfPolicyReq(reqid, whatIfPolicy, orgid, siteid) {
    return {
        requestid: reqid,
        type: "createWhatIfPolicy",
        model: "WhatIfPolicyModel",
        action: "CAN_CREATE",
        configprops: {
            policy: whatIfPolicy
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: parkingPolicyTagReqTopicKey
    };
}

function updateWhatIfPolicyReq(reqid, policyid, whatIfPolicy, orgid, siteid) {
    return {
        requestid: reqid,
        type: "updateWhatIfPolicy",
        model: "WhatIfPolicyModel",
        action: "CAN_UPDATE",
        configprops: {
            policyid: policyid,
            policy: whatIfPolicy
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: parkingPolicyTagReqTopicKey
    };
}


function deleteWhatIfPolicyReq(reqid, policyid, orgid, siteid) {
    return {
        requestid: reqid,
        type: "deleteWhatIfPolicy",
        model: "WhatIfPolicyModel",
        action: "CAN_DELETE",
        configprops: {
            policyid: policyid
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: parkingPolicyTagReqTopicKey
    };
}


function whatIfPolicyTagsAssociationReq(reqid, policyid, tagspolicylink, orgid, siteid) {
    return {
        requestid: reqid,
        type: "whatIfPolicyTagsAssociation",
        model: "WhatIfPolicyModel",
        action: "CAN_UPDATE",
        configprops: {
            policyid: policyid,
            tagspolicylink: tagspolicylink
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: parkingPolicyTagReqTopicKey
    };
}


function whatIfPolicyTagsDisassociationReq(reqid, policyid, tagspolicylink, orgid, siteid) {
    return {
        requestid: reqid,
        type: "whatIfPolicyTagsDisassociation",
        model: "WhatIfPolicyModel",
        action: "CAN_DELETE",
        configprops: {
            policyid: policyid,
            tagspolicylink: tagspolicylink
        },
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        },
        service: parkingPolicyTagReqTopicKey
    };
}