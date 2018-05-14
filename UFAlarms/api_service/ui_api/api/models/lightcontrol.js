/**
 * Request payloads of all Light control APIs.
 */

'use strict';

// Export all the functions to use it in controller
module.exports = {
    setLightLevelForDeviceReq,
    setLightLevelOnSiteReq,
    setLightLevelInGroupReq
}

function setLightLevelForDeviceReq(reqid, userid, body, nodeid, orgid, siteid) {
    body.nodeid = nodeid;

    return {
        requestid: reqid,
        type: 'lighting-control',
        model: 'NodeModel',
        action: 'CAN_MANAGE_LIGHTS',
        user: userid,
        nodeprops: addTypeAndHarvesting(body, false),
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    };
}


function setLightLevelOnSiteReq(reqid, userid, body, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'lighting-control-site',
        model: 'NodeModel',
        action: 'CAN_MANAGE_LIGHTS',
        user: userid,
        nodeprops: addTypeAndHarvesting(body, true),
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    };
}

function setLightLevelInGroupReq(reqid, userid, body, groupid, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'lighting-control-group',
        model: 'NodeModel',
        action: 'CAN_MANAGE_LIGHTS',
        user: userid,
        nodeprops: addTypeAndHarvesting(body, true),
        siteprops: {
            siteid: siteid
        },
        groupprops: {
            groupid: groupid
        },
        orgprops: {
            orgid: orgid
        }
    };
}

function addTypeAndHarvesting(body, addHarvesting) {
    let state = "LightingForceState";
    if (body.clear === true) {
        state = "LightingSetAuto";
    }
    body.type = state;

    if (addHarvesting) {
        body.harvesting = false;
    }

    return body;
}