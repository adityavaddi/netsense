'use strict';
/**
 * Request payloads of all Parking SpotMetadata APIs.
 */

// Export all the functions to use it in controller
module.exports = {
    getAllParkingSpotsReq,
    bulkCreateOrUpdateParkingSpotReq,
    bulkDeleteParkingSpotReq
}

// refer main.conf.js for the actual request topic name
const parkingMetadataReqTopicKey = "parkingmetadatarequest";

function getAllParkingSpotsReq(reqid, userid, parkingSpotIds, orgid, siteid) {
    let params = {
        requestid: reqid,
        type: 'getAllMetadataForParkingSpot',
        model: 'ParkingSpotModel',
        action: 'CAN_READ',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: parkingMetadataReqTopicKey
    };
    params.parkingspaceids = parkingSpotIds;
    return params;
}

function bulkCreateOrUpdateParkingSpotReq(reqid, userid, parkingSpaces, orgid, siteid) {
    return {
        requestid: reqid,
        type: 'updateMetadataForParkingSpot',
        model: 'ParkingSpotModel',
        action: 'CAN_UPDATE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        spaceattributes: parkingSpaces,
        service: parkingMetadataReqTopicKey
    }
}

function bulkDeleteParkingSpotReq(reqid, userid, parkingSpaceIds, orgid, siteid) {
    let params = {
        requestid: reqid,
        type: 'deleteMetadataForParkingSpot',
        model: 'ParkingSpotModel',
        action: 'CAN_DELETE',
        user: userid,
        orgprops: {
            orgid: orgid
        },
        siteprops: {
            siteid: siteid
        },
        service: parkingMetadataReqTopicKey
    };
    params.parkingspaceids = parkingSpaceIds;
    return params;
}