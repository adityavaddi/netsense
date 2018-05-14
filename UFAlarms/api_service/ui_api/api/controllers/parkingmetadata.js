'use strict';

const util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid'),
    parkingMetadataModel = require('./../models/parkingmetadata.js'),
    response = require('./../helpers/response.js');

// Export all the functions to use it in swagger.yaml
module.exports = {
    getAllParkingSpots,
    bulkCreateOrUpdateParkingSpot,
    bulkDeleteParkingSpot
};

//POST /customers/{orgid}/sites/{siteid}/parkingspaces/spaceattributes
function getAllParkingSpots(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const parkingSpotIds = req.body.parkingspaceids;
    var errors = validateParkingSpotIds(parkingSpotIds);
    if (errors) {
        return response.Done(errors, null, res, req);
    }
    const reqPayload = parkingMetadataModel.getAllParkingSpotsReq(req.request_id, req.getCurrentUser(), parkingSpotIds, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// PUT /customers/{orgid}/sites/{siteid}/parkingspaces/spaceattributes
function bulkCreateOrUpdateParkingSpot(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const parkingSpaces = req.body;
    const reqPayload = parkingMetadataModel.bulkCreateOrUpdateParkingSpotReq(req.request_id, req.getCurrentUser(), parkingSpaces, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// DELETE /customers/{orgid}/sites/{siteid}/parkingspaces/spaceattributes
function bulkDeleteParkingSpot(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const parkingSpaceIds = req.body.parkingspaceids;
    var errors = validateParkingSpotIds(parkingSpaceIds);
    if (errors) {
        return response.Done(errors, null, res, req);
    }
    const reqPayload = parkingMetadataModel.bulkDeleteParkingSpotReq(req.request_id, req.getCurrentUser(), parkingSpaceIds, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

/**
 * Common function for all above functions
 * @param {*} user - logged in user
 * @param {*} reqPayload - request payload
 * @param {*} req - request
 * @param {*} res - response
 */
function sendRequest(reqPayload, req, res) {
    encoder.encodeParkingSpotsRequest(req.getCurrentUser(), reqPayload, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

/**
 * Check for empty parkingSpotIds array and empty parking spot id string inside array
 * @param {*} parkingSpaceIds - array of parking space ids
 */
function validateParkingSpotIds(parkingSpaceIds) {
    if (!parkingSpaceIds.length > 0 || !parkingSpaceIds.every(isParkingSpotIdNotEmpty)) {
        return {
            error: true,
            message: "Request contains invalid parking space ids",
            status: 400
        };
    }
}

function isParkingSpotIdNotEmpty(currentValue) {
    return currentValue.trim().length > 0;
}