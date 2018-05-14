'use strict';

var util = require('util'),
    uuid = require('uuid');
var encoder = require('./../../encoder/Encoder.js').JSONEncoder;

var response = require('./../helpers/response.js');

module.exports = {
    addFirmware: addFirmware,
    updateFirmware: updateFirmware,
    deleteFirmware: deleteFirmware,
    readFirmware: readFirmware,
    readAllFirmwares: readAllFirmwares,
    revertFirmware: revertFirmware,
    downloadFirmware: downloadFirmware,
    assignFirmwareToNode: assignFirmwareToNode,
    assignFirmwareToSite: assignFirmwareToSite,
    assignFirmwareToGroup: assignFirmwareToGroup,
    otaStatusForSite: otaStatusForSite,
    otaStatusForJob: otaStatusForJob,
    otaStatusJobUpdate: otaStatusJobUpdate
};

/**
 * Read all operation
 * @param req
 * @param res
 */
function readAllFirmwares(req, res) {
    encoder.readAllFirmwares(req.getCurrentUser(), function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

/**
 * Add operation
 * @param req
 * @param res
 */
function addFirmware(req, res) {
    var firmware = req.body,
        firmwareid = firmware.commit;

    firmware.firmwareid = firmwareid;

    encoder.addFirmware(req.getCurrentUser(), firmware, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

/**
 * Delete operation
 * @param req
 * @param res
 */
function deleteFirmware(req, res) {
    var firmwareid = req.swagger.params.firmwareid.value;
    encoder.deleteFirmware(req.getCurrentUser(), firmwareid, function (err, msg) {
        response.Done(err, null, res, req);
    });
}

/**
 * Update operation
 * @param req
 * @param res
 */
function updateFirmware(req, res) {
    var firmwareid = req.swagger.params.firmwareid.value;
    var firmware = req.body;
    firmware.firmwareid = firmwareid;
    encoder.updateFirmware(req.getCurrentUser(), firmware, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

/**
 * Read / ReadAll operation
 * @param req
 * @param res
 */
function readFirmware(req, res) {
    var firmwareid = req.swagger.params.firmwareid.value;
    encoder.readFirmware(req.getCurrentUser(), firmwareid, function (err, msg) {
        //console.log('read fw', err, msg)
        response.Done(err, msg, res, req);
    });
}


/**
 * Assign operation for node
 * @param req
 * @param res
 */
function assignFirmwareToNode(req, res) {
    var firmwareid = req.swagger.params.firmwareid.value,
        orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        nodeid = req.swagger.params.nodeid.value,
        description = req.body.description;

    encoder.assignFirmwareToNode(req.getCurrentUser(), orgid, siteid, nodeid, firmwareid, description, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

/**
 * Assign operation for group
 * @param req
 * @param res
 */
function assignFirmwareToGroup(req, res) {
    var firmwareid = req.swagger.params.firmwareid.value,
        orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        groupid = req.swagger.params.groupid.value,
        description = req.body.description;

    encoder.assignFirmwareToGroup(req.getCurrentUser(), orgid, siteid, [groupid], firmwareid, description, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

/**
 * Assign operation for site
 * @param req
 * @param res
 */
function assignFirmwareToSite(req, res) {
    var firmwareid = req.swagger.params.firmwareid.value,
        orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        description = req.body.description;

    encoder.assignFirmwareToSite(req.getCurrentUser(), orgid, siteid, firmwareid, description, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

/**
 * Revert operation
 * @param req
 * @param res
 */
function revertFirmware(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var nodeList = req.body.nodeList;

    encoder.assignFirmware(req.getCurrentUser(), orgid, siteid, nodeList, null, function (err, msg) {
        response.Done(err, null, res, req);
    });
}

/**
 * Read / ReadAll operation
 * @param req
 * @param res
 */
function downloadFirmware(req, res) {

    var fs = require('fs');

    var id = (req.swagger.params.id)?req.swagger.params.id.value:null;

    //doFirmwareCrud(req, res, "assign", id, id?{id:id}:null);
    // TODO: Remove mockup
    (function readfile(req, res) {
        if(true) {
            //res.setHeader("content-type", "application/octet-stream");
            var paths = fs.readdirSync('.');
            var stream = fs.createReadStream("./test/data/micronode-1.0.5.1-5c6fef2.zip")
            stream.pipe(res);
        }
    })(req, res);
}

/**
 * Get all ota commands for a site
 * @param req
 * @param res
 */
function otaStatusForSite(req, res) {
    var orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value;

    encoder.otaStatusForSite(req.getCurrentUser(), orgid, siteid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

/**
 * Get all ota status for a job
 * @param req
 * @param res
 */
function otaStatusForJob(req, res) {
    var orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        jobid = req.swagger.params.jobid.value;

    encoder.otaStatusForJob(req.getCurrentUser(), orgid, siteid, jobid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

/**
 * Update ota status for a job
 * @param req
 * @param res
 */
function otaStatusJobUpdate(req, res) {
    var orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        jobid = req.swagger.params.jobid.value,
        action = req.swagger.params.action.value;

    encoder.otaStatusJobUpdate(req.getCurrentUser(), orgid, siteid, jobid, action, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}
