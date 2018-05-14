'use strict';

const util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    lightControlModel = require('./../models/lightcontrol.js'),
    response = require('./../helpers/response.js');

module.exports = {
    setLightLevelForDevice,
    setLightLevelOnSite,
    setLightLevelInGroup
};

// POST /customers/{orgid}/sites/{siteid}/lightcontrol/node/{nodeid}:
function setLightLevelForDevice(req, res) {
    const orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        nodeid = req.swagger.params.nodeid.value,
        body = req.body;
    const reqPayload = lightControlModel.setLightLevelForDeviceReq(req.request_id, req.getCurrentUser(), body, nodeid, orgid, siteid);
    encoder.setLightLevelForDevice(reqPayload, function (err, msg) {
        response.Done(err, null, res, req);
    });
}

// POST /customers/{orgid}/sites/{siteid}/lightcontrol:
function setLightLevelOnSite(req, res) {
    const orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        body = req.body;
    const reqPayload = lightControlModel.setLightLevelOnSiteReq(req.request_id, req.getCurrentUser(), body, orgid, siteid);
    encoder.setLightLevelOnSite(reqPayload, function (err, msg) {
        response.Done(err, null, res, req);
    });
}

// POST /customers/{orgid}/sites/{siteid}/lightcontrol/groups/{groupid}:
function setLightLevelInGroup(req, res) {
    const orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        groupid = req.swagger.params.groupid.value,
        body = req.body;
    const reqPayload = lightControlModel.setLightLevelInGroupReq(req.request_id, req.getCurrentUser(), body, groupid, orgid, siteid);
    encoder.setLightLevelInGroup(reqPayload, function (err, msg) {
        response.Done(err, null, res, req);
    });
}
