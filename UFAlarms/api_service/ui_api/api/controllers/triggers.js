'use strict';

const util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid'),
    triggersModel = require('./../models/triggers.js'),
    response = require('./../helpers/response.js');

// Export all the functions to use it in swagger.yaml
module.exports = {
    getAllTriggers,
    getTrigger,
    createTrigger,
    updateTrigger,
    deleteTrigger
};

// GET /customers/{orgid}/sites/{siteid}/triggers
function getAllTriggers(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    let reqPayload = triggersModel.getAllTriggersReq(req.request_id, req.getCurrentUser(), orgid, siteid);
    let filterQuery = req.swagger.params.filter.value;
    // To remove quotes (single & double) from filter query
    filterQuery = filterQuery ? filterQuery.replace(/["']/g, "") : null;
    if (filterQuery && filterQuery.trim().length > 0) {
        reqPayload = triggersModel.getAllFilteredTriggersReq(req.request_id, req.getCurrentUser(), filterQuery, orgid, siteid);
    }
    sendRequest(reqPayload, req, res);
}

// GET /customers/{orgid}/sites/{siteid}/triggers/{triggerid}
function getTrigger(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const triggerid = req.swagger.params.triggerid.value;
    const reqPayload = triggersModel.getTriggerReq(req.request_id, req.getCurrentUser(), triggerid, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// POST /customers/{orgid}/sites/{siteid}/triggers
function createTrigger(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const id = uuid.v1();
    let trigger = req.body;
    trigger.triggerid = id;
    const reqPayload = triggersModel.createTriggerReq(req.request_id, req.getCurrentUser(), trigger, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// PUT /customers/{orgid}/sites/{siteid}/triggers/{triggerid}
function updateTrigger(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    let trigger = req.body;
    trigger.triggerid = req.swagger.params.triggerid.value;
    const reqPayload = triggersModel.updateTriggerReq(req.request_id, req.getCurrentUser(), trigger, orgid, siteid,trigger.triggerid);
    sendRequest(reqPayload, req, res);
}

// DELETE /customers/{orgid}/sites/{siteid}/triggers/{triggerid}
function deleteTrigger(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const triggerid = req.swagger.params.triggerid.value;
    const reqPayload = triggersModel.deleteTriggerReq(req.request_id, req.getCurrentUser(), triggerid, orgid, siteid);
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
    encoder.encodeMSRequest(req.getCurrentUser(), reqPayload, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}