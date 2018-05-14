'use strict';

const util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid'),
    businessAlertsModel = require('./../models/businessalerts.js'),
    response = require('./../helpers/response.js');

// Export all the functions to use it in swagger.yaml
module.exports = {
    getAllBusinessAlerts,
    getBusinessAlert,
    dismissBusinessAlert,
    createBusinessAlert,
    updateBusinessAlert,
    deleteBusinessAlert
};


// GET /customers/{orgid}/sites/{siteid}/businessalerts
function getAllBusinessAlerts(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    let reqPayload = businessAlertsModel.getAllBusinessAlertsReq(req.request_id, req.getCurrentUser(), orgid, siteid);
    let filterQuery = req.swagger.params.filter.value;
    // To remove quotes (single & double) from filter query
    filterQuery = filterQuery ? filterQuery.replace(/["']/g, "") : null;
    if (filterQuery && filterQuery.trim().length > 0) {
        reqPayload = businessAlertsModel.getAllFilteredBusinessAlertsReq(req.request_id, req.getCurrentUser(), filterQuery, orgid, siteid);
    }
    sendRequest(reqPayload, req, res);
}

// GET /customers/{orgid}/sites/{siteid}/businessalerts/{businessalertid}
function getBusinessAlert(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const businessalertid = req.swagger.params.businessalertid.value;
    const reqPayload = businessAlertsModel.getBusinessAlertReq(req.request_id, req.getCurrentUser(), businessalertid, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// PUT /customers/{orgid}/sites/{siteid}/businessalerts/dismiss/{businessalertid}
function dismissBusinessAlert(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const businessalertid = req.swagger.params.businessalertid.value;
    const reqPayload = businessAlertsModel.deleteOrDismissBusinessAlertReq(req.request_id, req.getCurrentUser(), businessalertid, orgid, siteid, false);
    sendRequest(reqPayload, req, res);
}

/**
 * Below apis are not supported by MS yet
 */
// POST /customers/{orgid}/sites/{siteid}/businessalerts
function createBusinessAlert(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const id = uuid.v1();
    let businessAlert = req.body;
    businessAlert.businessalertid = id;
    const reqPayload = businessAlertsModel.createBusinessAlertReq(req.request_id, req.getCurrentUser(), businessAlert, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// PUT /customers/{orgid}/sites/{siteid}/businessalerts/{businessalertid}
function updateBusinessAlert(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    let businessAlert = req.body;
    businessAlert.businessalertid = req.swagger.params.businessalertid.value;
    const reqPayload = businessAlertsModel.updateBusinessAlertReq(req.request_id, req.getCurrentUser(), businessAlert, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// DELETE /customers/{orgid}/sites/{siteid}/businessalerts/{businessalertid}/delete
function deleteBusinessAlert(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const businessalertid = req.swagger.params.businessalertid.value;
    const reqPayload = businessAlertsModel.deleteOrDismissBusinessAlertReq(req.request_id, req.getCurrentUser(), businessalertid, orgid, siteid, true);
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