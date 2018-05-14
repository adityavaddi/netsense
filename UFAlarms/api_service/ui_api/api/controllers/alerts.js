'use strict';

const util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid'),
    alertsModel = require('./../models/alerts.js'),
    response = require('./../helpers/response.js'),
    helpers = require('./../helpers/helpers.js');

// Export all the functions to use it in swagger.yaml
module.exports = {
    getAlert,
    getAllAlerts,
    getAlertsForNode,
    createAlert,
    updateAlert,
    deleteAlert,
    dismissAlert
};

// GET /customers/{orgid}/sites/{siteid}/alerts
function getAllAlerts(req, res) {
    const user = req.getCurrentUser();
    const userType = helpers.getUserType(user);
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const reqPayload = alertsModel.getAllAlertsReq(req.request_id, req.getCurrentUser(), orgid, siteid, userType);
    sendRequest(reqPayload, req, res);
}

// GET /customers/{orgid}/sites/{siteid}/alerts/node/{nodeid}
function getAlertsForNode(req, res) {
    const user = req.getCurrentUser();
    const userType = helpers.getUserType(user);
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const nodeid = req.swagger.params.nodeid.value;
    const reqPayload = alertsModel.getAlertsForNodeReq(req.request_id, req.getCurrentUser(), orgid, siteid, nodeid, userType);
    sendRequest(reqPayload, req, res);
}

// GET /customers/{orgid}/sites/{siteid}/alerts/{alertid}
function getAlert(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const alertid = req.swagger.params.alertid.value;
    const reqPayload = alertsModel.getAlertReq(req.request_id, req.getCurrentUser(), alertid, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// DELETE /customers/{orgid}/sites/{siteid}/alerts/{alertid}
function deleteAlert(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const alertid = req.swagger.params.alertid.value;
    const reqPayload = alertsModel.deleteAlertReq(req.request_id, req.getCurrentUser(), alertid, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// POST /customers/{orgid}/sites/{siteid}/alerts/dismiss/{alertid}
function dismissAlert(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const alertid = req.swagger.params.alertid.value;
    const reqPayload = alertsModel.dismissAlertReq(req.request_id, req.getCurrentUser(), alertid, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// POST /customers/{orgid}/sites/{siteid}/alerts
function createAlert(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const id = uuid.v1();
    let alert = req.body;
    alert.alertid = id;
    const reqPayload = alertsModel.createAlertReq(req.request_id, req.getCurrentUser(), alert, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// Update alert is commented out in swagger.yaml
// POST /customers/{orgid}/sites/{siteid}/alerts/{alertid}
function updateAlert(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    let alert = req.body;
    alert.alertid = req.swagger.params.alertid.value;
    const reqPayload = alertsModel.updateAlertReq(req.request_id, req.getCurrentUser(), alert, orgid, siteid);
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