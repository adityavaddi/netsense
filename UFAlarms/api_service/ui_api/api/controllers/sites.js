'use strict';

var util = require('util'),
    uuid = require('uuid'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder;

var response = require('./../helpers/response.js');

module.exports = {
    getSite: getSite,
    getAllSites: getAllSites,
    getAllSuspendedSites: getAllSuspendedSites,
    addSite: addSite,
    deleteSite: deleteSite,
    updateSite: updateSite,
    suspendSite: suspendSite,
    activateSite: activateSite
};

function getSite(req, res) {
    encoder.getSite(req.getCurrentUser(), req.swagger.params.siteid.value, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getAllSites(req, res) {
    encoder.getAllSites(req.getCurrentUser(), req.swagger.params.orgid.value, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getAllSuspendedSites(req, res) {
    encoder.getAllSuspendedSites(req.getCurrentUser(), req.swagger.params.orgid.value, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function addSite(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var newid = uuid.v1();
    var site = req.body;
    site.siteid = newid;
    global.log.info("Adding site: " + JSON.stringify(site));
    encoder.createSite(req.getCurrentUser(), site, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function updateSite(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var site = req.body;
    site.siteid = siteid;
    global.log.info("Updating site: " + JSON.stringify(site));
    encoder.updateSite(req.session.CurrentUser, site, orgid, function (err, msg) {
        response.Done(err, msg, res, req);
    });
}

function deleteSite(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    global.log.info("Deleting site: " + siteid);
    encoder.deleteSite(req.session.CurrentUser, siteid, orgid, function (err, msg) {
        response.Done(err, null, res, req);
    });
}

function suspendSite(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    global.log.info("Suspending site: " + siteid);
    encoder.suspendSite(req.session.CurrentUser, siteid, orgid, function (err, msg) {
        response.Done(err, null, res, req);
    });
}

function activateSite(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    global.log.info("Activating site: " + siteid);
    encoder.activateSite(req.session.CurrentUser, siteid, orgid, function (err, msg) {
        response.Done(err, null, res, req);
    });
}


/**
 * Encoder call wrapper
 * @param req
 * @param res
 * @param operation
 * @param id
 * @param params
 */
function doSiteCrud(req, res, operation, id, params) {
    var callback = function (err, data) {
        response.Done(err, data, res, req);
    };
    encoder.doCrud(req.getCurrentUser(), operation, 'SiteModel', params, callback);
}
