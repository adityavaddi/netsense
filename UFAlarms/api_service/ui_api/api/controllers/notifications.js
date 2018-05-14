'use strict';

const util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid'),
    xssFilters = require('xss-filters'),
    notificationsModel = require('./../models/notifications.js'),
    response = require('./../helpers/response.js'),
    helpers = require('./../helpers/helpers.js');

module.exports = {
    getNotification,
    getAllNotificationsForUser,
    getAllNotificationsForSite,
    createNotification,
    updateNotification,
    deleteNotification,
    activateNotification,
    deactivateNotification,
    getNotificationTypes
};

// GET /customers/{orgid}/sites/{siteid}/notifications
function getAllNotificationsForSite(req, res) {
    const user = req.getCurrentUser();
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const reqPayload = notificationsModel.getAllNotificationsForSiteReq(req.request_id, user, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// GET /customers/{orgid}/user-notifications/
// Not used 
function getAllNotificationsForUser(req, res) {
    const user = req.getCurrentUser();
    const orgid = req.swagger.params.orgid.value;
    const reqPayload = notificationsModel.getAllNotificationsForUserReq(req.request_id, user, orgid);
    sendRequest(reqPayload, req, res);
}

// GET /customers/{orgid}/notifications/{notificationid}
// GET /customers/{orgid}/sites/{siteid}/notifications/{notificationid}
function getNotification(req, res) {
    const user = req.getCurrentUser();
    const notificationid = req.swagger.params.notificationid.value;
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid ?
        req.swagger.params.siteid.value : (!req.body.scope ?
            req.body.scope : xssFilters.inHTMLData(req.body.scope));
    if (!siteid) {
        global.log.error("siteid is missing");
        return response.Done({ error: true, message: "siteid is missing", status: 400 }, null, res, req);
    }
    const reqPayload = notificationsModel.getNotificationReq(req.request_id, user, notificationid, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// DELETE /customers/{orgid}/notifications/{notificationid}
// DELETE /customers/{orgid}/sites/{siteid}/notifications/{notificationid}
function deleteNotification(req, res) {
    const user = req.getCurrentUser();
    const notificationid = req.swagger.params.notificationid.value;
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid ?
        req.swagger.params.siteid.value : (!req.body.scope ?
            req.body.scope : xssFilters.inHTMLData(req.body.scope));
    if (!siteid) {
        global.log.error("siteid is missing");
        return response.Done({ error: true, message: "siteid is missing", status: 400 }, null, res, req);
    }

    const reqPayload = notificationsModel.deleteNotificationReq(req.request_id, user, notificationid, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// POST /customers/{orgid}/sites/{siteid}/notifications
function createNotification(req, res) {
    let error = false,
        message = '';
    if (req.body.resend_interval < 0) {
        error = true;
        message += " Resend interval can't be a negative number.";
    }
    if (req.body.hold_off < 0) {
        error = true;
        message += " Hold off can't be a negative number.";
    }
    if (error) {
        return response.Done({ error: true, message: message.trim(), status: 400 }, null, res, req);
    }

    const user = req.getCurrentUser();
    const userType = helpers.getUserType(user);
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid ?
        req.swagger.params.siteid.value : (!req.body.scope ?
            req.body.scope : xssFilters.inHTMLData(req.body.scope));
    if (!siteid) {
        global.log.error("siteid is missing");
        return response.Done({ error: true, message: "siteid is missing", status: 400 }, null, res, req);
    }


    const id = uuid.v1();
    const notification = {
        "rules": req.body.rules,
        "notificationtype": req.body.notificationtype,
        "severity": req.body.severity,
        "scope": siteid,
        "window": (!req.body.window ? req.body.window : xssFilters.inHTMLData(req.body.window)),
        "hold_off": req.body.hold_off,
        "resend_interval": req.body.resend_interval,
        "emailUsersList": req.body.emailUsersList,
        "smsUsersList": req.body.smsUsersList,
        "additionalEmails": req.body.additionalEmails,
        "notificationid": id,
        "description": (!req.body.description ? req.body.description : xssFilters.inHTMLData(req.body.description)),
        "active": req.body.active,
        "msg": (!req.body.msg ? req.body.msg : xssFilters.inHTMLData(req.body.msg)),
        "name": (!req.body.name ? req.body.name : xssFilters.inHTMLData(req.body.name)),
        "rule_selector": req.body.rule_selector
    };
    global.log.info("Adding notification: " + JSON.stringify(notification));
    const reqPayload = notificationsModel.createNotificationReq(req.request_id, user, notification, orgid, siteid, userType);
    sendRequest(reqPayload, req, res);
}

// POST /customers/{orgid}/notifications/{notificationid}
// POST /customers/{orgid}/sites/{siteid}/notifications/{notificationid}
function updateNotification(req, res) {
    let error = false,
        message = '';
    if (req.body.resend_interval < 0) {
        error = true;
        message += "Resend interval can't be negative number.";
    }
    if (req.body.hold_off < 0) {
        error = true;
        message += "Hold off can't be negative number.";
    }
    if (error) {
        return response.Done({ error: true, message: message, status: 400 }, null, res, req);
    }

    const user = req.getCurrentUser();
    const userType = helpers.getUserType(user);
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid ?
        req.swagger.params.siteid.value : (!req.body.scope ?
            req.body.scope : xssFilters.inHTMLData(req.body.scope));
    if (!siteid) {
        global.log.error("Siteid is missing");
        return response.Done({ error: true, message: "siteid is missing", status: 400 }, null, res, req);
    }

    const notification = {
        "rules": req.body.rules,
        "notificationtype": req.body.notificationtype,
        "severity": req.body.severity,
        "scope": siteid,
        "window": req.body.window,
        "hold_off": req.body.hold_off,
        "resend_interval": req.body.resend_interval,
        "emailUsersList": req.body.emailUsersList,
        "smsUsersList": req.body.smsUsersList,
        "additionalEmails": req.body.additionalEmails,
        "notificationid": req.swagger.params.notificationid.value,
        "description": req.body.description,
        "active": req.body.active,
        "msg": req.body.msg,
        "name": req.body.name,
        "rule_selector": req.body.rule_selector
    };
    const reqPayload = notificationsModel.updateNotificationReq(req.request_id, user, notification, orgid, siteid, userType);
    sendRequest(reqPayload, req, res);
}

function activateNotification(req, res) {
    const user = req.getCurrentUser();
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const notificationid = req.swagger.params.notificationid.value;
    const reqPayload = notificationsModel.activateNotificationReq(req.request_id, user, notificationid, true, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

function deactivateNotification(req, res) {
    const user = req.getCurrentUser();
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const notificationid = req.swagger.params.notificationid.value;

    const reqPayload = notificationsModel.activateNotificationReq(req.request_id, user, notificationid, false, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// GET /notificationtypes
function getNotificationTypes(req, res) {
    const user = req.getCurrentUser();
    const userType = helpers.getUserType(user);
    const reqPayload = notificationsModel.getNotificationTypesReq(req.request_id, userType);
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
