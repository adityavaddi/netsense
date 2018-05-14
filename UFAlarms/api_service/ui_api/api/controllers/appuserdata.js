'use strict';
const encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    userDataModel = require('./../models/appuserdata.js'),
    response = require('./../helpers/response.js');

module.exports = {
    getAllAppUserData,
    getAppUserData,
    createAppUserData,
    updateAppUserData,
    deleteAppUserData
};

// GET /applications/{appid}/users/{userid}/userdata
function getAllAppUserData(req, res) {
    const appid = req.swagger.params.appid.value;
    const userid = req.swagger.params.userid.value;
    let reqPayload = userDataModel.getAllAppUserDataReq(appid, userid);
    sendRequest(reqPayload, req, res);
}

// GET /applications/{appid}/users/{userid}/userdata/{userdataid}
function getAppUserData(req, res) {
    const appid = req.swagger.params.appid.value;
    const userid = req.swagger.params.userid.value;
    const userdataid = req.swagger.params.userdataid.value;
    let reqPayload = userDataModel.getAppUserDataReq(appid, userid, userdataid);
    sendRequest(reqPayload, req, res);
}

// POST /applications/{appid}/users/{userid}/userdata
function createAppUserData(req, res) {
    const appUserData = req.body;
    const appid = req.swagger.params.appid.value;
    const userid = req.swagger.params.userid.value;
    global.log.info("Adding AppUserData: " + JSON.stringify(appUserData));
    let reqPayload = userDataModel.createAppUserDataReq(appid, userid, appUserData.datavalue);
    sendRequest(reqPayload, req, res);
}

// PUT /applications/{appid}/users/{userid}/userdata/{userdataid}
function updateAppUserData(req, res) {
    const appUserData = req.body;
    const appid = req.swagger.params.appid.value;
    const userid = req.swagger.params.userid.value;
    const userdataid = req.swagger.params.userdataid.value;
    global.log.info("Updating AppUserData: " + JSON.stringify(appUserData));
    let reqPayload = userDataModel.updateAppUserDataReq(appid, userid, userdataid, appUserData.datavalue);
    sendRequest(reqPayload, req, res);
}

// DELETE /applications/{appid}/users/{userid}/userdata/{userdataid}
function deleteAppUserData(req, res) {
    const appid = req.swagger.params.appid.value;
    const userid = req.swagger.params.userid.value;
    const userdataid = req.swagger.params.userdataid.value;
    global.log.info("Deleting AppUserData: ", userdataid);
    let reqPayload = userDataModel.deleteAppUserDataReq(appid, userid, userdataid);
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
    encoder.encodeMSRequest(req.getCurrentUser(), reqPayload, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}
