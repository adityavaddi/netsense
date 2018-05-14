"use strict";

const encoder = require("./../../encoder/Encoder.js").JSONEncoder,
    whatIfModel = require("./../models/whatiftag.js"),
    response = require("./../helpers/response.js");

// Export all the functions to use it in swagger.yaml
module.exports = {
    getAllWhatIfTags,
    getWhatIfTag,
    createWhatIfTag,
    updateWhatIfTag,
    deleteWhatIfTag
};

// GET /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/tags
function getAllWhatIfTags(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const reqPayload = whatIfModel.getAllWhatIfTagsReq(req.request_id, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// GET /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/tags/{tagid}
function getWhatIfTag(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const tagid = req.swagger.params.tagid.value;
    const reqPayload = whatIfModel.getWhatIfTagReq(req.request_id, tagid, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// POST /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/tags
function createWhatIfTag(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    let whatIfTag = req.body;
    const reqPayload = whatIfModel.createWhatIfTagReq(req.request_id, whatIfTag, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// PUT /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/tags/{tagid}
function updateWhatIfTag(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const tagid = req.swagger.params.tagid.value;
    let whatIfTag = req.body;
    const reqPayload = whatIfModel.updateWhatIfTagReq(req.request_id, tagid, whatIfTag, orgid, siteid);
    sendRequest(reqPayload, req, res);
}


// DELETE /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/tags/{tagid}:
function deleteWhatIfTag(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const tagid = req.swagger.params.tagid.value;
    const reqPayload = whatIfModel.deleteWhatIfTagReq(req.request_id, tagid, orgid, siteid);
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