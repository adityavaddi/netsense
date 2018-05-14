"use strict";

const encoder = require("./../../encoder/Encoder.js").JSONEncoder,
    whatIfModel = require("./../models/whatifpolicy.js"),
    response = require("./../helpers/response.js");

// Export all the functions to use it in swagger.yaml
module.exports = {
    getAllWhatIfPolicies,
    getWhatIfPolicy,
    createWhatIfPolicy,
    updateWhatIfPolicy,
    deleteWhatIfPolicy,
    whatIfPolicyTagsAssociation,
    whatIfPolicyTagsDisassociation
};

// GET /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/parkingpolicies
function getAllWhatIfPolicies(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const reqPayload = whatIfModel.getAllWhatIfPoliciesReq(req.request_id, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// GET /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/parkingpolicies/{policyid}
function getWhatIfPolicy(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const policyid = req.swagger.params.policyid.value;
    const reqPayload = whatIfModel.getWhatIfPolicyReq(req.request_id, policyid, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// POST /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/parkingpolicies
function createWhatIfPolicy(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const whatIfPolicy = req.body;
    const reqPayload = whatIfModel.createWhatIfPolicyReq(req.request_id, whatIfPolicy, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// PUT /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/parkingpolicies/{policyid}
function updateWhatIfPolicy(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const policyid = req.swagger.params.policyid.value;
    const whatIfPolicy = req.body;
    const reqPayload = whatIfModel.updateWhatIfPolicyReq(req.request_id, policyid, whatIfPolicy, orgid, siteid);
    sendRequest(reqPayload, req, res);
}


// DELETE /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/parkingpolicies/{policyid}
function deleteWhatIfPolicy(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const policyid = req.swagger.params.policyid.value;
    const reqPayload = whatIfModel.deleteWhatIfPolicyReq(req.request_id, policyid, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// PUT /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/parkingpolicies/{policyid}/associatedtags
function whatIfPolicyTagsAssociation(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const policyid = req.swagger.params.policyid.value;
    const tagspolicylink = req.body;
    const reqPayload = whatIfModel.whatIfPolicyTagsAssociationReq(req.request_id, policyid, tagspolicylink, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// DELETE /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/parkingpolicies/{policyid}/associatedtags
function whatIfPolicyTagsDisassociation(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const policyid = req.swagger.params.policyid.value;
    const tagspolicylink = req.body;
    const reqPayload = whatIfModel.whatIfPolicyTagsDisassociationReq(req.request_id, policyid, tagspolicylink, orgid, siteid);
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