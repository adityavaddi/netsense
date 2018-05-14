"use strict";

const encoder = require("./../../encoder/Encoder.js").JSONEncoder,
    whatIfModel = require("./../models/whatifanalysis.js"),
    response = require("./../helpers/response.js");

// Export all the functions to use it in swagger.yaml
module.exports = {
    getAllWhatIfJobs,
    getWhatIfJob,
    createWhatIfJob,
    updateWhatIfJob,
    abortWhatIfJob,
    deleteWhatIfJob
};

// GET /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/jobs
function getAllWhatIfJobs(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    let reqPayload = whatIfModel.getAllWhatIfJobsReq(req.request_id, orgid, siteid);
    let filterQuery = req.swagger.params.filter.value;
    // To remove quotes (single & double) from filter query
    filterQuery = filterQuery ? filterQuery.replace(/["']/g, "") : null;
    if (filterQuery && filterQuery.trim().length > 0) {
        reqPayload = whatIfModel.getAllFilteredWhatIfJobsReq(req.request_id, filterQuery, orgid, siteid);
    }
    sendRequest(reqPayload, req, res);
}

// GET /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/jobs/{jobid}
function getWhatIfJob(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const jobid = req.swagger.params.jobid.value;
    const reqPayload = whatIfModel.getWhatIfJobReq(req.request_id, jobid, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// POST /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/jobs
function createWhatIfJob(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const userEmail = req.getCurrentUser().user.email;
    let job = req.body;
    const reqPayload = whatIfModel.createWhatIfJobReq(req.request_id, job, orgid, siteid, userEmail);
    sendRequest(reqPayload, req, res);
}

// PUT /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/jobs/{jobid}
function updateWhatIfJob(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const jobid = req.swagger.params.jobid.value;
    const userEmail = req.getCurrentUser().user.email;
    let job = req.body;
    const reqPayload = whatIfModel.updateWhatIfJobReq(req.request_id, jobid, job, orgid, siteid, userEmail);
    sendRequest(reqPayload, req, res);
}

// GET /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/jobs/{jobid}/abort:
function abortWhatIfJob(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const jobid = req.swagger.params.jobid.value;
    const reqPayload = whatIfModel.abortWhatIfJobReq(req.request_id, jobid, orgid, siteid);
    sendRequest(reqPayload, req, res);
}

// DELETE /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/jobs/{jobid}:
function deleteWhatIfJob(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const jobid = req.swagger.params.jobid.value;
    const reqPayload = whatIfModel.deleteWhatIfJobReq(req.request_id, jobid, orgid, siteid);
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