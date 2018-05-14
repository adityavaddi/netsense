'use strict';

var util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid');

var response = require('./../helpers/response.js');

module.exports = {
    getAllPolicyCategory: getAllPolicyCategory,
    getPolicyCategory: getPolicyCategory,
    deletePolicyCategory: deletePolicyCategory,
    createPolicyCategory: createPolicyCategory,
    updatePolicyCategory: updatePolicyCategory
};

/* Parking policy tags */

function getAllPolicyCategory(req, res) {
    //Send message to Kafka
    encoder.getAllPolicyCategory(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, function (err, msg) {
        if (!err) {
            response.Done(err, msg, res, req);
        } else {
            if (err.message)
                response.Done(err, null, res, req);
            else
                response.Done({error: true, status: 404, message: 'Not found.'}, [], res, req);
        }
    });
}

function getPolicyCategory(req, res) {
    //Send message to Kafka
    encoder.getPolicyCategory(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.tagid.value, function (err, msg) {
        if (!err) {
            response.Done(err, msg, res, req);
        } else {
            if (err.message)
                response.Done(err, null, res, req);
            else
                response.Done({error: true, status: 404, message: 'Not found.'}, [], res, req);
        }
    });
}

function deletePolicyCategory(req, res) {
    //Send message to Kafka
    encoder.deletePolicyCategory(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.tagid.value, function (err, msg) {
        if (!err) {
            response.Done(err, msg, res, req);
        } else {
            if (err.message)
                response.Done(err, null, res, req);
            else
                response.Done({error: true, status: 404, message: 'Not found.'}, [], res, req);
        }
    });
}

function createPolicyCategory(req, res) {
    var tag = req.body;
    //Send message to Kafka
    encoder.createPolicyCategory(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, tag, function (err, msg) {
        if (!err) {
            response.Done(err, msg, res, req);
        } else {
            if (err.message)
                response.Done(err, null, res, req);
            else
                response.Done({error: true, status: 404, message: 'Not found.'}, [], res, req);
        }
    });
}

function updatePolicyCategory(req, res) {
    var tag = req.body;
    //Send message to Kafka
    encoder.updatePolicyCategory(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.tagid.value, tag, function (err, msg) {
        if (!err) {
            response.Done(err, msg, res, req);
        } else {
            if (err.message)
                response.Done(err, null, res, req);
            else
                response.Done({error: true, status: 404, message: 'Not found.'}, [], res, req);
        }
    });
}
/* End Parking policy tags */