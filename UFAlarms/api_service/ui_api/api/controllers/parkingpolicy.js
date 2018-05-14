'use strict';

var util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid');

var response = require('./../helpers/response.js');
// var default_car_length = 4.9;

module.exports = {
    getParkingPolicy: getParkingPolicy,
    getAllParkingPolicy: getAllParkingPolicy,
    createParkingPolicy: createParkingPolicy,
    updateParkingPolicy: updateParkingPolicy,
    deleteParkingPolicy: deleteParkingPolicy,
    getParkingPolicyVersion: getParkingPolicyVersion,
    getAllVersionsOfParkingPolicy: getAllVersionsOfParkingPolicy,
    getAllActiveParkingPolicyForPeriod: getAllActiveParkingPolicyForPeriod,
    getActiveParkingPolicy: getActiveParkingPolicy,
    searchParkingPolicy: searchParkingPolicy,
    policyTagsAssociation: policyTagsAssociation,
    policyTagsDisassociation: policyTagsDisassociation
};

/* Parking Policy */

function getAllParkingPolicy(req, res) {
    //Send message to Kafka
    encoder.getAllParkingPolicy(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, function (err, msg) {
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

function getParkingPolicy(req, res) {
    //Send message to Kafka
    encoder.getParkingPolicy(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.parkingpolicyid.value, function (err, msg) {
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

function deleteParkingPolicy(req, res) {
    //Send message to Kafka
    encoder.deleteParkingPolicy(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.parkingpolicyid.value, function (err, msg) {
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

function createParkingPolicy(req, res) {
    var policy = req.body;
    //Send message to Kafka
    encoder.createParkingPolicy(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, policy, function (err, msg) {
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

function updateParkingPolicy(req, res) {
    var policy = req.body;
    //Send message to Kafka
    encoder.updateParkingPolicy(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.parkingpolicyid.value, policy, function (err, msg) {
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

function getParkingPolicyVersion(req, res) {
    //Send message to Kafka
    encoder.getParkingPolicyVersion(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.parkingpolicyid.value, req.swagger.params.version.value, function (err, msg) {
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

function getAllVersionsOfParkingPolicy(req, res) {
    //Send message to Kafka
    encoder.getAllVersionsOfParkingPolicy(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.parkingpolicyid.value, function (err, msg) {
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

function getAllActiveParkingPolicyForPeriod(req, res) {
    //Send message to Kafka
    encoder.getAllActiveParkingPolicyForPeriod(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.parkinggroupid.value, req.swagger.params.fromtime.value, req.swagger.params.totime.value, function (err, msg) {
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

function getActiveParkingPolicy(req, res) {
    //Send message to Kafka
    encoder.getActiveParkingPolicy(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.parkinggroupid.value,  function (err, msg) {
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

function searchParkingPolicy(req, res) {
    var searchPayload = req.body;
    //Send message to Kafka
    encoder.searchParkingPolicy(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, searchPayload, function (err, msg) {
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

function policyTagsAssociation(req, res) {
    var tagspolicylink = req.body;
    //Send message to Kafka
    encoder.policyTagsAssociation(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.parkingpolicyid.value, tagspolicylink, function (err, msg) {
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

function policyTagsDisassociation(req, res) {
    var tagspolicylink = req.body;
    //Send message to Kafka
    encoder.policyTagsDisassociation(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.parkingpolicyid.value, tagspolicylink, function (err, msg) {
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
/* End parking Policy */