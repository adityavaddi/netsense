'use strict';

var util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid');

var response = require('./../helpers/response.js');

module.exports = {
    associatedParkingGroups: associatedParkingGroups,
    policyAssociation: policyAssociation,
    policyDisassociation: policyDisassociation
};

/* Parking group policy */

function associatedParkingGroups(req, res) {
    //Send message to Kafka
    encoder.associatedParkingGroups(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.parkingpolicyid.value, function (err, msg) {
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

function policyAssociation(req, res) {
    var ParkingGroupPolicyLink = req.body;
    //Send message to Kafka
    encoder.policyAssociation(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.parkingpolicyid.value, ParkingGroupPolicyLink, function (err, msg) {
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

function policyDisassociation(req, res) {
    var ParkingGroupPolicyLink = req.body;
    //Send message to Kafka
    encoder.policyDisassociation(req.request_id, req.getCurrentUser(),req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.swagger.params.parkingpolicyid.value, ParkingGroupPolicyLink, function (err, msg) {
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