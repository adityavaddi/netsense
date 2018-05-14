'use strict';

var util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid');

var response = require('./../helpers/response.js');

module.exports = {
  getCustomer: getCustomer,
  getAllCustomers: getAllCustomers,
  getAllSuspendedCustomers: getAllSuspendedCustomers,
  addCustomer: addCustomer,
  deleteCustomer: deleteCustomer,
  suspendCustomer: suspendCustomer,
  activateCustomer: activateCustomer,
  updateCustomer: updateCustomer,
  addCustomerOf: addCustomerOf
};

function getCustomer(req, res) {
  encoder.getOrg(req.getCurrentUser(), req.swagger.params.orgid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function getAllCustomers(req, res) {
    global.log.debug("in getAllCustomers");
  encoder.getAllOrgs(req.getCurrentUser(), function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function addCustomer(req, res) {
  var new_id = uuid.v1();
  var org = req.body;
  var allowed_types = ['', 'partner', 'default'];
  if(org.type && allowed_types.indexOf(org.type)===-1)
    return response.Done({status:400, message:"Request contains invalid type "+org.type}, null, res, req);
  org.orgid = new_id;
  global.log.info("Adding customer: " + JSON.stringify(org));
  org.po = req.getCurrentUser().user.orgs[0];
  encoder.createOrg(req.getCurrentUser(), org, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function addCustomerOf(req, res) {
  var new_id = uuid.v1();
  var org = req.body;
  var allowed_types = ['', 'partner', 'default'];
  if(org.type && allowed_types.indexOf(org.type)===-1)
      return response.Done({status:400, message:"Request contains invalid type "+org.type}, null, res, req);
  org.orgid = new_id;
  var parentorg = req.swagger.params.orgid.value;
  if(parentorg)
      org.po = parentorg;
  global.log.info("Adding customer: " + JSON.stringify(org));
  encoder.createOrg(req.getCurrentUser(), org, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function updateCustomer(req, res) {
  var org = req.body;
  var allowed_types = ['', 'partner', 'default'];
  if(org.type && allowed_types.indexOf(org.type)===-1)
      return response.Done({status:400, message:"Request contains invalid type "+org.type}, null, res, req);
  org.orgid = req.swagger.params.orgid.value;
  global.log.info("Updating customer: " + JSON.stringify(org));
  encoder.updateOrg(req.getCurrentUser(), org, function (err, msg) {
    response.Done(err, msg, res, req);
  });

}

function deleteCustomer(req, res) {
  var orgid = req.swagger.params.orgid.value;
  global.log.info("Deleting customer: ", orgid);
  encoder.deleteOrg(req.getCurrentUser(), orgid, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function suspendCustomer(req, res) {
  var orgid = req.swagger.params.orgid.value;
  global.log.info("Suspending customer: ", orgid);
  encoder.suspendOrg(req.getCurrentUser(), orgid, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function activateCustomer(req, res) {
  var orgid = req.swagger.params.orgid.value;
  global.log.info("Activating customer: ", orgid);
  encoder.activateOrg(req.getCurrentUser(), orgid, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function getAllSuspendedCustomers(req, res) {
  encoder.getAllSuspendedOrgs(req.getCurrentUser(), function (err, msg) {
    response.Done(err, msg, res, req);
  });
}
