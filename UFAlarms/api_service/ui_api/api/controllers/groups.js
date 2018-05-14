'use strict';

var util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid');

var response = require('./../helpers/response.js');

module.exports = {
  getGroup: getGroup,
  getAllGroups: getAllGroups,
  addGroup: addGroup,
  deleteGroup: deleteGroup,
  updateGroup: updateGroup,
  addNodeToGroup: addNodeToGroup,
  removeNodeFromGroup: removeNodeFromGroup,
  resendScheduleToLG: resendScheduleToLG
};

function getAllGroups(req, res) {
  encoder.getAllGroups(req.session.CurrentUser, req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}


function getGroup(req, res) {
  encoder.getGroup(req.session.CurrentUser, req.swagger.params.gid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function deleteGroup(req, res) {
  encoder.deleteGroup(req.session.CurrentUser, req.swagger.params.gid.value, req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function addGroup(req, res) {
  var id = uuid.v1();
  var grp = req.body;
  grp.groupid = id;
  global.log.info("Adding group: " + JSON.stringify(grp));
  encoder.createGroup(req.session.CurrentUser, grp, req.swagger.params.orgid.value, req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function updateGroup(req, res) {
  var grp = req.body;
  grp.groupid = req.swagger.params.gid.value;
  encoder.updateGroup(req.session.CurrentUser, grp, req.swagger.params.orgid.value, req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function addNodeToGroup(req, res) {
  var grp = {
    "groupid": req.swagger.params.groupid.value,
    "nodeid": req.swagger.params.nodeid.value
  };
  encoder.addNodeToGroup(req.session.CurrentUser, grp, req.swagger.params.orgid.value, req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function removeNodeFromGroup(req, res) {
  var grp = {
    "groupid": req.swagger.params.groupid.value,
    "nodeid": req.swagger.params.nodeid.value
  };
  encoder.removeNodeFromGroup(req.session.CurrentUser, grp, req.swagger.params.orgid.value, req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function resendScheduleToLG(req, res) {
    var user = req.session.CurrentUser,
        orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        groupid = req.swagger.params.gid.value;
    encoder.resendScheduleToLG(user,
                               orgid,
                               siteid,
                               groupid,
                               function (err, msg) {
                                   response.Done(err, null, res, req);
                               });
}
