'use strict';

var util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid');

var response = require('./../helpers/response.js');

module.exports = {
  getOverlay: getOverlay,
  getAllOverlays: getAllOverlays,
  addOverlay: addOverlay,
  deleteOverlay: deleteOverlay,
  updateOverlay: updateOverlay
};

function getAllOverlays(req, res) {
  encoder.getAllOverlays(req.getCurrentUser(), req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
} 


function getOverlay(req, res) {
  encoder.getOverlay(req.getCurrentUser(), req.swagger.params.overlayid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function deleteOverlay(req, res) {
  encoder.deleteOverlay(req.getCurrentUser(), req.swagger.params.overlayid.value, req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, null, res, req);
  });
}

function addOverlay(req, res) {
  var id = uuid.v1();
  var ovl = req.body;
  ovl.overlayid = id;
  global.log.info("Adding overlay: " + JSON.stringify(ovl));
  encoder.createOverlay(req.getCurrentUser(), ovl, req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function updateOverlay(req, res) {
  var ovl = req.body;
  ovl.overlayid = req.swagger.params.overlayid.value;
  encoder.updateOverlay(req.getCurrentUser(), ovl, req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}