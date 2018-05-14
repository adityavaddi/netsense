'use strict';

var util = require('util'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    uuid = require('uuid');

var response = require('./../helpers/response.js');

module.exports = {
  getFixture: getFixture,
  getAllFixtures: getAllFixtures,
  addFixture: addFixture,
  deleteFixture: deleteFixture,
  updateFixture: updateFixture,
  assignFixtureToNode: assignFixtureToNode,
  assignFixtureToNodes: assignFixtureToNodes,
  assignFixtureToSite: assignFixtureToSite,
  assignFixtureToGroup: assignFixtureToGroup,
  assignFixtureToGroups: assignFixtureToGroups
};

function getAllFixtures(req, res) {
  encoder.getAllFixtures(req.session.CurrentUser, req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
} 


function getFixture(req, res) {
  encoder.getFixture(req.session.CurrentUser, req.swagger.params.fixtureid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function deleteFixture(req, res) {
  encoder.deleteFixture(req.session.CurrentUser, req.swagger.params.fixtureid.value, req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, null, res, req);
  });
}

function validateMinMaxPowerValues(fixture) {
  const invalidValues = [];
  if (fixture.MinPower0 != undefined && fixture.MinPower0.trim().length > 0) fixture.MinPower0 = validatePowerValue("MinPower0", fixture.MinPower0);
  if (fixture.MinPower10 != undefined && fixture.MinPower10.trim().length > 0) fixture.MinPower10 = validatePowerValue("MinPower10", fixture.MinPower10);
  if (fixture.MinPower50 != undefined && fixture.MinPower50.trim().length > 0) fixture.MinPower50 = validatePowerValue("MinPower50", fixture.MinPower50);
  if (fixture.MinPower100 != undefined && fixture.MinPower100.trim().length > 0) fixture.MinPower100 = validatePowerValue("MinPower100", fixture.MinPower100);
  if (fixture.MaxPower0 != undefined && fixture.MaxPower0.trim().length > 0) fixture.MaxPower0 = validatePowerValue("MaxPower0", fixture.MaxPower0);
  if (fixture.MaxPower10 != undefined && fixture.MaxPower10.trim().length > 0) fixture.MaxPower10 = validatePowerValue("MaxPower10", fixture.MaxPower10);
  if (fixture.MaxPower50 != undefined && fixture.MaxPower50.trim().length > 0) fixture.MaxPower50 = validatePowerValue("MaxPower50", fixture.MaxPower50);
  if (fixture.MaxPower100 != undefined && fixture.MaxPower100.trim().length > 0) fixture.MaxPower100 = validatePowerValue("MaxPower100", fixture.MaxPower100);

  function validatePowerValue(key, value) {
    const isValidValue = !isNaN(value);
    if (!isValidValue) invalidValues.push(key);
    return value;
  };

  if (invalidValues.length > 0) {
    return { error: true, msg: `Request contains invalid values for ${invalidValues}`, status: 400 };
  }
  return fixture;
}

function addFixture(req, res) {
  var id = uuid.v1();
  var fixture = {
    "name":req.body.name,
    "description":req.body.description,
    "manufacturer":req.body.manufacturer,
    "manufacturersku":req.body.manufacturersku,
    "fixtureid": id,
    "fixtureType": req.body.fixtureType,
    "nemasocket": req.body.nemasocket,
    "MaxPower0": req.body.MaxPower0,
    "MaxPower10": req.body.MaxPower10,
    "MaxPower50": req.body.MaxPower50,
    "MaxPower100": req.body.MaxPower100,
    "MinPower100": req.body.MinPower100,
    "MinPower50": req.body.MinPower50,
    "MinPower10": req.body.MinPower10,
    "MinPower0": req.body.MinPower0,
    "PowerDraw": req.body.PowerDraw,
    "MinimumLightLevelForFailureDetection": req.body.MinimumLightLevelForFailureDetection,
    "BallastCost": req.body.BallastCost,
    "BulbCost": req.body.BulbCost,
    "LegacyPowerDraw": req.body.LegacyPowerDraw,
    "DailyOperatingTime": req.body.DailyOperatingTime,
  };  
  global.log.info("Adding fixture: " + JSON.stringify(fixture));
  // Validate min and max power values
  const result = validateMinMaxPowerValues(fixture);
  if (result.error) return response.Done(result, null, res, req);
  encoder.createFixture(req.session.CurrentUser, result, req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function updateFixture(req, res) {
  var fixture = {
    "name":req.body.name,
    "description":req.body.description,
    "manufacturer":req.body.manufacturer,
    "manufacturersku":req.body.manufacturersku,
    "fixtureid": req.swagger.params.fixtureid.value,
    "fixtureType": req.body.fixtureType,
    "nemasocket": req.body.nemasocket,
    "MaxPower0": req.body.MaxPower0,
    "MaxPower10": req.body.MaxPower10,
    "MaxPower50": req.body.MaxPower50,
    "MaxPower100": req.body.MaxPower100,
    "MinPower100": req.body.MinPower100,
    "MinPower50": req.body.MinPower50,
    "MinPower10": req.body.MinPower10,
    "MinPower0": req.body.MinPower0,
    "PowerDraw": req.body.PowerDraw,
    "MinimumLightLevelForFailureDetection": req.body.MinimumLightLevelForFailureDetection,
    "BallastCost": req.body.BallastCost,
    "BulbCost": req.body.BulbCost,
    "LegacyPowerDraw": req.body.LegacyPowerDraw,
    "DailyOperatingTime": req.body.DailyOperatingTime,
  };
  // Validate min and max power values
  const result = validateMinMaxPowerValues(fixture);
  if (result.error) return response.Done(result, null, res, req);
  encoder.updateFixture(req.session.CurrentUser, result, req.swagger.params.siteid.value, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function assignFixtureToNode(req, res) {
  var fixtureid = req.swagger.params.fixtureid.value;
  var nodeid = req.swagger.params.nodeid.value;
  var siteid = req.swagger.params.siteid.value;
  encoder.assignFixture(req.session.CurrentUser, fixtureid, nodeid, siteid, null, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function assignFixtureToNodes(req, res) {
  var fixtureid = req.swagger.params.fixtureid.value;
  var siteid = req.swagger.params.siteid.value;
  var nodeids = req.swagger.params.nodeList.value.nodeList;
  encoder.assignFixtureToNodes(req.session.CurrentUser, fixtureid, nodeids, siteid, null, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function assignFixtureToSite(req, res) {
  var fixtureid = req.swagger.params.fixtureid.value;
  var siteid = req.swagger.params.siteid.value;
  encoder.assignFixture(req.session.CurrentUser, fixtureid, null, siteid, null, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function assignFixtureToGroup(req, res) {
  var fixtureid = req.swagger.params.fixtureid.value;
  var siteid = req.swagger.params.siteid.value;
  var groupid = req.swagger.params.groupid.value;
  encoder.assignFixture(req.session.CurrentUser, fixtureid, null, siteid, [groupid], function (err, msg) {
    response.Done(err, msg, res, req);
  });
}

function assignFixtureToGroups(req, res) {
  var fixtureid = req.swagger.params.fixtureid.value;
  var siteid = req.swagger.params.siteid.value;
  var groupids = req.body.groupList;
  encoder.assignFixture(req.session.CurrentUser, fixtureid, null, siteid, groupids, function (err, msg) {
    response.Done(err, msg, res, req);
  });
}
