'use strict';
const spawn = require('child_process').spawn;
var util = require('util');
var response = require('./../helpers/response.js');

module.exports = {
  commands: smsCommand
};

function smsCommand(req, res) {
  var nodeid = req.swagger.params.nodeid.value;
  var cmd = req.swagger.params.cmd.value;
  var p1 = req.swagger.params.p1.value;
  var p2 = req.swagger.params.p2.value;
  var p3 = req.swagger.params.p3.value;
  var p4 = req.swagger.params.p4.value;
  returnSMSCommandOutput(nodeid,cmd,p1,p2,p3, p4,res, req);
}

function returnSMSCommandOutput(nodeid,cmd,p1,p2,p3,p4,res, req) {
  var responseBuffer = "";
  if(req.session.CurrentUser.authorization.indexOf("CAN_UPGRADE_FIRMWARE")===-1) {
    response.Done(true, "User is not authorized to send commands", res, req);
  }
  if(cmd==='read' && p1) p1 = "--last "+p1;
  if(cmd==='send') p1 = '-c \"'+p1+'\"';
  const pigeonwire = spawn('pigeonwire', [nodeid, cmd, p1, p2, p3, p4]);

  pigeonwire.stdout.on('data', (data) => {
     responseBuffer+=data;
  });

  pigeonwire.stderr.on('data', (data) => {
    responseBuffer+=data;
  });

  pigeonwire.on('close', (code) => {
    response.Done(code, responseBuffer, res, req);
  });
}
