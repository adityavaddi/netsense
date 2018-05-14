'use strict';

var uuid = require('uuid'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder,
    response = require('./../helpers/response.js');

module.exports = {
    addConfig: addConfig,
    updateConfig: updateConfig,
    deleteConfig: deleteConfig,
    getConfig: getConfig,
    getAllConfigs: getAllConfigs,
    getDefaultConfigs: getDefaultConfigs,
    getDefaultConfigsForSite: getDefaultConfigsForSite,
    applyConfigToSite: applyConfigToSite,
    applyConfigToNodes: applyConfigToNodes,
    applyConfigToNode: applyConfigToNode,
    applyConfigToGroup: applyConfigToGroup,
    getConfigFromNode: getConfigFromNode,
    updateVPNInfo: updateVPNInfo,
    connectToVPN: connectToVPN,
    disconnectFromVPN: disconnectFromVPN
};

function addConfig(req, res) {
    var orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        config = req.body;

    config.configid = uuid.v1();

    encoder.addConfig(req.request_id, req.getCurrentUser(), orgid, siteid, config, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}

function updateConfig(req, res) {
    var orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        configid = req.swagger.params.configid.value,
        config = req.body;

    config.configid = configid;

    encoder.updateConfig(req.request_id, req.getCurrentUser(), orgid, siteid, config, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getConfigFromNode(req,res) {
    var orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        nodeid = req.swagger.params.nodeid.value,
        type = req.swagger.params.type.value;

    if (!(type == "prov" || type == "local" || type == "default")) {
      return response.Done({error: true, message: type + ' is not a valid type', statuus: 400}, null, res, req);
    }

    encoder.getConfigFromNode(req.request_id, req.getCurrentUser(), orgid, siteid, nodeid, type, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}

function deleteConfig(req, res) {
    var orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        configid = req.swagger.params.configid.value;

    encoder.deleteConfig(req.request_id, req.getCurrentUser(), orgid, siteid, configid, function(err, msg) {
        response.Done(err, null, res, req);
    });
}

function getConfig(req, res) {
    var orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        configid = req.swagger.params.configid.value;

    encoder.getConfig(req.request_id, req.getCurrentUser(), orgid, siteid, configid, function(err, msg) {
      if(err) {
        response.Done(err, msg, res, req);
      } else {
        var resp = JSON.parse(JSON.stringify(msg));
        if(resp.cfgid) {
          resp.configid = resp.cfgid;
          delete resp.cfgid;
        }
        response.Done(err, resp, res, req);
      }
    });
}

function getAllConfigs(req, res) {
    var orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value;

    encoder.getAllConfigs(req.request_id, req.getCurrentUser(), orgid, siteid, function(err, msg) {
      if(err) {
        response.Done(err, msg, res, req);
      } else {
        var resp = JSON.parse(JSON.stringify(msg));

        for(var i=0;i<resp.length;i++) {
          if(resp[i].cfgid) {
            resp[i].configid = resp[i].cfgid;
            delete resp[i].cfgid;
          }
        }
        response.Done(err, resp, res, req);
      }
    });
}

function getDefaultConfigsForSite(req, res) {
    var model = req.swagger.params.model.value;
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;

    encoder.getDefaultConfigsForSite(req.request_id, req.getCurrentUser(), orgid, siteid, model, function(err, msg) {
      if(err) {
        response.Done(err, msg, res, req);
      } else {
        var resp = JSON.parse(JSON.stringify(msg));
        if(resp.cfgid) {
          delete resp.cfgid;
        }
        if(resp.configid) {
          delete resp.configid;
        }
        response.Done(err, resp, res, req);
      }
    });
}

function getDefaultConfigs(req, res) {
    var model = req.swagger.params.model.value;

    encoder.getDefaultConfigs(req.request_id, req.getCurrentUser(), model, function(err, msg) {
      if(err) {
        response.Done(err, msg, res, req);
      } else {
        var resp = JSON.parse(JSON.stringify(msg));
        if(resp.cfgid) {
          delete resp.cfgid;
        }
        if(resp.configid) {
          delete resp.configid;
        }
        response.Done(err, resp, res, req);
      }
    });
}

function applyConfigToNode(req, res) {
    var siteid = req.swagger.params.siteid.value,
        orgid = req.swagger.params.orgid.value,
        nodeid = req.swagger.params.nodeid.value,
        configid = req.swagger.params.configid.value;

    encoder.applyConfigToNode(req.request_id, req.getCurrentUser(), configid, siteid, orgid, nodeid, function(err, result) {
        response.Done(err, result, res, req);
    });
}

function applyConfigToSite(req, res) {
    var siteid = req.swagger.params.siteid.value,
        orgid = req.swagger.params.orgid.value,
        configid = req.swagger.params.configid.value;

    encoder.applyConfigToSite(req.request_id, req.getCurrentUser(), configid, siteid, orgid, function(err, result) {
        response.Done(err, result, res, req);
    });
}

function applyConfigToGroup(req, res) {
    var siteid = req.swagger.params.siteid.value,
        orgid = req.swagger.params.orgid.value,
        groupid = req.swagger.params.groupid.value,
        configid = req.swagger.params.configid.value;

    encoder.applyConfigToGroup(req.request_id, req.getCurrentUser(), configid, siteid, orgid, groupid, function(err, result) {
        response.Done(err, result, res, req);
    });
}

function applyConfigToNodes(req, res) {
    var siteid = req.swagger.params.siteid.value,
        orgid = req.swagger.params.orgid.value,
        configid = req.swagger.params.configid.value,
        nodeids = req.body.nodeList;

    encoder.applyConfigToNodes(req.request_id, req.getCurrentUser(), configid, siteid, orgid, nodeids, function(err, result) {
        response.Done(err, result, res, req);
    });
}

// --- VPN -----
function updateVPNInfo(req, res) {
    var siteid  = req.swagger.params.siteid.value,
        nodeid  = req.swagger.params.nodeid.value,
        command = "query";

    encoder.updateVPNInfo(req.request_id, req.getCurrentUser(), siteid, nodeid, command, function(err, msg) {
        if(err) {
            response.Done(err, null, res, req);
        }
        else {
            var resp = JSON.parse(JSON.stringify(msg));
            response.Done(err, resp, res, req);
        }
    });
}

function connectToVPN(req, res) {
    var siteid  = req.swagger.params.siteid.value,
        nodeid  = req.swagger.params.nodeid.value,
        command = "start";

    encoder.connectToVPN(req.request_id, req.getCurrentUser(), siteid, nodeid, command, function(err, msg) {
        response.Done(err, null, res, req);
    });
}

function disconnectFromVPN(req, res) {
    var siteid  = req.swagger.params.siteid.value,
        nodeid  = req.swagger.params.nodeid.value,
        command = "stop";

    encoder.disconnectFromVPN(req.request_id, req.getCurrentUser(), siteid, nodeid, command, function(err, msg) {
        response.Done(err, null, res, req);
    });
}