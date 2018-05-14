'use strict';

var util = require('util'),
    encoder = require('./../../encoder/Encoder.js')
    .JSONEncoder;

var fs = require('fs');

var response = require('./../helpers/response.js');

module.exports = {
    getNode: getNode,
    getAllNodes: getAllNodes,
    addNode: addNode,
    deleteNode: deleteNode,
    bulkDeleteNode: bulkDeleteNode,
    updateNode: updateNode,
    createNode: createNode,
    bulkCreateNode: bulkCreateNode,
    activateNode: activateNode,
    deactivateNode: deactivateNode,
    assignNode: assignNode,
    bulkAssignNode: bulkAssignNode,
    getAllMinNodes: getAllMinNodes,
    getSensorHistory: getSensorHistory,
    getSensorHistoryFromTo: getSensorHistoryFromTo,
    getSiteSensorHistoryFromTo: getSiteSensorHistoryFromTo,
    getNodeConnectionStatus: getNodeConnectionStatus,
    getSiteNodesStatuses: getSiteNodesStatuses,
    getNodeLightStatus: getNodeLightStatus,
    commandNode: commandNode,
    redirectNodes: redirectNodes,
    resendScheduleToNode: resendScheduleToNode
};

function activateNode(req, res) {
    encoder.activateNode(req.getCurrentUser(), req.swagger.params.siteid.value, req.swagger.params.nodeid.value, true, function(err, msg) {
        response.Done(err, null, res, req);
    });
}

function deactivateNode(req, res) {
    encoder.activateNode(req.getCurrentUser(), req.swagger.params.siteid.value, req.swagger.params.nodeid.value, false, function(err, msg) {
        response.Done(err, null, res, req);
    });

}

function commandNode(req, res) {
    global.log.info("User %s issuing command %s for nodes [%s]", JSON.stringify(req.getCurrentUser()), req.swagger.params.cmd.value, JSON.stringify(req.body.nodeList));
    encoder.commandNode(req.request_id, req.getCurrentUser(), req.swagger.params.orgid.value, req.swagger.params.siteid.value, req.body.nodeList, req.swagger.params.cmd.value, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getNode(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    const nodeid = req.swagger.params.nodeid.value;
    encoder.getNode(req.request_id, req.getCurrentUser(), nodeid, orgid, siteid, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getAllNodes(req, res) {
    const orgid = req.swagger.params.orgid.value;
    const siteid = req.swagger.params.siteid.value;
    encoder.getAllNodes(req.request_id, req.getCurrentUser(), orgid, siteid, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getAllMinNodes(req, res) {
    const orgid = req.swagger.params.cid.value;
    const siteid = req.swagger.params.sid.value;
    encoder.getAllMinNodes(req.request_id, req.getCurrentUser(), orgid, siteid, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}

function addNode(req, res) {
    var siteid = req.swagger.params.siteid.value,
        orgid = req.swagger.params.orgid.value,
        node = req.body;

      if(node.model &&
        node.model != 'unode-v5' && node.model != 'unode-v6'){
        // Check v5 only params
        var v5only = ['apn', 'iccid', 'imei', 'imsi', ];
        for (var i=0; i<v5only.length; i++) {
            var param = v5only[i];
            if(node[param]!==undefined){
                return response.Done({message: `Invalid param '${param} for node model '${node.model}'`, status: 400, error: true}, null, res, req);
            }
        };
    }

    global.log.info("Adding node: " + JSON.stringify(node));
    encoder.createNode(req.getCurrentUser(), node, siteid, orgid, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}

function bulkCreateNode(req, res) {
    var data = null,
        nodes = [],
        extprops = {
            emptynode: true
        };
    if(req.files)
        data = req.files[0].buffer.toString('utf-8');
    else
        data = req.body.csvNodeList;

    if (data) {
        var nodesArray = data.split(/\r\n|\r|\n/g),
            nodeArray,
            node,
            params,
            nodeAttributes = nodesArray[0].split(",");

        for (var j = 0; j < nodeAttributes.length; j++) {
            if (nodeAttributes[j] === "siteid") {
                extprops.emptynode = false;
            }
        }
        for (var i = 1; i < nodesArray.length; i++) {
            nodeArray = nodesArray[i].split(",");
            node = {};
            for (var j = 0; j < nodeAttributes.length; j++) {
                node[nodeAttributes[j]] = nodeArray[j];
            }
            if (node.nodeid && node.nodeid !== "") {
                nodes.push(node);
            }
        }
    }
    //global.log.info("Nodes: ", nodes);
    encoder.createBulkNode(req.getCurrentUser(), nodes, extprops, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}

function assignNode(req, res) {
    var siteid = req.swagger.params.siteid.value,
        orgid = req.swagger.params.orgid.value,
        nodeid = req.swagger.params.nodeid.value;

    global.log.info("Assigning node: " + nodeid);
    encoder.assignNode(req.getCurrentUser(), nodeid, siteid, orgid, function(err, result) {
        /*if (!err) {
            //res.json(result);
            response.Done({error: true, status: 404, message: 'Not found.'}, [], res, req);
        } else {
            res.status(403)
                .json(err);
        }
        res.end();*/

        response.Done(err, result, res, req);
    });
}

function redirectNodes(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;
    var nodeList = req.body.nodeList;
    var server = req.body.server;
    encoder.applyServerToNodes(req.getCurrentUser(), server, siteid, orgid, nodeList, function(err, result) {
        response.Done(err, result, res, req);
    });
}

function bulkAssignNode(req, res) {
    var siteid = req.swagger.params.siteid.value,
        orgid = req.swagger.params.orgid.value,
        data = req.files[0].buffer.toString('utf-8');

    if (data) {
        var nodesArray = data.split(/\r\n|\r|\n/g),
            nodeArray,
            node,
            params,
            nodeAttributes = nodesArray[0].split(",");

        for (var i = 1; i < nodesArray.length; i++) {
            nodeArray = nodesArray[i].split(",");
            node = {};
            for (var j = 0; j < nodeAttributes.length; j++) {
                node[nodeAttributes[j]] = nodeArray[j];
            }
            if (node.nodeid && node.nodeid !== "") {
                encoder.assignNode(req.getCurrentUser(), node.nodeid, siteid, orgid, function(err, result) {});
            }
        }
        response.Done(null, "Nodes processed", res, req);
    } else {
        response.Done({
            message: "File not uploaded",
            error: true,
            status: 404
        }, null, res, req);
    };
}

function bulkDeleteNode(req, res) {
    var siteid = req.swagger.params.siteid.value,
        orgid = req.swagger.params.orgid.value,
        data = req.files[0].buffer.toString('utf-8');

    if (data) {
        var nodesArray = data.split(/\r\n|\r|\n/g),
            nodeArray,
            node,
            params,
            nodeAttributes = nodesArray[0].split(",");

        for (var i = 1; i < nodesArray.length; i++) {
            nodeArray = nodesArray[i].split(",");
            node = {};
            for (var j = 0; j < nodeAttributes.length; j++) {
                node[nodeAttributes[j]] = nodeArray[j];
            }
            if (node.nodeid && node.nodeid !== "") {
                encoder.deleteNode(req.getCurrentUser(), node.nodeid, siteid, orgid, function(err, result) {});
            }
        }
        response.Done(null, "Nodes processed", res, req);
    } else {
        response.Done({
            message: "File not uploaded",
            error: true,
            status: 404
        }, null, res, req);
    };
}

function createNode(req, res) {
    var model = req.body.model,
        nodeid = req.swagger.params.nodeid.value,
        node = {
            model: model,
            nodeid: nodeid
        };

    global.log.info("Creating node: " + JSON.stringify(node));
    encoder.createEmptyNode(req.getCurrentUser(), node, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}

function updateNode(req, res) {
    var siteid = req.swagger.params.siteid.value,
        orgid = req.swagger.params.orgid.value,
        node = req.body;

    node.nodeid = req.swagger.params.nodeid.value;

    if(node.model &&
        node.model != 'unode-v5' && node.model != 'unode-v6'){
        // Check v5 only params
        var v5only = ['apn', 'iccid', 'imei', 'imsi', ];
        for (var i=0; i<v5only.length; i++) {
            var param = v5only[i];
            if(node[param]!==undefined){
                return response.Done({message: `Invalid param '${param} for node model '${node.model}'`, status: 400, error: true}, null, res, req);
            }
        };
    }

    var params = {
        id: node.nid,
        nodeprops: node,
        siteprops: {
            siteid: siteid
        },
        orgprops: {
            orgid: orgid
        }
    };

    global.log.info("Updating node: " + JSON.stringify(node))
    doNodeCrud(req, res, 'update', node.nodeid, params);
}

function deleteNode(req, res) {
    var siteid = req.swagger.params.siteid.value,
        orgid = req.swagger.params.orgid.value,
        nodeid = req.swagger.params.nodeid.value;

    encoder.deleteNode(req.getCurrentUser(), nodeid, siteid, orgid, function(err, msg) {
        response.Done(err, null, res, req);
    });
}

/**
 * Encoder call wrapper
 * @param req
 * @param res
 * @param operation
 * @param id
 * @param params
 */
function doNodeCrud(req, res, operation, id, params) {
    var callback = function(err, data) {
        response.Done(err, data, res, req);
    };

    encoder.doCrud(req.getCurrentUser(), operation, 'NodeModel', params, callback);
}

function getSiteSensorHistoryFromTo(req, res) {
    var siteid = req.swagger.params.siteid.value,
        orgid = req.swagger.params.orgid.value,
        date2 = req.swagger.params.date2.value,
        date1 = req.swagger.params.date1.value,
        limit = req.swagger.params.limit.value,
        period = req.swagger.params.period ? req.swagger.params.period.value : "15min",
        sensorid = req.swagger.params.sensorid.value;

    var d1 = new Date(date1);
    var d2 = new Date(date2);

    if(isNaN(d1.getDate()) || isNaN(d2.getDate()) || d1.getTime()>=d2.getTime())
        response.Done({status:400, message:"Request contains invalid date"}, null, res, req);
    else {
      d2.setHours(23,59,59,999); // to set time to end of day
      date1 = d1.toISOString();
      date2 = d2.toISOString();

      encoder.getSiteSensorHistoryFromTo(req.request_id, req.getCurrentUser(), orgid, siteid, sensorid, date1, date2, limit, period, function(err, msg) {
          var resp = {};

          if(sensorid==="energy") {
            resp.toBarrels = 0.002;
            resp.toTrees = 0.018;
            resp.toCars = 0.0001;
            resp.toCO2 = 0.0007;
          }

          if (msg) {
              resp.message = msg.length + " datapoints found";
              resp.siteid = siteid;
              resp.sensorid = sensorid;
              resp.datapoints = msg;
          }

          response.Done(err, resp, res, req);
      });
    }
}

function getSensorHistoryFromTo(req, res) {
    var siteid = req.swagger.params.siteid.value,
        orgid = req.swagger.params.orgid.value,
        nodeid = req.swagger.params.nodeid.value,
        date2 = req.swagger.params.date2.value,
        date1 = req.swagger.params.date1.value,
        limit = req.swagger.params.limit.value,
        period = req.swagger.params.period ? req.swagger.params.period.value : "15min",
        sensorid = req.swagger.params.sensorid.value;

    var d1 = new Date(date1);
    var d2 = new Date(date2);

    if(isNaN(d1.getDate()) || isNaN(d2.getDate()) || d1.getTime()>=d2.getTime())
        response.Done({status:400, message:"Request contains invalid date"}, null, res, req);
    else {
      d2.setHours(23,59,59,999); // to set time to end of day
      date1 = d1.toISOString();
      date2 = d2.toISOString();

      encoder.getSensorHistoryFromTo(req.request_id, req.getCurrentUser(), orgid, siteid, nodeid, sensorid, date1, date2, limit, period, function(err, msg) {
          var resp = {};
          if(sensorid==="energy") {
            resp.toBarrels = 0.002;
            resp.toTrees = 0.018;
            resp.toCars = 0.0001;
            resp.toCO2 = 0.0007;
          }

          if (msg) {
              resp.message = msg.length + " datapoints found";
              resp.siteid = siteid;
              resp.nodeid = nodeid;
              resp.sensorid = sensorid;
              resp.datapoints = msg;
          }

          response.Done(err, resp, res, req);
      });
    }
}

function getSensorHistory(req, res) {
    var siteid = req.swagger.params.siteid.value,
        orgid = req.swagger.params.orgid.value,
        nodeid = req.swagger.params.nodeid.value,
        limit = req.swagger.params.limit.value,
        date = req.swagger.params.date.value,
        sensorid = req.swagger.params.sensorid.value;

    var d = new Date(date);

    if(isNaN(d.getDate())) {
        response.Done({status:400, message:"Request contains invalid date"}, null, res, req);
    } else {
      date = d.toISOString();
      encoder.getSensorHistory(req.request_id, req.getCurrentUser(), orgid, siteid, nodeid, sensorid, date, limit, function(err, data) {

          var resp = {};

          if(data) {
              resp.message = data.length + " datapoints found";
              resp.nodeid = nodeid;
              resp.sensorid = sensorid;
              resp.datapoints = data;
          }

          response.Done(err, resp, res, req);
      });
    }
}

function getNodeConnectionStatus(req, res) {
    var siteid = req.swagger.params.siteid.value,
        nodeid = req.swagger.params.nodeid.value;

    encoder.getNodeConnectionStatus(req.getCurrentUser(), siteid, nodeid, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getSiteNodesStatuses(req, res) {
    var siteid = req.swagger.params.siteid.value;
    var orgid = req.swagger.params.orgid.value;

    encoder.getSiteNodesStatuses(req.request_id, req.getCurrentUser(), orgid, siteid, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}

function getNodeLightStatus(req, res) {
    var siteid = req.swagger.params.siteid.value,
        nodeid = req.swagger.params.nodeid.value;

    encoder.getNodeLightStatus(req.getCurrentUser(), siteid, nodeid, function(err, msg) {
        response.Done(err, msg, res, req);
    });
}

function resendScheduleToNode(req, res) {
    var user = req.session.CurrentUser,
        orgid = req.swagger.params.orgid.value,
        siteid = req.swagger.params.siteid.value,
        nodeid = req.swagger.params.nodeid.value;
    encoder.resendScheduleToNode(user,
                                 orgid,
                                 siteid,
                                 nodeid,
                                 function (err, msg) {
                                     response.Done(err, null, res, req);
                                 });
}

