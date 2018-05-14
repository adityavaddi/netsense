var zmq = require('zmq'),
    mpkrequester = zmq.socket('req'),
    jsrequester = zmq.socket('req'),
    jsrequester_act_log = zmq.socket('req'),
    msgpack = require('msgpack5')(),
    Freezer = require('freezer-js'),
    Map = require('collections/map')

var cluster = require('cluster');
var http = require('http');
var numCPUs = require('os').cpus().length;

var map = new Map({}),
    enc = msgpack.encode,
    dec = msgpack.decode;

// TODO: Get all these from configuration file
mpkrequester.connect('tcp://localhost:5523');
jsrequester.connect('tcp://localhost:5521');
jsrequester_act_log.connect('tcp://localhost:5511');

var mpReplyNbr = 0;
var jsReplyNbr = 0;

mpkrequester.on('message', function (msg) {
    console.log('got reply', mpReplyNbr, dec(msg));
    mpReplyNbr += 1;
});

jsrequester.on('message', function (msg) {
    //console.log('got JS reply', jsReplyNbr, new Buffer(msg).toString('ascii'));
    jsonObj = JSON.parse(new Buffer(msg).toString('ascii'));
    map.get(jsonObj.qid).set('result', jsonObj.resp);
    jsReplyNbr += 1;
});

jsrequester_act_log.on('message', function (msg) {
    jsonObj = JSON.parse(new Buffer(msg).toString('ascii'));
    map.get(jsonObj.qid).set('result', jsonObj.resp);
});

var senddata = function (data, requester, callback) {
    var qid = Math.random().toString();
    var freezer = new Freezer({});
    var state = freezer.get();
    map.set(qid, state);
    freezer.on('update', function (newValue) {
        console.log("change happened");
        callback(freezer.get().result);
        map.delete(qid);
    });
    // Inject QID into the data object
    data.qid = qid;
    requester.send(JSON.stringify(data));
};

var createOrg = exports.createOrg = function (userid, params, callback) {
    var data = {
        msg: {
            type: 'createOrg',
            user: userid,
            orgprops: params
        }
    };
    senddata(data, jsrequester_act_log, function(msg) {
        callback(msg);
    });
};

var getActivityLogs = exports.getActivityLogs = function (userid, callback) {
    var data = {
        msg: {
            type: 'getActivityLogs',
            user: userid
        }
    };
    senddata(data, jsrequester_act_log, function(msg) {
        // result of getactivitylogs - handle this msg
        callback(msg)
    });
};

var getAllOrgs = exports.getAllOrgs = function (userid, callback) {
    var data = {
        msg: {
            type: 'getAllOrgs',
            user: userid
        }
    };
    senddata(data, jsrequester_act_log, function(msg) {
        // result of getactivitylogs - handle this msg
        callback(msg)
    });
};

var getAllSitesForOrg = exports.getAllSitesForOrg = function (userid, orgid, callback) {
    var data = {
        msg: {
            type: 'getAllSitesForOrg',
            user: userid,
            orgprops: {
                orgid: orgid
            }
        }
    };
    senddata(data, jsrequester_act_log, function(msg) {
        // result of getactivitylogs - handle this msg
        callback(msg);
    });
};

var getAllNodesForSite = exports.getAllNodesForSite = function (userid, siteid, callback) {
    var data = {
        msg: {
            type: 'getAllNodesForSite',
            user: userid,
            siteprops: {
                siteid: siteid
            }
        }
    };
    senddata(data, jsrequester_act_log, function(msg) {
        // result of getactivitylogs - handle this msg
        callback(msg);
    });
};

/***
The bellow commented code block is for reference - this is how the function should be called
*/
getActivityLogs('uberuser', function(msg) {

    console.log('Msg: ' +msg);
});



