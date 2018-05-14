'use strict';

var util = require('util'),
    uuid = require('uuid'),
    encoder = require('./../../encoder/Encoder.js').JSONEncoder;

var response = require('./../helpers/response.js');

module.exports = {
    getSomething: getSomething,
    postSomething: postSomething
};

function getSomething(req, res) {
    var what = req.swagger.params.what.value;
    var newid = req.request_id;
    global.log.info("Got nothing posted");
    var data =  {
        trace: []
    };

    data.trace.push({
        id: instance_id,
        time: (new Date()).getTime(),
        log: ('['+instance_id+'] Sent request id = ' + req.request_id)
    });
    encoder.testSomething(newid, req.getCurrentUser(), what, data, function (err, msg) {
        response.Done(err, msg, res);
    });
}

function postSomething(req, res) {
    var what = req.swagger.params.what.value;
    var newid = req.request_id;
    var data = req.body;
    data.trace = [];
    data.trace.push({
        id: instance_id,
        time: (new Date()).getTime(),
        log: ('['+instance_id+'] Sent request id = ' + req.request_id)
    });
    global.log.info("Got something posted: " + JSON.stringify(data));
    encoder.testSomething(newid, req.getCurrentUser(), what, data, function (err, msg) {
        response.Done(err, msg, res);
    });
}
