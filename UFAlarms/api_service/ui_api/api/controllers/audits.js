'use strict';

var util = require('util');
var encoder = require('./../../encoder/Encoder.js').JSONEncoder;

var response = require('./../helpers/response.js');

module.exports = {
    getAllAudits: getAllAudits
};

function getAllAudits(req, res) {
    var orgid = req.swagger.params.orgid.value;
    var siteid = req.swagger.params.siteid.value;
    var datemin = req.swagger.params.datemin.value;
    var datemax = req.swagger.params.datemax.value;

    var requestid = req.headers['x-request-id'];

    var d1 = new Date(datemin);
    var d2 = new Date(datemax);

    if(isNaN(d1.getDate()) || isNaN(d2.getDate()) || d1.getTime()>=d2.getTime())
        response.Done({status:400, message:"Request contains invalid dates"}, null, res, req);
    else {
        encoder.getActivityLogs(req.getCurrentUser(), requestid, datemin, datemax, orgid, siteid, function (err, msg) {
            response.Done(err, msg, res, req);
        });
    }
}
