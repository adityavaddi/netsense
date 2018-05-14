'use strict';

var util = require('util');
var encoder = require('./../../encoder/Encoder.js').JSONEncoder;

var response = require('./../helpers/response.js');

module.exports = {
  autoComplete: autoComplete
};

function autoComplete(req,res) {
    var query = req.swagger.params.query.value;
    var categories = req.swagger.params.categories.value;
    var cb = function(err,data) {
        // Send success with whatever data is returned - empty array is still valid data
        response.Done(err, data, res, req);
    };
    encoder.autoComplete(req.session.CurrentUser,query,categories,cb);
}
