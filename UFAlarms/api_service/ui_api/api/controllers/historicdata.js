'use strict';

var util = require('util');
var encoder = require('./../../encoder/Encoder.js').JSONEncoder;

var response = require('./../helpers/response.js');

module.exports = {
  historicaldata: getHistoricalData,
};

function getHistoricalData(req, res) {
  var since = req.swagger.params.since.value;
  var to = req.swagger.params.to.value;
  var type = req.swagger.params.type.value;
  var from = req.swagger.params.from.value;
  var which = req.swagger.params.which.value;
  var id = req.swagger.params.type.id;

  var d1 = new Date(from);
  var d2 = new Date(to);

  if(isNaN(d1.getDate()) || isNaN(d2.getDate()) || d1.getTime()>=d2.getTime())
      response.Done({status:400, message:"Request contains invalid dates"}, null, res, req);
  else {
      returnHistoricalData(req,res,which,type,from,id,since,to);
  }
}

function returnHistoricalData(req,res,which,type,from,id,since,to) {
  var cb = function(err,data) {
    // Send success with whatever data is returned - empty array is still valid data
    response.Done(err, data, res, req);
  };
  encoder.fetchHistoricalData(req.getCurrentUser(),which,type,from,id,since,to,cb);
}
