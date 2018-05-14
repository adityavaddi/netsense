var https = require('https');
// var protractor = require('protractor');
var input = require('../variables');

var api = function () {
  this.login = function () {
    var defferred = protractor.promise.defer();
    var config = {
      hostname: input.hostname,
      path: '/login',
      method: 'POST',
      headers: {
        "Content-Type": "application/json"
      },
      body: {
        "email": input.username,
        "password": input.password
      }
    };

    var callback = function () {
      deferred.fulfill();
    };

    var req = http.request(config, callback);
    req.end();

    return deferred.promise;
  }
};
