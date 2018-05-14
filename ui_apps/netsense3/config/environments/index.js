'use strict';

//** Environment Variables Used **// 
// 
// APP           : app* || demo
// NODE_ENV      : development* || production || docker
// UI_PROTOCOL   : http* || https
// UI_HOST       : localhost* || nsn-local.sensity.com
// UI_PORT       : 8090
// INTF_PROTOCOL : http* || https
// INTF_HOST     : localhost* || nsn-local.sensity.com
// INTF_PORT     : 10010* || 443
//
// NOTE : * indecates default type in development environment
// ********************************//

var path = require('path');
var _ = require('lodash');

var all = {
  env: process.env.NODE_ENV || 'development',
  root: path.normalize(__dirname + '/../..'),
  appName: process.env.APP || 'app'
};

module.exports = _.merge(all, require('./' + all.env + '.js'));