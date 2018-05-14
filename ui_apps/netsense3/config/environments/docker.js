'use strict'
module.exports = {
  // UI
  referenceImplementation : {
    protocol: process.env.UI_PROTOCOL || 'https',
    host: process.env.UI_HOST || 'nsn-local.sensity.com',
    port: 8090,
    webpackServerPort: 8089
  },
  // Backend
  interface : {
    protocol: process.env.INTF_PROTOCOL || 'https',
    host: process.env.INTF_HOST || 'nsn-local.sensity.com',
    port: 443,
    version: 'v3.0'
  }
};