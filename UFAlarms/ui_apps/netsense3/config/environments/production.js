'use strict'
module.exports = {
  // UI
  referenceImplementation : {
    protocol: process.env.UI_PROTOCOL || 'https',
    host: process.env.UI_HOST || 'localhost',
    port: process.env.UI_PORT || 8090,
    webpackServerPort: 8089
  },
  // Backend
  interface : {
    protocol: process.env.INTF_PROTOCOL ||'http',
    host: process.env.INTF_HOST || 'localhost',
    port: process.env.INTF_PORT || 10010,
    version: 'v3.0'
  }
};