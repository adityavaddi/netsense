"use strict";

//var cache = require('./LocalCache.js').LocalCache;

// request module will be used to make the request to neo4j
var request = require('request');
var configManager = require('kea-config');

// neo4j transaction service URL
var graphdb_host = process.env.NSN_GRAPHDB_HOST || "localhost";
var graphdb_port = process.env.NSN_GRAPHDB_PORT || "7474";
var txUrl = "http://" + graphdb_host + ":" + graphdb_port + "/db/data/transaction/commit";

var ddc = require('./datadealerconnector');
//var msc = require('./msconnector');
var msck = require('./kafka-msconnector');

var mngr = {};

configManager.init('./config/main.conf.js');
var kafka_settings = configManager.get('kafka');
/**
 * Executes query
 * @param params query params
 * @param callback Node style callback to return result
 */
mngr.ExecQuery = function (params, callback) {

    var result = {};
    var service = params.service;
    delete params.topic;
    try {

        if (service) {
            // Send to microservice
            global.log.info("Sending message to micro service %s", service, JSON.stringify(params));

            // Check ACL service
            msck.Send('acl', params, function (aclerr, aclresult) {
                global.log.info("DataInterfaceService - response from ACL service", aclresult, aclerr);
                if (!aclerr && aclresult) {
                    // Proceed to Microservice
                    msck.Send(service, params, function (err, result) {
                        global.log.info("DataInterfaceService - response from Microservice", result, err);

                        if(err || (result && !result.success)) {
                            var error = { error: true, message: (err.error ? err.error : err.message), status: (err.status ? err.status : 400)};
                            callback && callback(error, result);
                        } else {
                            callback && callback(null, result);
                        }
                    });
                } else {
                    var error = { error: true, message: "Internal service error", status: 500 };
                    if (aclerr && aclerr.error) {
                        error = { error: true, message: aclerr.error, status: (aclerr.status ? aclerr.status : 400) };
                    }
                    callback && callback(error, aclresult);
                }
            });
        } else {
            // Send to datadealer
            ddc.Send(params, function (result) {
                //global.log.info('mngr.ExecQuery result', result);
                //  TODO: Better error reporting needed
                if (result.success) {
                    callback && callback(null, result);
                } else {
                    var error = { error: true, message: result.error, status: (result.status ? result.status : 400) };
                    callback && callback(error, result);
                }
            })
        }


    } catch (exc) {
        global.log.error("DSI error: %s %s", exc.message, exc.stack)
        callback && callback({ error: true, message: "DSI error: " + exc.message }, result);
    }
};

/**
 * Exec DS System Request. No access rights checked (No ACL)
 * @param params query params
 * @param callback Node style callback to return result
 */
mngr.SysExecQuery = function (params, callback) {
    var service = params.service;
    try {

        if (service) {
            // Send request to MS
            global.log.info("Sending message to micro service %s", service, JSON.stringify(params));
            msck.Send(service, params, function (err, result) {
                global.log.info("DataInterfaceService Sys - response from Microservice", err, result);

                if (err || (result && !result.success)) {
                    var error = { error: true, message: (err.error ? err.error : err.message), status: (err.status ? err.status : 400) };
                    callback && callback(error, result);
                } else {
                    callback && callback(null, result);
                }
            });
        } else {
            // Send request to datadealer
            ddc.Send(params, function (result) {
                if (result.success) {
                    callback && callback(null, result);
                } else {
                    var error = { error: true, message: result.error, status: (result.status ? result.status : 400) };
                    callback && callback(error, result);
                }
            })
        }


    } catch (exc) {
        global.log.error("DSI Sys error: %s %s", exc.message, exc.stack)
        callback && callback({ error: true, message: "DSI error: " + exc.message }, result);
    }
};

module.exports = mngr;