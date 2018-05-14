/**
 * Creates global mosca server
 */
"use strict";

var server = null;

var configManager = require('kea-config');
configManager.init('./config/main.conf.js');



var mosca = require("mosca");
var settings = {
    port: configManager.get('mosca.port') // Port 3000 conflicts with UI proxy
    //http: {
    //    port: 3000,
    //    bundle: true,
    //    static: './'
    //}
    ,
    persistence: mosca.persistence.Memory
};

    try {
        server = new mosca.Server(
            settings, function () {
                global.log.info("mosca is running")
            }
        );

        server.on("error", function (err) {
            global.log.error('mosca error',err);
        });

// fired whena  client is connected
        server.on('clientConnected', function (client) {
            global.log.info('client connected', client.id);
        });

// fired when a message is received
        server.on('published', function (packet, client) {
            //global.log.info('Published : ', packet.payload.toString());
        });

// fired when a client subscribes to a topic
        server.on('subscribed', function (topic, client) {
            //global.log.info('subscribed : ', topic);
        });

// fired when a client subscribes to a topic
        server.on('unsubscribed', function (topic, client) {
            //global.log.info('unsubscribed : ', topic);
        });

// fired when a client is disconnecting
        server.on('clientDisconnecting', function (client) {
            //global.log.info('clientDisconnecting : ', client.id);
        });

// fired when a client is disconnected
        server.on('clientDisconnected', function (client) {
            //global.log.info('clientDisconnected : ', client.id);
        });
    }
    catch(err) {
        console.log("mosca Init Error: ",err);
        global.log.error(err, "initializing server in mosca-server.js");
    }

//noinspection JSUnresolvedVariable
exports = server;