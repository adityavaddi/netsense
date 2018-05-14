var msgpack = require('msgpack5')(),
    Freezer = require('freezer-js'),
    Map = require('collections/map'),
    configManager = require('kea-config'),
    metrics = require('metrics'),
    ddReporting = require('./../metrics/dd-report.js');

var cluster = require('cluster');
var http = require('http');
//var amqp = require('amqplib');
var amqp = require('amqplib/callback_api');

configManager.init('./config/main.conf.js');

var uuid = require('uuid');

// DD graphite reporter
var graphite_settings = configManager.get('graphite');
reporter = new ddReporting.GraphiteReporter(ddReporting.report, "interface", graphite_settings.host, graphite_settings.port);
reporter.on('log', function(level, msg, exc) {
    if(exc) {
        console.log('%s -- %s (%s)', level, msg, exc);
    } else {
        console.log('%s -- %s', level, msg);
    }
});
reporter.start(graphite_settings.reporting_interval);

var map = new Map({}),
    enc = msgpack.encode,
    dec = msgpack.decode,
    callbacks = {};



var rabbitmq_settings = configManager.get('rabbitmq');


var reqqnameprefix = rabbitmq_settings.apiservice.request_queue_prefix;
var repqnameprefix = rabbitmq_settings.apiservice.reply_queue_prefix;

var channel = null;
var queue = null;
var exc_name = 'amq.topic';

amqp.connect(rabbitmq_settings.apiservice.uri, function(err, conn) {
    if(err){
        //return callback(err, null);
        return global.log.error("Could not connect to RabbitMQ");
    }
    conn.createChannel(function(err, ch) {
        if(err){
            //return callback(err, null);
            return global.log.error("Could not create channel");
        }
        channel = ch;
        ch.assertExchange(exc_name, 'topic', {durable: true, autoDelete : false});

        ch.assertQueue(repqnameprefix+global.instance_id, {exclusive: true}, function(err, q) {
            if(err){
                //return callback(err, null);
                return global.log.error("Could not assert queue");
            }

            queue = q;
            ch.bindQueue(q.queue, exc_name, repqnameprefix+global.instance_id);

            ch.consume(q.queue, function(msg) {
                if (callbacks[msg.properties.correlationId]) {
                    console.log('[%s] Got response id=%s', global.instance_id, msg.properties.correlationId, msg.content.toString());
                    var cb = callbacks[msg.properties.correlationId];
                    var response = JSON.parse(msg.content.toString());
                    if(response.error)
                        (cb)(response, null);
                    else
                        (cb)(null, response);
                    delete callbacks[msg.properties.correlationId];
                    return;
                }
                // TODO: else
            }, {noAck: true});

        });
    });
});

var sendToRabbit = function (service, message, callback) {

    if(!channel){
        return callback({
            error: true,
            status: 500,
            message: 'Could not connect to RabbitMQ'
        });
    }

    message.qid = global.instance_id;

    var corr = message.requestID;

    console.log('[%s] Sending request id=%s', global.instance_id, corr);

    channel.sendToQueue(
        reqqnameprefix+service,
        new Buffer(JSON.stringify(message)),
        { correlationId: corr, replyTo: queue.queue }
    );


};

exports.Send = function (service, msg, callback) {

    callbacks[msg.requestID] = callback;

    // Send to microservice
    global.log.info("Sending message to Rabbit", JSON.stringify(msg));
    sendToRabbit(service, msg, callback);
}

/***
 The bellow commented code block is for reference - this is how the function should be called

 getActivityLogs('uberuser', function(msg) {

    global.log.info('Msg: ' +msg)
})*/



