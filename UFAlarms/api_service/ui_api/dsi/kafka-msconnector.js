'use strict';
var msgpack = require('msgpack5')(),
    configManager = require('kea-config'),
    metrics = require('metrics'),
    Freezer = require('freezer-js'),
    Map = require('collections/map'),
    uuid = require('uuid'),
    ddReporting = require('./../metrics/dd-report.js'),
    cluster = require('cluster'),
    http = require('http'),
    Kafka = require('node-rdkafka')
;
console.log(Kafka.librdkafkaVersion);

configManager.init('./config/main.conf.js');


// DD graphite reporter
/*var graphite_settings = configManager.get('graphite');
reporter = new ddReporting.GraphiteReporter(ddReporting.report, "interface", graphite_settings.host, graphite_settings.port);
reporter.on('log', function(level, msg, exc) {
    if(exc) {
        console.log('%s -- %s (%s)', level, msg, exc);
    } else {
        console.log('%s -- %s', level, msg);
    }
});
reporter.start(graphite_settings.reporting_interval);
*/

var map = new Map({});


var kafka_settings = configManager.get('kafka');
// var clientId = kafka_settings.client.clientId;
//var reqqnameprefix = kafka_settings.apiservice.request_queue_prefix;
var repqnameprefix = kafka_settings.apiservice.reply_queue_prefix;
//var req_topic = reqqnameprefix + kafka_settings.client.clientId;
var rep_topic =  kafka_settings.topics.reply;

var rets = 0;

var topics = [rep_topic];

// --- Streams --------------------------------------------------------------------------

// Read from the librdtesting-01 topic... note that this creates a new stream on each call!
var readStream = Kafka.createReadStream(kafka_settings.consumer, {
    'auto.offset.reset': 'latest'
}, {
    topics: topics,
    waitInterval: 0,
});

readStream.on('error', function(err) {
    global.log.error('Kafka readStream stream error', err);
});

readStream.consumer.on('event.error', function(err) {
    global.log.error('Kafka readStream consumer error\',', err);
})

readStream.consumer.on('ready', function(err) {
    global.log.error('Kafka readStream consumer ready\',', err);
})

readStream.consumer.on('disconnected', function(arg) {
    global.log.info('consumer disconnected. ' + JSON.stringify(arg));
});

readStream.on('data', function(message) {
    try {
        var jsonObj = JSON.parse(message.value);
        //console.log('Received', jsonObj.messageid, message.value);
        //console.log('Callbacks', JSON.stringify(map));

        var state = map.get(jsonObj.messageid);
        if (state) {
            if (!jsonObj.response) {
                global.log.error("ERROR: Message format invalid!", JSON.stringify(jsonObj));
                return;
            }
            state.set('result', jsonObj.response);
        } else {
            global.log.error('No state for message', jsonObj.messageid);
        }
    } catch (e) {
        global.log.error('Error processing MS response', e.message);
    }
    // console.log('Got message');
    // console.log(message.value.toString());
});


var writeStreams = {};

/**
 * Return  existing or create new topic stream
 * @param topic String Topic name
 * @returns {*}
 */
function getWriteStream(topic) {

    if(writeStreams[topic])
        return writeStreams[topic];

// Our producer with its Kafka brokers
// This call returns a new writable stream to our topic
    var writeStream = Kafka.Producer.createWriteStream(kafka_settings.producer, {}, {
        topic: topic
    });

    writeStream.on('error', function (err) {
        global.log.error('Kafka stream error', err);
    });

    writeStream.producer.on('event.error', function(err) {
        global.log.error('Kafka writeStream producer stream error', err);
    });

    writeStream.producer.on('event.log', function(err) {
        global.log.info('Kafka writeStream producer stream log', err);
    });

    writeStream.producer.on('disconnected', function(arg) {
        global.log.info('producer disconnected. ' + JSON.stringify(arg));
    });

    writeStream.producer.on('ready', function(arg) {
        global.log.info('producer ready. ' + JSON.stringify(arg));
    });

    writeStreams[topic] = writeStream;

    return writeStreams[topic];
}


// --- Export ---------------------------------------------------------------------------

var sendToKafka = function (topic, message, callback) {
    var data = JSON.stringify(message);
    global.log.debug('kafka payload', data);
    let writeStream = getWriteStream(topic);

// Writes a message to the stream
    var queuedSuccess = writeStream.write(data);

    if (queuedSuccess) {
        global.log.debug('sent %d messages', ++rets);
    } else {
        // Note that this only tells us if the stream's queue is full,
        // it does NOT tell us if the message got to Kafka!  See below...
        global.log.error('Too many messages in our queue already');
        //callback(new Error('Too many messages in our queue already'));
    }
};

exports.Send = function (service, msg, callback) {
    if (!(service in kafka_settings.topics)) {
        global.log.error("Unknown service ", service, " for topics ", JSON.stringify(kafka_settings.topics));
        return;
    }

    var topic = kafka_settings.topics[service];
    
    msg.instanceid = kafka_settings.instance_id;
    msg.timestamp = new Date().toISOString();

    var data = {
        messageid: uuid.v4(),
        responsetopic: rep_topic,
        request: msg
    };

    global.log.info('[%s] Sending message id=%s to topic=%s', kafka_settings.instance_id, data.messageid, topic);

    var freezer = new Freezer({starttime: msg.timestamp});
    var state = freezer.get();
    map.set(data.messageid, state);

    freezer.on('update', function (newValue) {
        var result = freezer.get().result;
        global.log.info("interface <= microservices ", JSON.stringify(result));
        (result.error) ? callback(result, null) : callback(null, result);
        map.delete(data.messageid);
    });

    var cb = function(error, data){
        global.log.info("Send callback", error, data);
        if(error){
            global.log.error(error);
            callback(error, null);
            //map.delete(data.messageid);
        }
    };

    // Send to microservice
    global.log.info("Sending message to Kafka on topic ", topic, " ", JSON.stringify(data));
    sendToKafka(topic, data, cb);
};