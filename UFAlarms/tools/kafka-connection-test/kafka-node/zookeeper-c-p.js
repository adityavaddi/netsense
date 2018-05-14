#!/usr/bin/env node

var kafka = require('kafka-node');
var HighLevelConsumer = kafka.HighLevelConsumer;
var Client = kafka.Client;

var argv = require('optimist').argv;
console.log('argv', JSON.stringify(argv));

var topic = argv.topic || 'topic1';
var zookeeper_server = argv.zookeeper_server || 'localhost';
var zookeeper_port = argv.zookeeper_port || '2181';
var kafka_server = argv.kafka_server || 'localhost';
var kafka_port = argv.kafka_port || '9092';
var connection_string = zookeeper_server+':'+zookeeper_port;
console.log('kafka_connection_string', connection_string);

var topics = [{ topic: topic }];
var options = { autoCommit: true, fetchMaxWaitMs: 1000, fetchMaxBytes: 1024 * 1024 };
var client1 = new Client(connection_string);
var consumer = new HighLevelConsumer(client1, topics, options);

var HighLevelProducer = kafka.HighLevelProducer;
var count = 3;
var rets = 0;
var client2 = new Client(connection_string);
var producer = new HighLevelProducer(client2);

consumer.on('message', function (message) {
    console.log(message);
});

consumer.on('error', function (err) {
    console.log('error', err);
});

producer.on('ready', function () {
    setInterval(send, 1000);
});

producer.on('error', function (err) {
    console.log('error', err);
});

function send () {
    var message = new Date().toString();
    producer.send([
        {topic: topic, messages: [message]}
    ], function (err, data) {
        if (err) console.log(err);
        else console.log('send %d messages', ++rets);
        if (rets === count) process.exit();
    });
}

send();