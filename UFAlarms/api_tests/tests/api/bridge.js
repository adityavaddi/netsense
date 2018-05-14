"use strict";

var server_url = process.env.stack_url;
var should = require('should');
var msgpack = require('msgpack5')();
var mqtt = require('mqtt');
const Kafka = require('node-rdkafka');
const uuid = require('uuid');
var Freezer = require('freezer-js');
var Map = require('collections/map');
var map = new Map({});
var amqp = require('amqplib/callback_api');
var configManager = require('kea-config');
configManager.init('./config/main.conf.js');


var mqttClient = mqtt.connect(configManager.get('sse').url);
console.log('MQTT:', configManager.get('sse').url)
console.log('Kafka:', configManager.get('kafka').kafkaurl)

var rabbitConfig = configManager.get('rabbit').url;
console.log('Connected to %s', rabbitConfig);


var topics = ['sensor', 'alarm', 'login', 'config.reply', 'ppv.node.evt', 'media', 'corenode.sensor', 'corenode.alarm'];

var writeStreams = {};
/**
 * Return  existing or create new topic stream
 * @param topic String Topic name
 *
 * @returns {*}
 */
function getWriteStream(topic) {

    if (writeStreams[topic])
        return writeStreams[topic];

    // Our producer with its Kafka brokers
    // This call returns a new writable stream to our topic
    let writeStream = Kafka.Producer.createWriteStream({
        'client.id': 'interface-producer-bridge',
        'metadata.broker.list': configManager.get('kafka').kafkaurl,

    }, {}, {
        topic: topic
    });

    writeStream.on('error', function (err) {
        console.log('Kafka stream error', err);
    });

    writeStream.producer.on('event.error', function (err) {
        console.log('Kafka writeStream producer stream error', err);
    });

    writeStream.producer.on('event.log', function (err) {
        console.log('Kafka writeStream producer stream log', err);
    });

    writeStream.producer.on('disconnected', function (arg) {
        console.log('producer disconnected. ' + JSON.stringify(arg));
    });

    writeStream.producer.on('ready', function (arg) {
        console.log('Kafka producer ready. ' + JSON.stringify(arg));
    });

    writeStreams[topic] = writeStream;

    return writeStreams[topic];
}

function sendMessageToKafka(topic, message, callback) {
    const data = JSON.stringify(message);    
    let writeStream = getWriteStream(topic);

    // Writes a message to the stream
    const queuedSuccess = writeStream.write(data);
    callback(null);
}


let readStream = Kafka.createReadStream({
    'group.id': 'node-rdkafka-consumer' + new Date().getTime(),
    'metadata.broker.list': configManager.get('kafka').kafkaurl,
    'enable.auto.commit': true,
    'metadata.max.age.ms': 100,
    'fetch.wait.max.ms': 5,
    'fetch.error.backoff.ms': 10,
    // 'debug': 'all',
    'socket.keepalive.enable': true,
    'auto.commit.interval.ms': 5000,
    'heartbeat.interval.ms': 10000,
}, {
    'auto.offset.reset': 'latest'
}, {
    topics: topics,
    waitInterval: 0,
});


readStream.consumer.on('event.error', function (err) {
    console.log('Kafka readStream consumer error\',', err);
})

readStream.consumer.on('ready', function (err) {
    console.log('Kafka readStream consumer ready\'', err);
})

readStream.consumer.on('disconnected', function (arg) {
    console.log('consumer disconnected. ' + JSON.stringify(arg));
});


readStream.on('data', function (message) {
    try {
        var jsonObj = JSON.parse(message.value);
        if (message.topic) var state = map.get(message.topic);
        if (state) {
            if (!jsonObj) {
                console.log("ERROR: Message format invalid!", JSON.stringify(jsonObj));
                return;
            }
            state.set('result', jsonObj);
        }
    } catch (e) {
        console.log('Error processing MS response', e.message);
    }
});

function readMessageFromKafka(key, callback) {
    var freezer = new Freezer({
        starttime: new Date().toISOString()
    });
    var state = freezer.get();
    map.set(key, state);

    freezer.on('update', function (newValue) {
        var result = freezer.get().result;
        callback(result);
        map.delete(key);
    });
}

describe('Bridge', function () {
    describe('MQTT to Kafka bridge', function () {

        before(function (done) {
            const dummyMessage = {
                time: new Date().toISOString()
            };
            const kafkaTopic = 'sensor';
            sendMessageToKafka(kafkaTopic, dummyMessage, function (err, message) {
                readMessageFromKafka(kafkaTopic, function (result) {
                    done();
                });
            });
        });

        it('should read sensor sample and publish to sensor kafka topic', function (done) {
            var sensorSample = {
                a: 'POST',
                p: 'v1/bridgenode/out/UNSOL/sensors',
                f: '',
                sid: 'bridgenode',
                uuid: uuid.v4(),
                l: {
                    n: 'degC',
                    s: 'jt',
                    v: 123.11,
                    t: 1477674256293862
                }
            };

            var sensorSamplePayload = JSON.parse(JSON.stringify(sensorSample));
            sensorSamplePayload["l"] = msgpack.encode(sensorSamplePayload["l"]);

            setTimeout(function () {
                mqttClient.connected.should.eql(true);
                mqttClient.publish('v1/bridgenode/out/UNSOL/sensors', msgpack.encode(sensorSamplePayload));
                console.log("Posting Sensor Sample to MQTT");
            }, 100);
            readMessageFromKafka('sensor', function (result) {
                sensorSample.should.eql(result);
                done();
            });
        });

        // added x to ignore this test case
        xit('should read parking sample and publish to  kafka topic ppvnodeevt', function (done) {
            var parkingSample = {
                h: {
                    e: true,
                    n: 'ObjectStopped',
                    t: '1518736977298355',
                    uuid: '4pO2Cd7@z%K>y0]m5(E%',
                    ch: '0'
                },
                o: []
            };

            var parkingSamplePayload = JSON.parse(JSON.stringify(parkingSample));

            setTimeout(function () {
                mqttClient.connected.should.eql(true);
                mqttClient.publish('v1/N05666362/out/va-v1/evt/pko', msgpack.encode(parkingSamplePayload));
                console.log("Posting Parking Sample to MQTT");
                //TODO - add assertion when enabling the test
                done();
            }, 100);
        });

        it('should read device alarm message and publish to alarm kafka topic', function (done) {
            var alarmPayload = {
                s: 0,
                a: 'SW/storage/volmissing/data',
                m: '/data volume missing',
                c: 3
            };
            var alarm = {
                uuid: uuid.v4(),
                f: 'SW/storage/volmissing/data',
                a: 'UNSOL',
                l: alarmPayload,
                p: 'v1/bridgenode/out/UNSOL/alarm/{alarmid}',
                sid: 'bridgenode',
                d: '2017-09-18T15:33:22Z'
            };

            var deviceAlarmPayload = JSON.parse(JSON.stringify(alarm));
            deviceAlarmPayload["l"] = msgpack.encode(deviceAlarmPayload["l"]);

            setTimeout(function () {
                mqttClient.connected.should.eql(true);
                mqttClient.publish('v1/bridgenode/out/UNSOL/alarm', msgpack.encode(deviceAlarmPayload));
                console.log("Posting Device Alarm to MQTT");
            }, 100);
            readMessageFromKafka('alarm', function (result) {
                alarm.should.eql(result);
                done();
            });
        });

        it('should read login request and publish to login kafka topic', function (done) {
            var loginReqPayload = {
                nid: 'bridgenode',
                protoc: 0,
                dev: 'falcon-q',
                ssid: 'SensityDefault',
                cid: 'dfa88erf',
                profile: '',
                ch: '24',
                tok: 'ASFEF654213QWEQWEQWAFDS5632132465fq4w5e643241',
                ip: '127.0.0.1',
                t: 150880029022289,
                bssid: 'AD:FF:BE:CB:AB:EF:BB:AA',
                mac: 'BA:FC:AF:BF:DA:FD:AE:CB',
                sec: 'WPA_PSK',
                subtype: '1,1',
                modemRevEd: ''
            };
            var loginReq = {
                a: 'POST',
                p: 'v1/bridgenode/out/POST/loginReq',
                sid: 'bridgenode',
                d: '2017-10-10T20:34:01Z',
                uuid: uuid.v4(),
                f: '',
                l: loginReqPayload
            };

            var loginRequestPayload = JSON.parse(JSON.stringify(loginReq));
            loginRequestPayload["l"] = msgpack.encode(loginRequestPayload["l"]);
            setTimeout(function () {
                mqttClient.connected.should.eql(true);
                mqttClient.publish('v1/bridgenode/out/POST/loginReq', msgpack.encode(loginRequestPayload));
                console.log("Posting Login Req to MQTT");
            }, 100);
            readMessageFromKafka('login', function (result) {
                loginReq.should.eql(result);
                done();
            });
        });

        it('should read lwt and publish to login kafka topic', function (done) {
            var lwtDisconnectPayload = {
                s: 4,
                a: 'lwt',
                m: 'bridgenode_client_ext_ disconnected',
                c: 1
            };
            var lwt = {
                uuid: uuid.v4(),
                f: '',
                a: 'UNSOL',
                l: lwtDisconnectPayload,
                p: 'v1/bridgenode/out/UNSOL/lwt',
                sid: 'bridgenode',
                d: '2018-01-05T16:43:19Z'
            };
            var lwtDisconnectPayload = JSON.parse(JSON.stringify(lwt));
            lwtDisconnectPayload["l"] = msgpack.encode(lwtDisconnectPayload["l"]);
            setTimeout(function () {
                mqttClient.connected.should.eql(true);
                mqttClient.publish('v1/bridgenode/out/UNSOL/lwt', msgpack.encode(lwtDisconnectPayload));
                console.log("Posting LWT to MQTT");
            }, 100);
            readMessageFromKafka('login', function (result) {
                lwt.should.eql(result);
                done();
            });
        });

        it('should read config Sample and publish to config.reply kafka topic', function (done) {
            var configSample = {
                e: "",
                s: 200,
                uuid: uuid.v4(),
                a: "GET/REPLY",
                l: {
                    't': "2693130743",
                    'db': "default",
                    'l': 'eyJHZW5ldGVjLkdlbmVyYWwuQnJhbmQuQ29tcGFueU5hbWUiOiJTZW5zaXR5IiwiR2VuZXRlYy5HZW5lcmFsLkJyYW5kLk1vZGVsTmFtZSI6IlZpZGVvIE5vZGUiLCJHZW5ldGVjLkdlbmVyYWwuRmlybXdhcmVWZXJzaW9uIjoxLCJHZW5ldGVjLkdlbmVyYWwuUHJvdG9jb2xWZXJzaW9uIjoxLCJHZW5ldGVjLk5ldHdvcmsuU2VydmljZXMuSHR0cC5Qb3J0Ijo4MDAwLCJHZW5ldGVjLmN1c3RvbV9ldmVudHMiOiJlbnRlcixsZWZ0IiwiYWxhcm0ucmVkaXMubWVtb3'
                },
                p: 'v1/bridgenode/GET/REPLY/1d6aa519-1e0e-41cf-b279-d0d1a376b2c6/config',
                sid: "N02c01286",
                d: '2018-01-29T20:05:31Z'
            };

            var configSamplePayload = JSON.parse(JSON.stringify(configSample));
            configSamplePayload["l"] = msgpack.encode(configSamplePayload["l"]);

            setTimeout(function () {
                mqttClient.connected.should.eql(true);
                mqttClient.publish('v1/bridgenode/out/GET/REPLY/1d6aa519-1e0e-41cf-b279-d0d1a376b2c6/config/list', msgpack.encode(configSamplePayload));
                console.log("Posting Config Sample to MQTT");
            }, 100);
            readMessageFromKafka('config.reply', function (result) {
                configSample.should.eql(result);
                done();
            });
        });

        it('should read media mqtt messages and publish to media kafka topic', function (done) {
            var mediaSample = {
                a: 'UNSOL',
                p: 'v1/+/out/UNSOL/mediaserver/#',
                sid: 'mediaserver_imgcapture',
                cid: 'N02c0017c',
                uuid: uuid.v4(),
                c: 256,
                l: {
                    n: 'bridgenode',
                    o: 0,
                    t: 33383,
                    ts: '22018-01-23T01:30:36Z',
                    c: []
                }
            };

            var mediaSamplePayload = JSON.parse(JSON.stringify(mediaSample));
            mediaSamplePayload["l"] = msgpack.encode(mediaSamplePayload["l"]);

            setTimeout(function () {
                mqttClient.connected.should.eql(true);
                mqttClient.publish('v1/bridgenode/out/UNSOL/mediaserver', msgpack.encode(mediaSamplePayload));
                console.log("Posting Media Sample to MQTT");
            }, 100);
            readMessageFromKafka('media', function (result) {
                mediaSample.should.eql(result);
                done();
            });
        });

        after(function (done) {
            if (mqttClient.connected) mqttClient.end(); // disconnect mqtt client
            done();
        })

    });

    describe('RMQ to Kafka bridge and Kafka to RMQ', function () {

        it("should send sensor Sample corenode data to RabbitMQ and bridge will post to kafka topic corenode.sensor", function (done) {
            var sensorSample = {
                name: 'SensorSample',
                nodeid: 'corenodemochanode',
                sensor: 'p',
                units: '2',
                time: 1515612128076938,
                value: 1
            };

            var rabbitConfig = configManager.get('rabbit').url;

            setTimeout(function () {
                amqp.connect(rabbitConfig, function (err, conn) {
                    if (err) {
                        console.log("Rabbit MQ connection error: ", err);
                        return;
                    }
                    console.log('Connected to %s', rabbitConfig);

                    conn.createChannel(function (err, ch) {
                        if (err) {
                            console.log("Rabbit MQ Channel creation error: ", err);
                            return;
                        }
                        var sensorqueue = 'bridge';
                        var x = ch.publish('node.events', '*.sensor.*', msgpack.encode(sensorSample));
                        readMessageFromKafka('corenode.sensor', function (result) {
                            sensorSample.should.eql(result);
                            done();
                        });
                    });
                    setTimeout(function () {
                        conn.close();
                    }, 500);
                });
            }, 100);
        });

        it("should send AlarmSample corenode data to RabbitMQ and bridge will post to kafka topic corenode.alarm", function (done) {
            var alarmSample = {
                name: 'DeviceAlarm',
                nodeid: 'c_node_id',
                alarmType: 'HWFail_RTC',
                alarmSeverity: 'Clear',
                msg: 'Hardware Failure'
            }

            var rabbitConfig = configManager.get('rabbit').url;

            setTimeout(function () {
                amqp.connect(rabbitConfig, function (err, conn) {
                    if (err) {
                        console.log("Rabbit MQ connection error: ", err);
                        return;
                    }
                    console.log('Connected to %s', rabbitConfig);

                    conn.createChannel(function (err, ch) {
                        if (err) {
                            console.log("Rabbit MQ Channel creation error: ", err);
                            return;
                        }
                        var sensorqueue = 'bridge';
                        var y = ch.publish('node.events', '*.alarm.*', msgpack.encode(alarmSample));
                        readMessageFromKafka('corenode.alarm', function (result) {
                            alarmSample.should.eql(result);
                            done();
                        });

                    });
                    setTimeout(function () {
                        conn.close();
                    }, 500);
                });
            }, 100);
        });
    });

});
