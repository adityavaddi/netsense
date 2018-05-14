"use strict";
const server_url = process.env.stack_url;
const should = require('should');
const request = require('supertest')(server_url);
const msgpack = require('msgpack5')();
const superagent = require('superagent');
const agent = superagent.agent();
const version = '/v3.0';
const Kafka = require('node-rdkafka');
const stream = require('stream');
const configManager = require('kea-config');

const uuid = require('uuid');
const messageId = uuid.v4();
configManager.setup('./config/');
configManager.init('./config/main.conf.js');

const helpers = require('./../../utils/helpers');
let csrfToken = null;

let writeStreams = {};

const gpsNodeId = "gpscorenode001";

const sensorSample = {
    altAndMisc: 7192641,
    altitude: 175.60000000000002,
    ctrlmode: 1,
    epochsecs: 1517516302,
    fixtype: 3,
    gpsver: 'undefined',
    hdop: 3.18,
    lat: 267.3852237,
    lon: -321.9323167,
    name: 'Node1',
    nodeid: gpsNodeId,
    numsatellite: 4,
    pdop: 3.33,
    rs: 'A',
    snr: 211715,
    snrAndMisc: null,
    snravg: 0,
    snrmax: 0,
    snrmin: 0,
    spare: 0,
    vdop: 1
};

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
        'client.id': 'interface-producer-gps',
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
        console.log('producer ready. ' + JSON.stringify(arg));
    });

    writeStreams[topic] = writeStream;

    return writeStreams[topic];
}

function sendMessageToKafka(topic, message, callback) {
    const data = JSON.stringify(message);
    console.log('kafka payload', data);
    let writeStream = getWriteStream(topic);

    // Writes a message to the stream
    const queuedSuccess = writeStream.write(data);

    if (queuedSuccess) {
        console.log('sent messages');
    } else {
        // Note that this only tells us if the stream's queue is full,
        // it does NOT tell us if the message got to Kafka!  See below...
        console.log('Too many messages in our queue already');
        //callback(new Error('Too many messages in our queue already'));
    }
    callback(null);
}


let readStream = Kafka.createReadStream(
    {
        'group.id': 'node-rdkafka-consumer' + new Date().getTime(),
        'metadata.broker.list': configManager.get('kafka').kafkaurl,
        'enable.auto.commit': true,
        'fetch.wait.max.ms': 5,
        'fetch.error.backoff.ms': 10,
        // 'debug': 'all',
        'socket.keepalive.enable': true,
        'auto.commit.interval.ms': 5000,
        'heartbeat.interval.ms': 10000,
    }
    , {
        'auto.offset.reset': 'latest'
    }, {
        topics: ['api.reply.interface'],
        waitInterval: 0,
    });


readStream.consumer.on('event.error', function (err) {
    console.log('Kafka readStream consumer error\',', err);
})

readStream.consumer.on('ready', function (err) {
    console.log('Kafka readStream consumer ready\',', err);
})

readStream.consumer.on('disconnected', function (arg) {
    console.log('consumer disconnected. ' + JSON.stringify(arg));
});

function compareConsumedEvent(topic, callback) {
    readStream.on('data', function (data) {
        const msg = JSON.parse(data.value);
        if (data.topic == topic && topic == 'api.reply.interface' && msg.messageid == messageId) {
            callback(msg);
        }
        callback(null);
    });
}

describe('GPS Service', function () {

    it('should sign in with correct credentials', function (done) {
        const data = { email: "uberuser@sensity.com", password: "ubeR$23" };
        request
            .post(version + '/login')
            .send(data)
            .set('Accept', 'application/json')
            .expect('Content-Type', /json/)
            .expect(200)
            .end(function (err, res) {
                should.not.exist(err);
                agent.saveCookies(res);
                csrfToken = helpers.getCsrfToken(res);
                done();
            });
    });

    it('should create test nodes', function (done) {
        let createNodeReq = request.put(version + '/nodes');
        agent.attachCookies(createNodeReq);
        const payload = {
            csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude\n" +
                "gpscorenode001,unode-v4,uberorg,ubersite,44.4,41.2\n" +
                "gpscorenode002,unode-v4,uberorg,ubersite,44.4,41.2\n"
        };
        createNodeReq.send(payload)
            .set('X-CSRF-Token', csrfToken)
            .expect(200)
            .end(function (err, res2) {
                should.not.exist(err);
                done();
            });
    });

    it("should send gps event to kafka", function (done) {        
        sendMessageToKafka('corenode.gps', sensorSample, function (err, res) {
            done();
        });

    });

    it("should wait for 4 sec to persist gps", function (done) {
        setTimeout(function () {
            done();
        }, 4 * 1000);
    });

    it("shoud get lat, lon for getNode from gps service", function (done) {
        const getGpsByNodeIdReq = {
            messageid: messageId,
            responsetopic: 'api.reply.interface',
            request: {
                requestid: '63f32065-6013-49fe-bd89-773ac2c3c781',
                type: 'getGpsByNodeId',
                model: 'GpsModel',
                action: 'CAN_READ',
                orgprops: {
                    orgid: 'uberorg'
                },
                siteprops: {
                    siteid: 'ubersite'
                },
                nodeprops: {
                    nodeid: gpsNodeId
                },
                instanceId: 'a5917b59-91f7-4689-b60c-24e36afdb2aa',
                timestamp: '2018-03-01T22:25:17.118Z'
            }
        };
        //Make GPS Api call to validate the gps values
        sendMessageToKafka('ms.request.gps', getGpsByNodeIdReq, function (err, res) {
            console.log("Completed the gps api call");
            compareConsumedEvent('api.reply.interface', function (msg) {
                if (msg != null) {
                    let result = msg.response.result;
                    console.log('consumer received message on topic corenode.gps', msg);
                    result.nodeid.should.eql(sensorSample.nodeid);
                    parseFloat(result.latitude).should.eql(sensorSample.lat);
                    parseFloat(result.longitude).should.eql(sensorSample.lon);
                    done();
                }
            });

        });
    });

    it('should clean up nodes after', function (done) {
                var nodeids = ['gpscorenode001', 'gpscorenode002'];
                var nodeid = null;
                var dirty = nodeids.length;
                while (nodeid = nodeids.shift()) {
                    var uri = version + '/customers/uberorg/sites/ubersite/nodes';
                    var delete_req = request.delete(uri + "/" + nodeid);
                    agent.attachCookies(delete_req);
                    delete_req
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(204)
                        .end(function (err, res) {
                            should.not.exist(err);

                            if (!(--dirty))
                                done();
                        });
                }
            });

});