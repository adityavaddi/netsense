/**
 * Created by brefsdal on 12/16/15.
 * 
 */

"use strict";
var server_url = process.env.stack_url;
var should = require('should');
var request = require('supertest')(server_url);
var superagent = require('superagent');
var agent = superagent.agent();
var version = '/v3.0';
var mqtt = require('mqtt');
var msgpack = require('msgpack5')();
var amqp = require('amqplib/callback_api');
var v_node_id = "mqtt_alarm_node";
var c_node_id = "rabbitmq_alarm_node";
var exchange = 'node.events';

var configManager = require('kea-config');
configManager.setup('./config/');

const helpers = require('./../../utils/helpers');
let csrfToken = null;


/*
Required Components: Datadealer, MQTT, RabbitMQ, Kafka, Cassandra, Interface Service, ACL Service Bridge Service, Alert Service

 Run suite of tests with
 % cd Farallones/api_tests/tests/api
 % mocha --recursive
 */


describe('Alerts', function () {
    describe('GET /customers/{orgid}/sites/{siteid}/alerts', function () {
        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers/uberorg/sites/ubersite/alerts')
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.should.eql({ error: true, message: 'Access denied', status: 403 });
                    done();
                });
        });

        it('should sign in with correct credentials', function (done) {
            this.timeout(16000);
            var data = { email: "uberuser@sensity.com", password: "ubeR$23" };
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
                    var req = request.get(version + '/customers/uberorg/sites/ubersite/alerts');
                    agent.attachCookies(req);
                    req.end(function (err, res) {
                        done();
                    });
                });
        });

        it('should create test nodes', function (done) {
            var createNodeReq = request.put(version + '/nodes');
            agent.attachCookies(createNodeReq);
            var payload = {
                csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude\n" +
                "alertscorenodeid001,unode-v4,uberorg,ubersite,44.4,41.2\n" +
                "alertsvideonodeid001,falcon-q,uberorg,ubersite,44.4,41.2\n" +
                v_node_id + ",falcon-q,uberorg,ubersite,44.4,41.2\n" +
                c_node_id + ",unode-v4,uberorg,ubersite,44.4,41.2\n"
            };
            createNodeReq.send(payload)
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res2) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should access all site alerts with login credentials', function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/alerts');
            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(0);
                    done();
                });
        });

        it("should create alert from core node alarm posted to RabbitMQ", function (done) {
            var alarm = {
                name: "DeviceAlarm",
                nodeid: c_node_id,
                alarmType: "HWFail_RTC",
                alarmSeverity: "Critical",
                msg: "Hardware Failure"
            }
            var rabbitConfig = configManager.get('rabbit').url;
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
                    ch.assertExchange(exchange, 'topic', { durable: true });
                    ch.publish(exchange, '*.alarm.*', msgpack.encode(alarm), { persistent: true },
                        function (err) {
                            if (err) {
                                console.log("Error posting alarm to RabbitMQ: ", err);
                                return;
                            }
                            else {
                                console.log(" Alarm posted to RabbitMQ queue: ", alarm);
                            }
                        });
                });
                setTimeout(function () { conn.close(); done(); }, 4500);
            });
        });

        it("should get, dismiss and delete alert created from RabbitMQ using credentials", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/alerts";
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var alertid = res.body[0].alertid;
                    console.log('RabbitMQ AlertID: ', alertid)
                    var dismiss_req = request.post(uri + "/dismiss/" + alertid);
                    agent.attachCookies(dismiss_req);
                    dismiss_req.send({})
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            console.log('Dismissed RabbitMQ Alert: ', alertid);
                            should.not.exist(err);
                            var delete_req = request.delete(uri + "/" + alertid);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    console.log('Deleted RabbitMQ Alert: ', alertid);
                                    agent.attachCookies(delete_req);
                                    var delete_req1 = request.delete(uri + "/" + alertid);
                                    delete_req1.set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect('Content-Type', /json/)
                                        .expect(404)
                                        .end(function (err, res) {
                                            console.log('Deleting Alert that do not exist: ', alertid);
                                            var get_req = request.get(uri + "/" + alertid);
                                            agent.attachCookies(get_req);
                                            get_req.set('Accept', 'application/json')
                                                .expect('Content-Type', /json/)
                                                .expect(404)
                                                .end(function (err, res) {
                                                    var get_all_req = request.get(uri);
                                                    agent.attachCookies(get_all_req);
                                                    get_all_req.set('Accept', 'application/json')
                                                        .expect('Content-Type', /json/)
                                                        .expect(204)
                                                        .end(function (err, res) {
                                                            res.body.should.eql([]);
                                                            done();
                                                        });
                                                });
                                        });
                                });
                        });
                });
        });

        it("should create clear alert from alarm posted to RabbitMQ", function (done) {
            var alarm = {
                name: "DeviceAlarm",
                nodeid: c_node_id,
                alarmType: "HWFail_RTC",
                alarmSeverity: "Clear",
                msg: "Hardware Failure"
            }
            var rabbitConfig = configManager.get('rabbit').url;
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
                    ch.assertExchange(exchange, 'topic', { durable: true });
                    ch.publish(exchange, '*.alarm.*', msgpack.encode(alarm), { persistent: true },
                        function (err) {
                            if (err) {
                                console.log("Error posting alarm to RabbitMQ: ", err);
                                return;
                            }
                            else {
                                console.log(" Alarm posted to RabbitMQ queue: ", alarm);
                            }
                        });
                });
                setTimeout(function () { conn.close(); done(); }, 4500);
            });
        });

        it('should not display clear core node alerts with login credentials', function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/alerts');
            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(0);
                    done();
                });
        });

        it("should create alert from alarm posted on the alarm topic", function (done) {
            var subscription = {
                uri: configManager.get('sse').url,
                topics: ['v1/' + v_node_id + '/out/UNSOL/alarm/#']
            };
            var mqttClient = mqtt.connect(subscription.uri);
            mqttClient.on('connect', function () {
                console.log('Connected to %s', subscription.uri);
                for (var j in subscription.topics) {
                    mqttClient.subscribe(subscription.topics[j]);
                    //console.log('Subscribed to %s', subscription.topics[j]);
                }
                var timer = setTimeout(function () {
                    mqttClient.end();
                    done();
                }, 4500);
                mqttClient.on('message', function (topic, message) {
                    try {
                        var response = msgpack.decode(message);
                        response.sid.should.eql(v_node_id);
                        //console.log('New message', topic, response);
                    } catch (e) {
                        console.log('Error parsing received message: %s', e.message, e);
                    }
                });
                var alarmPayload = {
                    s: 1,
                    a: 'SW/storage/volmissing/data',
                    m: '/data volume missing',
                    c: 3
                };
                var alarm = {
                    uuid: '51e481b2-1f4e-4a6f-9b4e-111222333',
                    f: 'SW/storage/volmissing/data',
                    a: 'UNSOL',
                    l: msgpack.encode(alarmPayload),
                    p: 'v1/' + v_node_id + '/out/UNSOL/alarm/{alarmid}',
                    sid: v_node_id,
                    d: '2017-09-18T15:33:22Z'
                };
                setTimeout(function () { mqttClient.publish('v1/' + v_node_id + '/out/UNSOL/alarm', msgpack.encode(alarm)) }, 300);
            });
        });

        it("should get, dismiss and delete alert created from MQTT using credentials", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/alerts";
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var alertid = res.body[0].alertid;
                    console.log('MQTT AlertID: ', alertid)
                    var dismiss_req = request.post(uri + "/dismiss/" + alertid);
                    agent.attachCookies(dismiss_req);
                    dismiss_req.send({})
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            console.log('Dismissed MQTT Alert: ', alertid);
                            should.not.exist(err);
                            var delete_req = request.delete(uri + "/" + alertid);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    console.log('Deleted MQTT Alert: ', alertid);
                                    agent.attachCookies(delete_req);
                                    var delete_req1 = request.delete(uri + "/" + alertid);
                                    delete_req1.set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect('Content-Type', /json/)
                                        .expect(404)
                                        .end(function (err, res) {
                                            console.log('Deleting Alert that do not exist: ', alertid);
                                            var get_req = request.get(uri + "/" + alertid);
                                            agent.attachCookies(get_req);
                                            get_req.set('Accept', 'application/json')
                                                .expect('Content-Type', /json/)
                                                .expect(404)
                                                .end(function (err, res) {
                                                    var get_all_req = request.get(uri);
                                                    agent.attachCookies(get_all_req);
                                                    get_all_req.set('Accept', 'application/json')
                                                        .expect('Content-Type', /json/)
                                                        .expect(204)
                                                        .end(function (err, res) {
                                                            res.body.should.eql([]);
                                                            done();
                                                        });
                                                });
                                        });
                                });
                        });
                });
        });

        it("should create clear alert from alarm posted to MQTT", function (done) {
            var subscription = {
                uri: configManager.get('sse').url,
                topics: ['v1/' + v_node_id + '/out/UNSOL/alarm/#']
            };
            var mqttClient = mqtt.connect(subscription.uri);
            mqttClient.on('connect', function () {
                console.log('Connected to %s', subscription.uri);
                for (var j in subscription.topics) {
                    mqttClient.subscribe(subscription.topics[j]);
                    //console.log('Subscribed to %s', subscription.topics[j]);
                }
                var timer = setTimeout(function () {
                    mqttClient.end();
                    done();
                }, 4500);
                mqttClient.on('message', function (topic, message) {
                    try {
                        var response = msgpack.decode(message);
                        response.sid.should.eql(v_node_id);
                        //console.log('New message', topic, response);
                    } catch (e) {
                        console.log('Error parsing received message: %s', e.message, e);
                    }
                });
                var alarmPayload = {
                    s: 0,
                    a: 'SW/storage/volmissing/data',
                    m: '/data volume missing',
                    c: 3
                };
                var alarm = {
                    uuid: '51e481b2-1f4e-4a6f-9b4e-111222333',
                    f: 'SW/storage/volmissing/data',
                    a: 'UNSOL',
                    l: msgpack.encode(alarmPayload),
                    p: 'v1/' + v_node_id + '/out/UNSOL/alarm/{alarmid}',
                    sid: v_node_id,
                    d: '2017-09-18T15:35:28Z'
                };
                setTimeout(function () { mqttClient.publish('v1/' + v_node_id + '/out/UNSOL/alarm', msgpack.encode(alarm)) }, 300);
            });
        });

        it('should not display clear video node alerts with login credentials', function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/alerts');
            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(0);
                    done();
                });
        });

        it("should create an alert for core node using credentials", function (done) {
            var data = {
                name: "DeviceAlarm",
                nodeid: "alertscorenodeid001",
                orgid: "uberorg",
                siteid: "ubersite",
                type: "HWFail_EEPROM",
                category: "Sensor",
                severity: "Major",
                msg: "Up"
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/alerts');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    delete result.alertid;
                    delete result.updated;
                    delete result.created;
                    delete result.active;
                    delete result.nodehw;
                    delete result.bssid;
                    delete result.sitename;
                    delete result.ufname;
                    delete result.description;
                    delete result.displaytopartner;
                    delete result.displaytocustomer;
                    result.should.eql(data);
                    done();
                });
        });

        it("should access alerts with login credentials and correct nodeid", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/alerts/node/alertscorenodeid001');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(1);
                    done();
                });
        });

        it("should get, dismiss and delete core node alert using credentials", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/alerts";
            var data = {
                name: "DeviceAlarm",
                nodeid: "alertscorenodeid001",
                orgid: "uberorg",
                siteid: "ubersite",
                type: "HWFail_EEPROM",
                category: "Sensor",
                severity: "Major",
                msg: "Up"
            };
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var alertid = res.body[0].alertid;
                    console.log('Core Node AlertID: ', alertid)
                    delete res.body[0].alertid;
                    delete res.body[0].updated;
                    delete res.body[0].created;
                    delete res.body[0].active;
                    delete res.body[0].nodehw;
                    delete res.body[0].bssid;
                    delete res.body[0].sitename;
                    delete res.body[0].ufname;
                    delete res.body[0].description;
                    delete res.body[0].displaytopartner;
                    delete res.body[0].displaytocustomer;
                    res.body[0].should.eql(data);
                    var dismiss_req = request.post(uri + "/dismiss/" + alertid);
                    agent.attachCookies(dismiss_req);
                    dismiss_req.send({})
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);
                            console.log('Dismissed Core Node Alert: ', alertid);
                            var delete_req = request.delete(uri + "/" + alertid);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    console.log('Deleted Core Node Alert: ', alertid);
                                    var get_req = request.get(uri + "/" + alertid);
                                    agent.attachCookies(get_req);
                                    get_req.set('Accept', 'application/json')
                                        .expect('Content-Type', /json/)
                                        .expect(404)
                                        .end(function (err, res) {
                                            var get_all_req = request.get(uri);
                                            agent.attachCookies(get_all_req);
                                            get_all_req.set('Accept', 'application/json')
                                                .expect('Content-Type', /json/)
                                                .expect(204)
                                                .end(function (err, res) {
                                                    res.body.should.eql([]);
                                                    done();
                                                });
                                        });
                                });
                        });
                });
        });

        it("should create an alert for video node using credentials", function (done) {
            var data = {
                name: "DeviceAlarm",
                nodeid: "alertsvideonodeid001",
                orgid: "uberorg",
                siteid: "ubersite",
                type: "HWFail_EEPROM",
                category: "Sensor",
                severity: "Clear",
                msg: "Up"
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/alerts');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    delete result.alertid;
                    delete result.updated;
                    delete result.created;
                    delete result.active;
                    delete result.nodehw;
                    delete result.bssid;
                    delete result.sitename;
                    delete result.ufname;
                    delete result.description;
                    delete result.displaytopartner;
                    delete result.displaytocustomer;
                    result.should.eql(data);
                    done();
                });
        });

        it("should get, dismiss and delete video node alert using credentials", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/alerts";
            var data = {
                name: "DeviceAlarm",
                nodeid: "alertsvideonodeid001",
                orgid: "uberorg",
                siteid: "ubersite",
                type: "HWFail_EEPROM",
                category: "Sensor",
                severity: "Clear",
                msg: "Up"
            };
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var alertid = res.body[0].alertid;
                    console.log('Video Node AlertID: ', alertid)
                    delete res.body[0].alertid;
                    delete res.body[0].updated;
                    delete res.body[0].created;
                    delete res.body[0].active;
                    delete res.body[0].nodehw;
                    delete res.body[0].bssid;
                    delete res.body[0].sitename;
                    delete res.body[0].ufname;
                    delete res.body[0].description;
                    delete res.body[0].displaytopartner;
                    delete res.body[0].displaytocustomer;
                    res.body[0].should.eql(data);
                    var dismiss_req = request.post(uri + "/dismiss/" + alertid);
                    agent.attachCookies(dismiss_req);
                    dismiss_req.send({})
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            console.log('Dismissed Video Node Alert: ', alertid);
                            should.not.exist(err);
                            var delete_req = request.delete(uri + "/" + alertid);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    console.log('Deleted Video Node Alert: ', alertid);
                                    var get_req = request.get(uri + "/" + alertid);
                                    agent.attachCookies(get_req);
                                    get_req.set('Accept', 'application/json')
                                        .expect('Content-Type', /json/)
                                        .expect(404)
                                        .end(function (err, res) {
                                            var get_all_req = request.get(uri);
                                            agent.attachCookies(get_all_req);
                                            get_all_req.set('Accept', 'application/json')
                                                .expect('Content-Type', /json/)
                                                .expect(204)
                                                .end(function (err, res) {
                                                    res.body.should.eql([]);
                                                    done();
                                                });
                                        });
                                });
                        });
                });
        });
    });

    describe("Clean up nodes", function (done) {
        it('should clean up nodes after', function (done) {
            var nodeids = ['alertscorenodeid001', 'alertsvideonodeid001', v_node_id, c_node_id];
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
});
