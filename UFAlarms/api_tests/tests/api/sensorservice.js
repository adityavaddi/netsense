'use strict'
var server_url = process.env.stack_url;
//var server_url = 'https://nsn-local.sensity.com';
var should = require('should');
//var server  = require(server_url);
var request = require('supertest')(server_url);
var msgpack = require('msgpack5')();
var superagent = require('superagent');
var agent = superagent.agent();
var version = '/v3.0';
var mqtt = require('mqtt');
var amqp = require('amqplib/callback_api');
var node_id = "testsensormochanode";
var core_node_id = "testcoresensormochanode";
var stream = require('stream');
var configManager = require('kea-config');
configManager.setup('./config/');
const helpers = require('./../../utils/helpers');
let csrfToken = null;
/* Datadealer needs to be running for these tests to be successful
 % lein with-profile testjs run all
 Run suite of tests with
 % cd Farallones/api_service/ui_api
 % mocha --recursive
*/

let fixtureid = null;
// Sample data stream
describe('Sensor Service', function() {
    it('should sign in with correct credentials', function(done) {
        var data = {
            email: "uberuser@sensity.com",
            password: "ubeR$23"
        };
        request
            .post(version + '/login')
            .send(data)
            .set('Accept', 'application/json')
            .expect('Content-Type', /json/)
            .expect(200)
            .end(function(err, res) {
                should.not.exist(err);
                agent.saveCookies(res);
                csrfToken = helpers.getCsrfToken(res);
                done();
            });
    });
    it('should create node for sensor', function(done) {
        var createNodereq = request.put(version + '/nodes');
        agent.attachCookies(createNodereq);
        var payload = {
            csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude\ntestsensormochanode,falcon-q,uberorg,ubersite,44.1,41.2\ntestcoresensormochanode,unode-v2,uberorg,ubersite,44.1,41.2"
        };
        createNodereq.send(payload)
            //.expect('Content-Type', /json/)
            .set('X-CSRF-Token', csrfToken)
            .expect(200)
            .end(function(err, res2) {
                should.not.exist(err);
                done();
            });
    });

    /*
     * SensorSample Data
     */
    it("should receive sensor sample data stream from ss", function(done) {
        var timer, total = 0;
        var reqss, skip_done = false;
        var subscription = {
            uri: configManager.get('sse').url,
            topics: ['/streamv1/uberorg/ubersite/' + node_id + '/SensorSample']
        };
        var mqttClient = mqtt.connect(subscription.uri);
        mqttClient.on('connect', function() {
            console.log('Connected to %s', subscription.uri);
            for (var j in subscription.topics) {
                mqttClient.subscribe(subscription.topics[j]);
                console.log('Subscribed to %s', subscription.topics[j]);
            }
            timer = setTimeout(function() {
                mqttClient.end();
                done();
            }, 1500);
            mqttClient.on('message', function(topic, message) {
                try {
                    // message is Buffer
                    var response = JSON.parse(message.toString());
                    response.nodeid.should.eql("testsensormochanode");
                    console.log('New message', topic, response);
                    if (response.nodeid == node_id) {
                        total++;
                    }
                } catch (e) {
                    global.log.error('Error parsing received message: %s', e.message, e);
                }
            });
            var sensorSamplePayload = {
                n: 'degC',
                s: 'jt',
                v: 123.11,
                t: 1477674256293862
            };
            var sensorSample = {
                uuid: '51e481b2-1f4e-4a6f-9b4e-b11c87b7d887',
                sid: 'testsensormochanode',
                a: 'POST',
                p: 'v1/testsensormochanode/out/UNSOL/sensors',
                f: '',
                l: msgpack.encode(sensorSamplePayload)
            };
            setTimeout(function() {
                mqttClient.publish('v1/testsensormochanode/out/UNSOL/sensors', msgpack.encode(sensorSample))
            }, 100);
        });
    });
    it("should get sensor samples from a node", function(done) {
        var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/testsensormochanode/sensors/jtm/date/2016-03-07T14:07:40.00Z/limit/10');
        agent.attachCookies(req);
        req
            .set('Content-Type', 'application/json')
            .set('Accept', 'application/json')
            .set('X-CSRF-Token', csrfToken)
            .expect('Content-Type', /json/)
            .expect(200)
            .end(function(err, res) {
                console.log('Got Sensor Samples');
                should.not.exist(err);
                res.body.datapoints.length.should.eql(1);
                var results = res.body;
                done();
            });
    });
    it("should get sensor samples from a node for a specified period", function(done) {
        var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/testsensormochanode/sensors/jtm/from/2016-03-07T02:07:01.00Z/to/2017-03-12T14:07:41.00Z/limit/100');
        agent.attachCookies(req);
        req
            .set('Content-Type', 'application/json')
            .set('Accept', 'application/json')
            .set('X-CSRF-Token', csrfToken)
        req.expect('Content-Type', /json/)
            .expect(200)
            .end(function(err, res) {
                should.not.exist(err);
                res.body.datapoints.length.should.eql(1);
                done();
            });
    });

    // Toggling down this mocha test for 3.0.7 (sensor power alarms)
    /*   it("should send driver level SensorSample corenode data stream to RabbitMQ", function (done) {
           var d = Date.now() * 1000;
           var fixtureSensorSampleLT = {
               name: 'FixtureSensorSample',
               nodeid: 'testcoresensormochanode',
               sensor: 'lt',
               units: 'w',
               time: d,
               value: 65
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
                   var exch = 'node.events';
                   ch.assertExchange(exch, 'topic', {
                       durable: true
                   });
                   ch.publish(exch, '*.sensor.*', msgpack.encode(fixtureSensorSampleLT), {
                           persistent: true
                       },
                       function (err) {
                           if (err) {
                               console.log("Error posting driver level sensor to RabbitMQ: ", err);
                               return;
                           } else {
                               console.log(" Sensor posted to RabbitMQ exchange : ", exch);
                           }
                       });
               });
               setTimeout(function () {
                   conn.close();
                   done();
               }, 1500);
           });
       });*/

    /*
     * Sending SensorSample message to RabbitMQ
     */
    it("should send SensorSample corenode data stream to RabbitMQ", function(done) {
        var sensorSample = {
            name: 'SensorSample',
            nodeid: 'testcoresensormochanode',
            sensor: 'p',
            units: 'mWs',
            time: 1515612128076938,
            value: 1
        }
        var rabbitConfig = configManager.get('rabbit').url;
        amqp.connect(rabbitConfig, function(err, conn) {
            if (err) {
                console.log("Rabbit MQ connection error: ", err);
                return;
            }
            console.log('Connected to %s', rabbitConfig);
            conn.createChannel(function(err, ch) {
                if (err) {
                    console.log("Rabbit MQ Channel creation error: ", err);
                    return;
                }
                var exch = 'node.events';
                ch.assertExchange(exch, 'topic', {
                    durable: true
                });
                ch.publish(exch, '*.sensor.*', msgpack.encode(sensorSample), {
                        persistent: true
                    },
                    function(err) {
                        if (err) {
                            console.log("Error posting sensor to RabbitMQ: ", err);
                            return;
                        } else {
                            console.log(" Sensor posted to RabbitMQ exchange : ", exch);
                        }
                    });
            });
            setTimeout(function() {
                conn.close();
                done();
            }, 1500);
        });
    });
    // Toggling down this mocha test for 3.0.7 (sensor power alarms)
    // create Fixture for a node
    /*   it("should create a fixture using credentials", function (done) {
           var data = {
               name: "My fixture",
               MaxPower0: "0",
               MaxPower10: "10",
               MaxPower50: "50",
               MaxPower100: "100",
               MinPower100: "100",
               MinPower50: "50",
               MinPower10: "10",
               MinPower0: "0"
           };
           var req = request.post(version + '/customers/uberorg/sites/ubersite/fixtures');
           agent.attachCookies(req);
           req.send(data)
               .set('Accept', 'application/json')
               .set('X-CSRF-Token', csrfToken)
               .expect('Content-Type', /json/)
               .expect(200)
               .end(function (err, res) {
                   should.not.exist(err);
                   var result = res.body;
                   delete result.fixtureid;
                   result.should.eql(data);
                   done();
               });
       });*/

    // Toggling down this mocha test for 3.0.7 (sensor power alarms)
    /*
        it("should apply a fixture to a node", function (done) {
            var nodeid = "testcoresensormochanode";
            var req = request.get(version + '/customers/uberorg/sites/ubersite/fixtures');
            agent.attachCookies(req);
            req.expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    fixtureid = res.body[0].fixtureid;
                    var areq = request.post(version + '/customers/uberorg/sites/ubersite/fixtures/' + fixtureid + '/assign/node/' + nodeid);
                    agent.attachCookies(areq);
                    areq.send()
                        .set('Content-Type', 'application/json')
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);
                            res.body.should.eql({
                                fixtureid: fixtureid,
                                nodeid: nodeid
                            });
                            var nreq = request.get(version + '/customers/uberorg/sites/ubersite/nodes/' + nodeid);
                            agent.attachCookies(nreq);
                            nreq.expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    var result = res.body,
                                        nfixtureid = result.fixtureid;
                                    nfixtureid.should.eql(fixtureid);
                                    done();
                                });
                        });
                });
        });
    */

    it("should send mP Sensor corenode data stream to RabbitMQ", function(done) {
        var sensorSample = {
            name: 'SensorSample',
            nodeid: 'testcoresensormochanode',
            sensor: 'mP',
            units: 'mW',
            time: 1515612128076938,
            value: 80
        }
        var rabbitConfig = configManager.get('rabbit').url;
        amqp.connect(rabbitConfig, function(err, conn) {
            if (err) {
                console.log("Rabbit MQ connection error: ", err);
                return;
            }
            console.log('Connected to %s', rabbitConfig);
            conn.createChannel(function(err, ch) {
                if (err) {
                    console.log("Rabbit MQ Channel creation error: ", err);
                    return;
                }
                var exch = 'node.events';
                ch.assertExchange(exch, 'topic', {
                    durable: true
                });
                ch.publish(exch, '*.sensor.*', msgpack.encode(sensorSample), {
                        persistent: true
                    },
                    function(err) {
                        if (err) {
                            console.log("Error posting sensor to RabbitMQ: ", err);
                            return;
                        } else {
                            console.log(" Sensor posted to RabbitMQ exchange : ", exch);
                        }
                    });
            });
            setTimeout(function() {
                conn.close();
                done();
            }, 1500);
        });
    });

    it("should get sensor samples from a  Core node", function(done) {
        var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/testcoresensormochanode/sensors/p/date/2016-03-07T14:07:40.00Z/limit/10');
        agent.attachCookies(req);
        req
            .set('Content-Type', 'application/json')
            .set('Accept', 'application/json')
            .set('X-CSRF-Token', csrfToken)
            .expect('Content-Type', /json/)
            .expect(200)
            .end(function(err, res) {
                console.log('Got Sensor Samples');
                should.not.exist(err);
                res.body.datapoints.length.should.eql(1);
                var results = res.body;
                done();
            });
    });
    it("should get Core node sensor samples from a node for a specified period", function(done) {
        var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/testcoresensormochanode/sensors/p/from/2015-03-07T02:07:01.00Z/to/2018-03-12T14:07:41.00Z/limit/10');
        agent.attachCookies(req);
        req
            .set('Content-Type', 'application/json')
            .set('Accept', 'application/json')
            .set('X-CSRF-Token', csrfToken)
        req.expect('Content-Type', /json/)
            .expect(200)
            .end(function(err, res) {
                should.not.exist(err);
                res.body.datapoints.length.should.eql(1);
                done();
            });
    });

    // Test for multiple sensors
    it("should get sensor samples from a  Core node", function(done) {
        var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/testcoresensormochanode/sensors/p,mP/date/2015-03-07T14:07:40.00Z/limit/10');
        agent.attachCookies(req);
        req
            .set('Content-Type', 'application/json')
            .set('Accept', 'application/json')
            .set('X-CSRF-Token', csrfToken)
            .expect('Content-Type', /json/)
            .expect(200)
            .end(function(err, res) {
                console.log('Got Sensor Samples');
                should.not.exist(err);
                res.body.datapoints.length.should.eql(2);
                var results = res.body;
                done();
            });
    });
    it("should get Core node sensor samples from a node for a specified period", function(done) {
        var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/testcoresensormochanode/sensors/p,mP/from/2015-03-07T02:07:01.00Z/to/2019-03-12T14:07:41.00Z/limit/10');
        agent.attachCookies(req);
        req
            .set('Content-Type', 'application/json')
            .set('Accept', 'application/json')
            .set('X-CSRF-Token', csrfToken)
        req.expect('Content-Type', /json/)
            .expect(200)
            .end(function(err, res) {
                should.not.exist(err);
                res.body.datapoints.length.should.eql(2);
                done();
            });
    });

    it("should get energy sensor samples from a node for a specified period", function(done) {
        var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/all/sensors/energy/from/2016-03-12T23:20:50.52Z/to/2016-03-12T23:20:55.52Z/limit/10');
        agent.attachCookies(req);
        req.expect('Content-Type', /json/)
            .expect(200)
            .end(function(err, res) {
                should.not.exist(err);
                done();
            });
    });
    it("should get energy sensor samples from a site for a specified period", function(done) {
        var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/all/sensors/energy/from/2016-03-12T23:20:50.52Z/to/2016-03-12T23:20:55.52Z/limit/10');
        agent.attachCookies(req);
        req.expect('Content-Type', /json/)
            .expect(200)
            .end(function(err, res) {
                should.not.exist(err);
                done();
            });
    });
    it("should get energy sensor aggregations per 15min from a node ", function(done) {
        var req = request.get(version + '/customers/uberorg/sites/ubersite/sensors/energy/from/2016-03-12T23:20:50.52Z/to/2016-03-12T23:20:55.52Z/limit/10/period/15min');
        agent.attachCookies(req);
        req.expect('Content-Type', /json/)
            .expect(200)
            .end(function(err, res) {
                should.not.exist(err);
                done();
            });
    });
    it("should get energy sensor aggregations per 15min from a site ", function(done) {
        var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/all/sensors/energy/from/2016-03-12T23:20:50.52Z/to/2016-03-12T23:20:55.52Z/limit/10/period/15min');
        agent.attachCookies(req);
        req.expect('Content-Type', /json/)
            .expect(200)
            .end(function(err, res) {
                should.not.exist(err);
                done();
            });
    });

    // Toggling down this mocha test for 3.0.7 (sensor power alarms)
    /* it("should get, dismiss and delete alert created from RabbitMQ using credentials", function (done) {
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
     });*/

    describe("Clean up nodes", function(done) {
        it('should clean up nodes after', function(done) {
            var nodeids = [node_id, core_node_id];
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
                    .end(function(err, res) {
                        should.not.exist(err);

                        if (!(--dirty))
                            done();
                    });
            }
        });
        // Toggling down this mocha test for 3.0.7 (sensor power alarms)
        /*        it('should delete fixture after', function (done) {
                    var uri = version + '/customers/uberorg/sites/ubersite/fixtures';
                    var delete_req = request.delete(uri + "/" + fixtureid);
                    agent.attachCookies(delete_req);
                    delete_req.set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(204)
                        .end(function (err, res) {
                            should.not.exist(err);
                            done();
                        });

                });*/
    });
});