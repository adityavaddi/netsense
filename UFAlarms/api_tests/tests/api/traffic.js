/**
 * Created by dignjatic on 07/17/16.
 */

"use strict";

var server_url = process.env.stack_url;
var should = require('should');
var request = require('supertest')(server_url);
var superagent = require('superagent');
var agent = superagent.agent();
var version = '/v3.0';
var mqtt = require('mqtt');
//var nodeid = "N02c00073";
var node_id = "N02c01087";
//var node_id = "N02c01081";
var stream = require('stream');

var configManager = require('kea-config');
configManager.setup('./config/');

const helpers = require('./../../utils/helpers');
let csrfToken = null;

// Some traffic event
var cnfevent = {
    "roi": {
        "roiid": "Obgytgx6Y.ZRW3j}@fq!",
        "name": "Event_2",
        "image_bounding_box": [
            {
                "x": 0.519444465637207,
                "y": 0.31914061307907104
            },
            {
                "x": 0.2944444417953491,
                "y": 0.5550781488418579
            }
        ],
        "world_bounding_box": [
            {
                "latitude": 33.915622734531816,
                "longitude": -117.88517292173817
            },
            {
                "latitude": 33.91568320597613,
                "longitude": -117.88512497879893
            }
        ]
    },
    "nodeid": "N02c01087",
    "type": "LineCrossingConfig",
    "eventid": "sometrafficconfigevnt",
    "configured_date": "2016-11-24 01:17:12.40",
    "siteid": "ubersite",
    "channel": 0,
    "active": true,
    "orgid": "uberorg",
    "user": "nr"
}

// Some traffic events
var lcevent = {
    time: 1477674256293862,
    trafficdetectioneventid: "sometrafficevent",
    type: "LineCrossingEvent",
    orgid: "uberorg",
    siteid: "ubersite",
    nodeid: "ubernode",
    channel: 0,
    count: 17113,
    detected_objects: [{
        detectedobjectid: ":dqkpC3sVwg+$}&11XWR",
        position_precision: 1.0,
        height: 3.0,
        class: "car",
        image_bounding_box: [{
            x: 0.0,
            y: 0.0
        },
        {
            x: 0.0,
            y: 0.0
        }],
        world_bounding_box: [{
            latitude: 0.0,
            longitude: 0.0
        },
        {
            latitude: 0.0,
            longitude: 0.0
        }],
        image_velocity: {
            x: 0.0,
            y: 0.0
        },
        world_velocity: {
            x: 2.5206684667514017,
            y: 9.699795106549171
        }
    }]
}

var olevent = JSON.parse(JSON.stringify(lcevent));
olevent.type = 'ObjectLeavingEvent';
var oleventx = JSON.parse(JSON.stringify(olevent));
oleventx.trafficdetectioneventid = 'sometrafficeventx';
oleventx.detected_objects[0].class = 'pedestrian';

/*
 Datadealer needs to be running for these tests to be successful
 % lein with-profile testjs run all

 Run suite of tests with
 % cd Farallones/api_service/ui_api
 % mocha --recursive
 */

describe('Traffic', function () {
    describe('GET /customers/{orgid}/sites/{siteid}/traffic', function () {

        it('should fail without login credentials', function (done) {

            var req = request.post(version + '/customers/uberorg/sites/ubersite/node/ubernode/traffic');
            var filter = {
                "startTime": "2017-03-08 00:00 UTC",
                "endTime": "2017-03-08 02:05 UTC",
                "timeResolution": "1hr",
                "type": "person"
            };
            req.send(filter)
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.should.eql({ error: true, message: 'Access denied', status: 403 });
                    done();
                });
        });

        it('should fail SSE without login credentials', function (done) {
            request
                .get('/streamv1/uberorg/ubersite/+/TrafficDetectionEvent?type=ObjectLeavingEvent')
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.should.eql({ error: true, message: 'Please log in' });
                    done();
                });
        });

        it('should sign in with correct credentials', function (done) {
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
                    done();
                });

        });


        it('should get minimal subset of fields for all nodes (just ' + node_id + ' or create it)', function (done) {
            //setTimeout(function(){
            var req = request.get(version + '/customers/uberorg/sites/ubersite/minnodes');
            agent.attachCookies(req);
            req.send()
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    if (true || result.length === 0) {
                        var data = {
                            "nodeid": node_id,
                            "model": "vnode-v1"
                        };
                        var req = request.post(version + '/nodes/' + node_id);
                        agent.attachCookies(req);
                        req.send(data)
                            .set('Accept', 'application/json')
                            .set('X-CSRF-Token', csrfToken)
                            .expect('Content-Type', /json/)
                            .expect(200)
                            .end(function (err, res) {
                                //should.not.exist(err);
                                //var result = res.body;
                                //result.should.eql(data);

                                var nodeid = node_id;
                                var uri = version + '/customers/uberorg/sites/ubersite/nodes';
                                var req = request.post(uri + '/' + nodeid + '/assign');
                                agent.attachCookies(req);
                                req
                                    .set('Content-Type', 'application/json')
                                    .set('Accept', 'application/json')
                                    .set('X-CSRF-Token', csrfToken)
                                    .expect('Content-Type', /json/)
                                    .expect(200)
                                    .end(function (err, res) {
                                        should.not.exist(err);
                                        done();
                                    });
                            });
                    } else done();
                });
            //},60000);
        });
        /*
        // This is only really testable with a sim or with live nodes, good to have but can't be run in isolation
        
                    it("should access trafficinginfo for a site with login credentials", function(done) {
                        var req = request.get(version+'/customers/uberorg/sites/ubersite/traffic');
                        agent.attachCookies(req);
                        req.expect(200)
                            .end(function(err, res) {
                                should.not.exist(err);
                                //res.body.length.should.eql(0);
                                done();
                            });
        
                    });
        
         */
        it("should access traffic config for a node", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/node/N02c01087/traffic/config');
            agent.attachCookies(req);
            var filter = {
            };
            req.send(filter)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var results = res.body;
                    //console.log("\n********************* HISTORIC TRAFFIC CONFIGS **********************\n", res.body)
                    results.length.should.eql(3);
                    results[Math.floor(Math.random() * results.length)].nodeid.should.eql('N02c01087');
                    results[Math.floor(Math.random() * results.length)].siteid.should.eql('ubersite');
                    results[Math.floor(Math.random() * results.length)].orgid.should.eql('uberorg');

                    // Try to filter by name
                    filter.type = "LineCrossingConfig";
                    var req = request.post(version + '/customers/uberorg/sites/ubersite/node/N02c01087/traffic/config');
                    agent.attachCookies(req);
                    req.send(filter)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);
                            var results = res.body;
                            //console.log("\n********************* HISTORIC TRAFFIC CONFIGS **********************\n", res.body)
                            results.length.should.eql(2);
                            results[Math.floor(Math.random() * results.length)].type.should.eql('LineCrossingConfig');

                            // Try to filter by eventid
                            filter.eventid = "tuTqBT8go:H#QO=t*OmY";
                            delete filter.name;
                            var req = request.post(version + '/customers/uberorg/sites/ubersite/node/N02c01087/traffic/config');
                            agent.attachCookies(req);
                            req.send(filter)
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    var results = res.body;
                                    //console.log("\n********************* HISTORIC TRAFFIC CONFIGS **********************\n", res.body)
                                    results.length.should.eql(1);
                                    results[Math.floor(Math.random() * results.length)].eventid.should.eql('tuTqBT8go:H#QO=t*OmY');

                                    // Try to filter by empty nodeid
                                    filter.nodeid = "";
                                    delete filter.eventid;
                                    var req = request.post(version + '/customers/uberorg/sites/ubersite/node//traffic/config');
                                    agent.attachCookies(req);
                                    req.send(filter)
                                        .set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect(404)
                                        .end(function (err, res) {
                                            should.not.exist(err);
                                            var results = res.body;

                                            // Try to filter by empty eventid
                                            filter.eventid = "";
                                            delete filter.nodeid;
                                            var req = request.post(version + '/customers/uberorg/sites/ubersite/node/N02c01087/traffic/config');
                                            agent.attachCookies(req);
                                            req.send(filter)
                                                .set('Accept', 'application/json')
                                                .set('X-CSRF-Token', csrfToken)
                                                .expect(200)
                                                .end(function (err, res) {
                                                    should.not.exist(err);
                                                    var results = res.body;
                                                    done();
                                                });
                                        });
                                });
                        });
                });

        });

        it("should access traffic config for a site", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/traffic/config');
            agent.attachCookies(req);
            var filter = {

            };
            req.send(filter)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var results = res.body;
                    //console.log("\n********************* HISTORIC TRAFFIC CONFIGS **********************\n", res.body)
                    results.length.should.eql(4);
                    results[Math.floor(Math.random() * results.length)].siteid.should.eql('ubersite');

                    // Try to filter by name
                    filter.type = "ObjectEnteringConfig";
                    var req = request.post(version + '/customers/uberorg/sites/ubersite/traffic/config');
                    agent.attachCookies(req);
                    req.send(filter)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);
                            var results = res.body;
                            //console.log("\n********************* HISTORIC TRAFFIC CONFIGS **********************\n", res.body)
                            results.length.should.eql(1);
                            results[Math.floor(Math.random() * results.length)].type.should.eql('ObjectEnteringConfig');

                            // Try to filter by eventid
                            filter.eventid = "qFytScJ=YmIiP(Q+napH";
                            delete filter.type;
                            var req = request.post(version + '/customers/uberorg/sites/ubersite/traffic/config');
                            agent.attachCookies(req);
                            req.send(filter)
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    var results = res.body;
                                    //console.log("\n********************* HISTORIC TRAFFIC CONFIGS **********************\n", res.body)
                                    results.length.should.eql(1);
                                    results[Math.floor(Math.random() * results.length)].eventid.should.eql('qFytScJ=YmIiP(Q+napH');

                                    done();
                                });
                        });
                });

        });

        it("should access current traffic info, node level", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/node/ubernode/traffic');
            agent.attachCookies(req);
            var filter = {
                "nodeid": "ubernode",
            };
            req.send(filter)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var results = res.body;
                    //console.log("\n********************* CURRENT TRAFFIC EVENTS **********************\n", res.body)
                    results.length.should.eql(3);
                    results[Math.floor(Math.random() * results.length)].nodeid.should.eql('ubernode');
                    results[Math.floor(Math.random() * results.length)].aggregated_count.should.eql(5);
                    results[0].count_per_class.should.eql({car: 4927});
                    done();
                });

        });

        it("should filter current traffic info by type, node level", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/node/ubernode/traffic');
            agent.attachCookies(req);
            var filter = {
                "nodeid": "ubernode",
                "type": "ObjectLeavingEvent"
            };
            req.send(filter)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var results = res.body;
                    //console.log("\n********************* CURRENT TRAFFIC EVENTS **********************\n", res.body)
                    results.length.should.eql(0);
                    done();
                });

        });
        it("should access current traffic info, site level", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/traffic');
            agent.attachCookies(req);
            var filter = {
                "siteid": "ubersite",
            };
            req.send(filter)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var results = res.body;
                    //console.log("\n********************* CURRENT TRAFFIC EVENTS **********************\n", res.body)
                    results.length.should.eql(5);
                    results[Math.floor(Math.random() * results.length)].siteid.should.eql('ubersite');
                    results[Math.floor(Math.random() * results.length)].aggregated_count.should.eql(5);
                    done();
                });
        });
        it("should access current traffic info, site level", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/traffic');
            agent.attachCookies(req);
            var filter = {
                "siteid": "ubersite",
                "active_filter": "inactive"
            };
            req.send(filter)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var results = res.body;
                    results.length.should.eql(2);
                    results[Math.floor(Math.random() * results.length)].siteid.should.eql('ubersite');
                    results[Math.floor(Math.random() * results.length)].active.should.eql(false);
                    done();
                });
        });
        it("should filter current traffic info, site level", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/traffic');
            agent.attachCookies(req);
            var filter = {
                "siteid": "ubersite",
                "trafficdetectioneventid": "R6YG3T44*#]5r9usP(uA",
                "active_filter": 'inactive'
            };
            req.send(filter)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var results = res.body;
                    results.length.should.eql(1);
                    results[Math.floor(Math.random() * results.length)].siteid.should.eql('ubersite');
                    results[Math.floor(Math.random() * results.length)].trafficdetectioneventid.should.eql('R6YG3T44*#]5r9usP(uA');
                    done();
                });
        });

        it("should access historic traffic info, node level", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/node/ubernode/traffic');
            agent.attachCookies(req);
            var filter = {
                "startTime": "2017-02-21 12:00 UTC",
                "endTime": "2017-02-21 14:05 UTC",
                "timeResolution": "15min",
                "nodeid": "ubernode",
                "object_class": "car"
            };
            req.send(filter)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var results = res.body;
                    //console.log("\n********************* HISTORIC TRAFFIC EVENTS **********************\n", res.body)
                    results.length.should.eql(55);
                    results[0].startdt.should.eql('2017-02-21 12:00');
                    results[results.length - 1].startdt.should.eql('2017-02-21 14:00');
                    results[Math.floor(Math.random() * results.length)].object_class.toLowerCase().should.eql('car');
                    results[Math.floor(Math.random() * results.length)].siteid.should.eql('ubersite');
                    should.exist(results[Math.floor(Math.random() * results.length)].avgvelocity);
                    should.exist(results[Math.floor(Math.random() * results.length)].avgdwell);

                    var req = request.post(version + '/customers/uberorg/sites/ubersite/node/ubernode/traffic');
                    agent.attachCookies(req);
                    // Should filter by event name
                    filter.type = 'ObjectLeavingEvent';
                    req.send(filter)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);
                            var results = res.body;
                            //console.log("\n********************* HISTORIC TRAFFIC EVENTS **********************\n", res.body)
                            results.length.should.eql(18);
                            results[0].startdt.should.eql('2017-02-21 12:00');
                            results[results.length - 1].startdt.should.eql('2017-02-21 14:00');
                            results[Math.floor(Math.random() * results.length)].object_class.toLowerCase().should.eql('car');
                            results[Math.floor(Math.random() * results.length)].siteid.should.eql('ubersite');
                            results[Math.floor(Math.random() * results.length)].type.should.eql('ObjectLeavingEvent');
                            should.not.exist(results[Math.floor(Math.random() * results.length)].avgdwell);
                            should.exist(results[Math.floor(Math.random() * results.length)].avgvelocity);

                            var req = request.post(version + '/customers/uberorg/sites/ubersite/node/ubernode/traffic');
                            agent.attachCookies(req);
                            // Should filter by event name
                            filter.type = 'ObjectDwellEvent';
                            req.send(filter)
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    var results = res.body;
                                    //console.log("\n********************* HISTORIC TRAFFIC EVENTS **********************\n", res.body)
                                    results.length.should.eql(1);
                                    results[Math.floor(Math.random() * results.length)].type.should.eql('ObjectDwellEvent');
                                    should.exist(results[Math.floor(Math.random() * results.length)].enddt);
                                    should.exist(results[Math.floor(Math.random() * results.length)].avgdwell);
                                    should.not.exist(results[Math.floor(Math.random() * results.length)].avgvelocity);
                                    done();
                                });

                        });
                });

        });
        it("should access historic traffic info, site level", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/traffic');
            agent.attachCookies(req);
            var filter = {
                "startTime": "2017-03-08 00:00 UTC",
                "endTime": "2017-03-08 02:05 UTC",
                "timeResolution": "1hr",
                "object_class": "person"
            };
            req.send(filter)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var results = res.body;
                    //console.log("\n********************* HISTORIC TRAFFIC EVENTS **********************\n", res.body)
                    //res.body.length.should.eql(0);
                    results.length.should.eql(3);
                    results[0].startdt.should.eql('2017-03-08 00:00');
                    results[results.length - 1].startdt.should.eql('2017-03-08 02:00');
                    results[Math.floor(Math.random() * results.length)].object_class.toLowerCase().should.eql('person');
                    results[Math.floor(Math.random() * results.length)].siteid.should.eql('ubersite');
                    done();
                });

        });

        it('should fail SSE without valid orgid and siteid', function (done) {
            request
                .get('/streamv1/uberorgx/ubersite/+/TrafficDetectionEvent?type=ObjectLeavingEvent')
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    //res.body.should.eql({ error: true, message: 'Please log in' });

                    request
                        .get('/streamv1/uberorg/ubersitex/+/TrafficDetectionEvent?type=ObjectLeavingEvent')
                        .set('Accept', 'application/json')
                        .expect('Content-Type', /json/)
                        .expect(403)
                        .end(function (err, res) {
                            should.not.exist(err);
                            //res.body.should.eql({ error: true, message: 'Please log in' });
                            done();
                        });

                });
        });


        if (false)   // Should be rewritten
            it("should receive traffic event", function (done) {
                var timer, total = 0;
                var reqsse, skip_done = false;
                var subscription = {
                    uri: configManager.get('sse').url,
                    topics: ['/streamv1/uberorg/ubersite/' + node_id + '/traffic/TrafficDetectionEvent']
                };

                // Create sse consumer
                class SSEStream extends stream.Writable {
                    _write(chunk, enc, next) {
                        var str = chunk.toString();
                        if (str !== ("\n") && str !== (':' + '\n')) {
                            try {
                                var response = JSON.parse(chunk.toString().substr(5));
                                console.log('SSEStream received:', response);
                                // TODO Add test
                                if (response && response.trafficdetectioneventid === 'sometrafficevent') {
                                    if (skip_done)
                                        return;
                                    // Should receive only ObjectLeavingEvent
                                    response.should.eql(olevent);
                                    global.log.warn("Got correct traffic event");
                                    if (total < 3) {
                                        console.log("Please try this test with dcc running");
                                    }
                                    if (timer)
                                        clearTimeout(timer);
                                    mqttClient.end();
                                    skip_done = true;
                                    done();
                                }
                                // response.should
                            } catch (e) {
                                console.error(e)
                            }

                        }
                        next();
                    }
                }

                var writable = new SSEStream();
                // Subscribe to sse
                reqsse = request.get('/streamv1/uberorg/ubersite/+/TrafficDetectionEvent?type=ObjectLeavingEvent&trafficdetectioneventid=sometrafficevent&object_class=car');
                agent.attachCookies(reqsse);
                reqsse.pipe(writable);

                var mqttClient = mqtt.connect(subscription.uri);

                mqttClient.on('connect', function () {
                    global.log.info('Connected to %s', subscription.uri);
                    for (var j in subscription.topics) {
                        mqttClient.subscribe(subscription.topics[j]);
                        global.log.info('Subscribed to %s', subscription.topics[j]);
                    }

                    timer = setTimeout(function () {
                        mqttClient.end();
                        var msg = 'Error: something went wrong with dynamic traffic events.';
                        if (!total)
                            msg += ' Please check is mosca running.';
                        done({ error: true, message: msg });
                    }, 1000);

                    mqttClient.on('message', function (topic, message) {
                        try {
                            // message is Buffer
                            var response = JSON.parse(message.toString());
                            global.log.info('New message', topic, response);
                            if (response.nodeid == node_id) {
                                total++;
                            }
                        } catch (e) {
                            global.log.error('Error parsing received message: %s', e.message, e);
                        }
                    });

                    setTimeout(function () {
                        mqttClient.publish(subscription.topics[0], JSON.stringify(lcevent));
                    }, 200);
                    setTimeout(function () {
                        mqttClient.publish(subscription.topics[0], JSON.stringify(olevent));
                    }, 200);
                });

            });

        if (false)   // Should be rewritten
            it("should filter traffic events", function (done) {
                var timer, total = 0;
                var reqsse, skip_done = false;
                var subscription = {
                    uri: 'mqtt://127.0.0.1:3002',
                    topics: ['/streamv1/uberorg/ubersite/' + node_id + '/traffic/TrafficDetectionEvent']
                }

                // Create sse consumer
                class SSEStream1 extends stream.Writable {
                    _write(chunk, enc, next) {
                        var str = chunk.toString();
                        if (str !== ("\n") && str !== (':' + '\n')) {
                            try {
                                var response = JSON.parse(chunk.toString().substr(5));
                                console.log('SSEStream1 received:', response);
                                // TODO Add test
                                if (response && response.type == 'ObjectLeavingEvent' && response.trafficdetectioneventid === 'sometrafficevent') {
                                    if (skip_done)
                                        return;
                                    // Should receive only ObjectLeavingEvent
                                    response.should.eql(olevent);
                                    global.log.warn("Got correct traffic event");
                                    if (timer)
                                        clearTimeout(timer);
                                    mqttClient.end();
                                    skip_done = true;
                                    done();
                                } else if (response && response.type == 'ObjectLeavingEvent' && response.trafficdetectioneventid === 'sometrafficeventx') {
                                    if (skip_done)
                                        return;
                                    // Should receive only ObjectLeavingEvent
                                    //response.should.not.eql(olevent);
                                    var msg = "Got incorrect traffic event";
                                    global.log.error(msg);
                                    if (timer)
                                        clearTimeout(timer);
                                    mqttClient.end();
                                    skip_done = true;
                                    done({ error: true, message: msg });
                                } else if (response && response.type == 'LineCrossingConfig' && response.trafficdetectioneventid === 'sometrafficconfigevnt') {
                                    if (skip_done)
                                        return;
                                    // Should receive only ObjectLeavingEvent
                                    //response.should.not.eql(olevent);
                                    var msg = "Should not receive config event";
                                    global.log.error(msg);
                                    if (timer)
                                        clearTimeout(timer);
                                    mqttClient.end();
                                    skip_done = true;
                                    done({ error: true, message: msg });
                                }
                                // response.should
                            } catch (e) {
                                console.error(e)
                            }

                        }
                        next();
                    }
                }
                var writable = new SSEStream1();
                // Subscribe to sse
                reqsse = request.get('/streamv1/uberorg/ubersite/+/TrafficDetectionEvent?type=ObjectLeavingEvent&trafficdetectioneventid=sometrafficevent');
                agent.attachCookies(reqsse);
                reqsse.pipe(writable);

                var mqttClient = mqtt.connect(subscription.uri);

                mqttClient.on('connect', function () {
                    global.log.info('Connected to %s', subscription.uri);
                    for (var j in subscription.topics) {
                        mqttClient.subscribe(subscription.topics[j]);
                        global.log.info('Subscribed to %s', subscription.topics[j]);
                    }

                    timer = setTimeout(function () {
                        mqttClient.end();
                        var msg = 'Error: something went wrong with dynamic traffic events.';
                        if (!total)
                            msg += ' Please check is mosca running.';
                        done({ error: true, message: msg });
                    }, 1000);

                    mqttClient.on('message', function (topic, message) {
                        try {
                            // message is Buffer
                            var response = JSON.parse(message.toString());
                            global.log.info('New message', topic, response);
                            if (response.nodeid == node_id) {
                                total++;
                            }
                        } catch (e) {
                            global.log.error('Error parsing received message: %s', e.message, e);
                        }
                    });

                    setTimeout(function () {
                        try {
                            mqttClient.publish(subscription.topics[0], JSON.stringify(oleventx));
                        } catch (e) { }
                    }, 200);
                    setTimeout(function () {
                        try {
                            mqttClient.publish(subscription.topics[0], JSON.stringify(cnfevent));
                        } catch (e) { }
                    }, 200);
                    setTimeout(function () {
                        try {
                            mqttClient.publish(subscription.topics[0], JSON.stringify(olevent));
                        } catch (e) { }
                    }, 200);
                });

            });

        if (false)   // Should be rewritten
            it("should receive traffic config event", function (done) {
                this.timeout(56000);
                var timer, total = 0;
                var reqsse, skip_done = false;
                var subscription = {
                    uri: 'mqtt://127.0.0.1:3002',
                    topics: ['/streamv1/uberorg/ubersite/' + node_id + '/traffic/TrafficDetectionEvent']
                }

                // Create sse consumer
                class SSEStream2 extends stream.Writable {
                    _write(chunk, enc, next) {
                        var str = chunk.toString();
                        if (str !== ("\n") && str !== (':' + '\n')) {
                            try {
                                var response = JSON.parse(chunk.toString().substr(5));
                                console.log('SSEStream2 received:', response);
                                // TODO Add test
                                if (response && response.type == 'LineCrossingConfig' && response.eventid === 'sometrafficconfigevnt') {
                                    if (skip_done)
                                        return;
                                    // Should receive only LineCrossingConfig
                                    response.should.eql(cnfevent);
                                    global.log.warn("Got correct traffic event");
                                    if (timer)
                                        clearTimeout(timer);
                                    mqttClient.end();
                                    skip_done = true;
                                    done();
                                }
                                // response.should
                            } catch (e) {
                                console.error(e)
                            }

                        }
                        next();
                    }
                }
                var writable = new SSEStream2();
                // Subscribe to sse
                reqsse = request.get('/streamv1/uberorg/ubersite/+/TrafficDetectionEvent?includeconfig=true');
                agent.attachCookies(reqsse);
                reqsse.pipe(writable);

                var mqttClient = mqtt.connect(subscription.uri);

                mqttClient.on('connect', function () {
                    global.log.info('Connected to %s', subscription.uri);
                    for (var j in subscription.topics) {
                        mqttClient.subscribe(subscription.topics[j]);
                        global.log.info('Subscribed to %s', subscription.topics[j]);
                    }

                    timer = setTimeout(function () {
                        mqttClient.end();
                        var msg = 'Error: something went wrong with dynamic traffic events.';
                        if (!total)
                            msg += ' Please check is mosca running.';
                        done({ error: true, message: msg });
                    }, 55000);

                    mqttClient.on('message', function (topic, message) {
                        try {
                            // message is Buffer
                            var response = JSON.parse(message.toString());
                            global.log.info('New message', topic, response);
                            if (response.nodeid == node_id) {
                                total++;
                            }
                        } catch (e) {
                            global.log.error('Error parsing received message: %s', e.message, e);
                        }
                    });

                    setTimeout(function () {
                        try {
                            mqttClient.publish(subscription.topics[0], JSON.stringify(oleventx));
                        } catch (e) { }
                    }, 3000);
                    setTimeout(function () {
                        try {
                            mqttClient.publish(subscription.topics[0], JSON.stringify(olevent));
                        } catch (e) { }
                    }, 5000);
                    setTimeout(function () {
                        try {
                            mqttClient.publish(subscription.topics[0], JSON.stringify(cnfevent));
                        } catch (e) { }
                    }, 7000);
                });

            });


        if (false)   // Should be rewritten
            it("should filter out traffic event by object class", function (done) {
                this.timeout(56000);
                var timer, total = 0;
                var reqsse, skip_done = false;
                var subscription = {
                    uri: 'mqtt://127.0.0.1:3002',
                    topics: ['/streamv1/uberorg/ubersite/' + node_id + '/traffic/TrafficDetectionEvent']
                }

                // Create sse consumer
                class SSEStream3 extends stream.Writable {
                    _write(chunk, enc, next) {
                        var str = chunk.toString();
                        if (str !== ("\n") && str !== (':' + '\n')) {
                            try {
                                var response = JSON.parse(chunk.toString().substr(5));
                                console.log('SSEStream3 received:', response);
                                // TODO Add test
                                if (response && response.detected_objects[0].class != 'pedestrian') {
                                    if (skip_done)
                                        return;
                                    global.log.error("Got incorrect traffic event");
                                    skip_done = true;
                                    done({ error: true, message: 'Got incorrect traffic event' });
                                } else if (response && response.detected_objects[0].class != 'car') {
                                    if (skip_done)
                                        return;
                                    // Should receive only ObjectLeavingEvent
                                    response.should.eql(oleventx);
                                    global.log.warn("Got correct traffic event");
                                    if (total < 3) {
                                        console.log("Please try this test with dcc running");
                                    }
                                    if (timer)
                                        clearTimeout(timer);
                                    mqttClient.end();
                                    skip_done = true;
                                    done();
                                }
                                // response.should
                            } catch (e) {
                                console.error(e)
                            }

                        }
                        next();
                    }
                }

                var writable = new SSEStream3();
                // Subscribe to sse
                reqsse = request.get('/streamv1/uberorg/ubersite/+/TrafficDetectionEvent?object_class=pedestrian');
                agent.attachCookies(reqsse);
                reqsse.pipe(writable);

                var mqttClient = mqtt.connect(subscription.uri);

                mqttClient.on('connect', function () {
                    global.log.info('Connected to %s', subscription.uri);
                    for (var j in subscription.topics) {
                        mqttClient.subscribe(subscription.topics[j]);
                        global.log.info('Subscribed to %s', subscription.topics[j]);
                    }

                    timer = setTimeout(function () {
                        mqttClient.end();
                        var msg = 'Error: something went wrong with dynamic traffic events.';
                        if (!total)
                            msg += ' Please check is mosca running.';
                        done({ error: true, message: msg });
                    }, 55000);

                    mqttClient.on('message', function (topic, message) {
                        try {
                            // message is Buffer
                            var response = JSON.parse(message.toString());
                            global.log.info('New message', topic, response);
                            if (response.nodeid == node_id) {
                                total++;
                            }
                        } catch (e) {
                            global.log.error('Error parsing received message: %s', e.message, e);
                        }
                    });

                    setTimeout(function () {
                        mqttClient.publish(subscription.topics[0], JSON.stringify(olevent));
                    }, 3000);
                    setTimeout(function () {
                        mqttClient.publish(subscription.topics[0], JSON.stringify(oleventx));
                    }, 5000);
                });

            });

        it("should delete node " + node_id + " using credentials", function (done) {
            var req = request.delete(version + '/customers/uberorg/sites/ubersite/nodes/' + node_id);
            agent.attachCookies(req);
            req.send()
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });


    });
});

