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
var configManager = require('kea-config');
configManager.setup('./config/');
var pgid = "Unknown";

const helpers = require('./../../utils/helpers');
let csrfToken = null;

/*
 Datadealer needs to be running for these tests to be successful
 % lein with-profile testjs run all

 Run suite of tests with
 % cd Farallones/api_service/ui_api
 % mocha --recursive
 */
describe('Parking', function () {
    describe('GET /customers/{orgid}/sites/{siteid}/parkinggroups', function () {
        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers/uberorg/sites/ubersite/parkinggroups')
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.should.eql({ error: true, message: 'Access not granted', status: 403 });
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


        it("should link and then unlink a set of users from a parkingGroup", function (done) {
            // First, create a user
            var userid = "uberuser";
            // Next, create a parking group
            var data = {
                name: "MyParkingGroup",
                description: "test parking group api",
                policy: "Standard parking",
                vehicle_types: "car",
                parkingzones: "05F1BE54-49C1-424C-BB29-CEEF13F06E51,3FEF28B7-4686-44B6-9CC5-FD8F72B2281B"
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/parkinggroups');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    if (err && res.body) console.error(res.body);
                    should.not.exist(err);
                    var result = res.body;
                    var parkinggroupid = result.parkinggroupid;
                    pgid = parkinggroupid;
                    delete result.parkinggroupid;
                    result.should.eql(data);
                    data = {
                        userList: [userid, "parkinguser"]
                    };
                    // Assign users in bulk to my parking group

                    var uri = version + "/customers/uberorg/sites/ubersite/parkinggroups/" + parkinggroupid + "/users/assign";
                    var req = request.post(uri);
                    agent.attachCookies(req);
                    req.send(data)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(204)
                        .end(function (err, res) {
                            should.not.exist(err);

                            // Now, unassign the above set of users from parking group
                            var uri = version + "/customers/uberorg/sites/ubersite/parkinggroups/" + parkinggroupid + "/users/unassign";
                            var req = request.post(uri);
                            agent.attachCookies(req);
                            req.send(data)
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect(204)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    done();
                                });

                        });
                });
        });



        it("should access parkingzones with login credentials", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/parkingzones');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    result.length.should.eql(4);
                    var inactive = null;
                    var with_spots = null;
                    for (var i = 0; i < result.length; i++) {
                        if (result[i].active === false) {
                            inactive = result[i];
                        }
                        if (result[i].spots && result[i].spots.length) {
                            with_spots = result[i];
                        }
                    }
                    should.exists(inactive);
                    should.exists(with_spots);
                    inactive.should.eql({
                        "orgid": "uberorg",
                        "siteid": "ubersite",
                        "parkinggroupid": pgid,
                        "parkingzoneid": "3FEF28B7-4686-44B6-9CC5-FD8F72B2281B",
                        "nodeid": "ubernode",
                        "channel": 1,
                        length: '8.05',
                        "type": "NonDemarcated",
                        "active": false,
                        "max_spaces": 1,
                        "occupied_spaces": 0,
                        "available_spaces": 1,
                        "world_bounding_box": [
                            { latitude: 41.88763317533143, longitude: -87.62229817127228 },
                            { latitude: 41.887634173730426, longitude: -87.62226363769172 },
                            { latitude: 41.88756728107507, longitude: -87.62225793797458 },
                            { latitude: 41.88757027627205, longitude: -87.62229951238321 }
                        ],
                        "image_bounding_box": [
                            { x: 0.6086523532867432, y: 0.39174535870552063 },
                            { x: 0.8420314192771912, y: 0.3664957880973816 },
                            { x: 0.7694030404090881, y: 0.17103376984596252 },
                            { x: 0.56187903881073, y: 0.20528662204742432 }
                        ],
                        "spots": [],
                        "config": {
                            "active": false,
                            "channel": 1,
                            "configured_date": "2017-03-07 20:13:55.44",
                            "name": "ubersite ubernode 2",
                            "nodeid": "ubernode",
                            "orgid": "uberorg",
                            "parkingzoneid": "3FEF28B7-4686-44B6-9CC5-FD8F72B2281B",
                            "roi": {
                                "image_bounding_box": [
                                    [
                                        {
                                            "x": 0.6086523532867432,
                                            "y": 0.39174535870552063
                                        },
                                        {
                                            "x": 0.8420314192771912,
                                            "y": 0.3664957880973816
                                        },
                                        {
                                            "x": 0.7694030404090881,
                                            "y": 0.17103376984596252
                                        },
                                        {
                                            "x": 0.56187903881073,
                                            "y": 0.20528662204742432
                                        }
                                    ]
                                ],
                                "name": "Parking_Area_1",
                                "roiid": "5F33ADD6-9D08-488C-BD74-E74A61C67F4F",
                                "world_bounding_box": [
                                    [
                                        {
                                            "latitude": 41.88763317533143,
                                            "longitude": -87.62229817127228
                                        },
                                        {
                                            "latitude": 41.887634173730426,
                                            "longitude": -87.62226363769172
                                        },
                                        {
                                            "latitude": 41.88756728107507,
                                            "longitude": -87.62225793797458
                                        },
                                        {
                                            "latitude": 41.88757027627205,
                                            "longitude": -87.62229951238321
                                        }
                                    ]
                                ]
                            },
                            "siteid": "ubersite",
                            "tag": "",
                            "type": "NonDemarcatedParkingConfig"
                        }
                    });
                    with_spots.should.eql({
                        "orgid": "uberorg",
                        "siteid": "ubersite",
                        "parkinggroupid": "Unknown",
                        "parkingzoneid": "6CCCC435-FCD3-457C-9E38-3B4B3E00325D",
                        "nodeid": "ubernode",
                        "channel": 1,
                        "type": "Demarcated",
                        "active": true,
                        //"max_spaces": 0,
                        "occupied_spaces": 1,
                        "available_spaces": null,
                        "world_bounding_box": [
                            { latitude: 41.887581947612155, longitude: -87.62229833829926 },
                            { latitude: 41.887635228589495, longitude: -87.6222982094155 },
                            { latitude: 41.887640674139924, longitude: -87.62226554603113 },
                            { latitude: 41.88758085498861, longitude: -87.62226251720993 }
                        ],
                        "image_bounding_box": [
                            { x: 0.5694444179534912, y: 0.23789063096046448 },
                            { x: 0.6083333492279053, y: 0.4046874940395355 },
                            { x: 0.8333333134651184, y: 0.3968749940395355 },
                            { x: 0.7555555701255798, y: 0.20781250298023224 }
                        ],
                        "spots": [
                            {
                                "iv": [
                                    0.058746337890625,
                                    0.01987881027162075
                                ],
                                "s": 1498094923000000,
                                "world": {
                                    "lon": [
                                        -87.62227345273534,
                                        -87.62227404694615,
                                        -87.62229456839917,
                                        -87.62229397418837
                                    ],
                                    "lat": [
                                        41.88759756924216,
                                        41.88764016476583,
                                        41.88764000492582,
                                        41.887597409402154
                                    ]
                                },
                                "c": "Car",
                                "wh": 1.4194155931472778,
                                "wv": [
                                    0.0008829380385577679,
                                    1.93961501121521
                                ],
                                "uuid": "E5BF2A4B-5F06-4F03-B4D3-91C937044461",
                                "wp": 1,
                                "img": {
                                    "v": [
                                        0.572773814201355,
                                        0.775047242641449
                                    ],
                                    "u": [
                                        0.21649999916553497,
                                        0.42054852843284607
                                    ]
                                }
                            }
                        ],
                        "config": {
                            "active": true,
                            "channel": 1,
                            "configured_date": "2017-06-22 01:08:47.39",
                            "name": "ubersite othernode 6",
                            "nodeid": "ubernode",
                            "orgid": "uberorg",
                            "parkingzoneid": "6CCCC435-FCD3-457C-9E38-3B4B3E00325D",
                            "roi": {
                                "image_bounding_box": [
                                    [
                                        {
                                            "x": 0.5694444179534912,
                                            "y": 0.23789063096046448,
                                        },
                                        {
                                            "x": 0.6083333492279053,
                                            "y": 0.4046874940395355,
                                        },
                                        {
                                            "x": 0.8333333134651184,
                                            "y": 0.3968749940395355
                                        },
                                        {
                                            "x": 0.7555555701255798,
                                            "y": 0.20781250298023224
                                        }
                                    ]
                                ],
                                "name": "Parking_Area_1",
                                "roiid": "9620BE73-57E0-4FCC-8FFC-B21C521C98BA",
                                "world_bounding_box": [
                                    [
                                        {
                                            "latitude": 41.887581947612155,
                                            "longitude": -87.62229833829926
                                        },
                                        {
                                            "latitude": 41.887635228589495,
                                            "longitude": -87.6222982094155
                                        },
                                        {
                                            "latitude": 41.887640674139924,
                                            "longitude": -87.62226554603113
                                        },
                                        {
                                            "latitude": 41.88758085498861,
                                            "longitude": -87.62226251720993
                                        }
                                    ]
                                ]
                            },
                            "siteid": "othersite",
                            "spots": {
                                "o": [],
                                "occ": {
                                    "objectuuids": [],
                                    "spotuuids": []
                                }
                            },
                            "tag": "",
                            "type": "DemarcatedParkingConfig"
                        }
                    });
                    //Delete parking group
                    var delete_req = request.delete(version + '/customers/uberorg/sites/ubersite/parkinggroups' + "/" + pgid);
                    agent.attachCookies(delete_req);
                    delete_req.set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);
                            done();
                        });
                });
        });

        var parkinggroupid = null;

        it("should create a parkinggroup using credentials", function (done) {
            var data = {
                name: "MyParkingGroup",
                description: "test parking group api",
                policy: "Standard parking",
                vehicle_types: "car"
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/parkinggroups');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    parkinggroupid = result.parkinggroupid;
                    delete result.parkinggroupid;
                    result.should.eql(data);
                    done();
                });
        });

        it('should return a single parking group', function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/parkinggroups/' + parkinggroupid);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should return a 404 for a non existing group', function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/parkinggroups/blah');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    var result = res.body;
                    result.status.should.eql(404);
                    done();
                });
        });

        it("should get, update, and delete parkinggroup using credentials", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/parkinggroups";
            var data = {
                name: "MyParkingGroup",
                description: "test parking group api",
                policy: "Standard parking",
                vehicle_types: "car"
            };
            var req = request.get(uri + "/" + parkinggroupid);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var result = res.body;
                    delete result.parkinggroupid;
                    delete result.parkingzones;
                    delete result.site;
                    result.should.eql(data);

                    var updated = {
                        name: "MyParkingGroup",
                        description: "updated desc",
                        policy: "Standard parking",
                        vehicle_types: "car",
                        parkingzones: "blahblah"
                    };
                    var req2 = request.post(uri + "/" + parkinggroupid);
                    agent.attachCookies(req2);
                    req2.send(updated)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res2) {
                            var result = res2.body;
                            delete result.updated;
                            delete result.created;
                            delete result.parkinggroupid;
                            delete result.site;

                            result.should.eql(updated);
                            var delete_req = request.delete(uri + "/" + parkinggroupid);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    var req = request.get(version + '/customers/uberorg/sites/ubersite/parkinggroups/' + parkinggroupid);
                                    agent.attachCookies(req);
                                    req.expect('Content-Type', /json/)
                                        .expect(404)
                                        .end(function (err, res) {
                                            var result = res.body;
                                            result.status.should.eql(404);
                                            done();
                                        });

                                });
                        });
                });
        });

        it("should access parkinggroups with login credentials", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/parkinggroups";
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    if (res.body.length > 0) {
                        for (var i in res.body) {
                            var delete_req = request.delete(uri + "/" + res.body[i].parkinggroupid);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                });
                        }
                        done();
                    } else done();

                });
        });

        it('should get minimal subset of fields for all nodes (just N02c0002e or create it)', function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/minnodes');
            agent.attachCookies(req);
            req.send()
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    if (result.length === 0) {
                        var data = {
                            "nodeid": "N02c0002e",
                            "model": "vnode-v1"
                        };
                        var req = request.post(version + '/nodes/N02c0002e');
                        agent.attachCookies(req);
                        req.send(data)
                            .set('Accept', 'application/json')
                            .set('X-CSRF-Token', csrfToken)
                            .expect('Content-Type', /json/)
                            .expect(200)
                            .end(function (err, res) {
                                should.not.exist(err);
                                var result = res.body;
                                result.should.eql(data);

                                var nodeid = "N02c0002e";
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
        });

        it("should delete node N02c0002e using credentials", function(done) {
            var req = request.delete(version + '/customers/uberorg/sites/ubersite/nodes/N02c0002e');
            agent.attachCookies(req);
            req.send()
                .expect(204)
                .set('X-CSRF-Token', csrfToken)
                .end(function(err, res) {
                    should.not.exist(err);
                    done();
                });
        });
        /*
        // This is only really testable with a sim or with live nodes, good to have but can't be run in isolation
        
                    it("should access parkinginfo for a site with login credentials", function(done) {
                        var req = request.get(version+'/customers/uberorg/sites/ubersite/parking');
                        agent.attachCookies(req);
                        req.expect(200)
                            .end(function(err, res) {
                                should.not.exist(err);
                                //res.body.length.should.eql(0);
                                done();
                            });
        
                    });
        
                    it("should access historic parkinginfo", function(done) {
                        var req = request.get(version+'/customers/uberorg/sites/ubersite/parking');
                        agent.attachCookies(req);
                        // this is in microseconds
                        var todate = Date.now();
                        var from = todate-4*60*60*1000;
                        var filter = { from: from, to: todate };
                        req.send(filter)
                            .expect(200)
                            .end(function(err, res) {
                                should.not.exist(err);
                                console.log("\n********************* HISTORIC PARKING EVENTS **********************\n", res.body)
                                //res.body.length.should.eql(0);
                                done();
                            });
        
                    });
        */
        /*
                    it("should receive parking event", function(done) {
                        this.timeout(46000);
                        var subscription = {
                            uri:configManager.get('sse').url,
                            topics: ['/streamv1/efe5bdb3-baac-5d8e-6cae57771c13/718f22f0-f064-11e5-abe1-c124f900b420/N02c0002e/DemarcatedParkingEvent']
                        }
        
                        var mqttClient  = mqtt.connect(subscription.uri);
        
                        mqttClient.on('connect', function () {
                            global.log.info('Connected to %s', subscription.uri);
                            for(var j in subscription.topics){
                                mqttClient.subscribe(subscription.topics[j]);
                                global.log.info('Subscribed to %s', subscription.topics[j]);
                            }
                        });
        
                        var timer = setTimeout(function(){
                            global.log.warn('Please start DCC if not started');
                            mqttClient.end();
                            done();
                        }, 45000);
        
                        mqttClient.on('message', function(topic, message) {
                            try{
                                // message is Buffer
                                var response = JSON.parse(message.toString());
                                global.log.info('New message', topic, response);
                                if( response.nodeid == 'N02c0002e'){
                                    clearTimeout(timer);
                                    mqttClient.end();
                                    done();
                                }
                            } catch(e){
                                global.log.error('Error parsing received message: %s', e.message, e);
                            }
                        });
        
        
                    });
        */
        /*
                  it("should delete node N02c0002e using credentials", function(done) {
                      var req = request.delete(version + '/customers/uberorg/sites/ubersite/nodes/N02c0002e');
                      agent.attachCookies(req);
                      req.send()
                          .expect(204)
                          .end(function(err, res) {
                              should.not.exist(err);
                              done();
                          });
                  });
        */
    });
});
