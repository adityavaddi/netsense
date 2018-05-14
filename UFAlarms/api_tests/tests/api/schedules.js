/**
 * Created by brefsdal on 12/30/15.
 */
"use strict";
var server_url = process.env.stack_url;
var should = require('should');
var request = require('supertest')(server_url);
var Kafka = require('node-rdkafka')
var superagent = require('superagent');
var agent = superagent.agent();
var version = '/v3.0';
var configManager = require('kea-config');
configManager.init('./config/main.conf.js');

const helpers = require('./../../utils/helpers');
let csrfToken = null;

console.log('kafka connection:', configManager.get('kafka').kafkaurl)

var topics = ['schedule.command', 'ms.request.schedule', 'ms.command.schedule', 'light.command', 'ms.trigger.schedule']

var consumer;
var producer;

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
        topics: ['ms.request.schedule', 'schedule.command'],
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



function compareConsumedEvent(topic, matcher, callback) {
    readStream.on('data', function (data) {
        if (data.topic === topic && data.topic === 'ms.request.schedule') {
            var msg = JSON.parse(data.value);
            msg.messageid != null
            if (msg.request.nodeprops.nodeids.length > 0) {
                msg.request.siteprops.siteid.should.eql(matcher);
                callback(null);
            } else {
                console.log('event failed', msg)
                callback(null);
            }
        } else if (data.topic === topic && data.topic === 'schedule.command') {
            var msg = JSON.parse(data.value);
            msg.messageid != null
            if (msg.nodeids.length > 0) {
                msg.name.should.eql(matcher);
                callback(null);
            } else {
                console.log('event failed', msg)
                callback(null);
            }
        }
    });
};

/*
 Datadealer needs to be running for these tests to be successful
 % lein with-profile testjs run all

 Run suite of tests with
 % cd Farallones/api_service/ui_api
 % mocha --recursive
 */

let scheduleID;

describe('schedules', function () {

    describe('GET /customers/{orgid}/sites/{siteid}/schedules', function () {

        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers/uberorg/sites/ubersite/schedules')
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.should.eql({
                        error: true,
                        message: 'Access denied',
                        status: 403
                    });
                    done();
                });
        });

        it('should sign in with correct credentials', function (done) {
            var data = {
                email: "uberuser@sensity.com",
                password: "ubeR$23"
            };
            request
                .post(version + '/login')
                .send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    csrfToken = helpers.getCsrfToken(res);
                    done();
                });

        });

        it("should create a schedule using credentials", function (done) {
            var data = {
                name: "Test Schedule",
                description: "This is a test schedule",
                events: [{
                    days: ["mon", "tue", "wed", "thu", "fri"],
                    actions: [{
                        "time": "sunset",
                        "level": 100
                    }, {
                        "time": "sunrise+30",
                        "level": 0
                    }, {
                        "time": "23:00:00",
                        "level": 50
                    }, {
                        "time": "05:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise",
                        "level": 50
                    }, {
                        "time": "sunset-30",
                        "level": 50
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }, {
                    days: ["sat", "sun"],
                    actions: [{
                        "time": "05:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise+30",
                        "level": 0
                    }, {
                        "time": "sunset",
                        "level": 100
                    }, {
                        "time": "23:00:00",
                        "level": 0
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }, {
                    date: "2015-12-31",
                    actions: [{
                        "time": "00:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise",
                        "level": 0
                    }, {
                        "time": "sunset",
                        "level": 100
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }],
                network: {
                    highTime: "sunset",
                    highLevel: 80,
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }
            };

            var req = request.post(version + '/customers/uberorg/sites/ubersite/schedules');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    var scheduleid = result.scheduleid;
                    delete result.scheduleid;
                    delete result.sites;
                    //data.sites = { siteid: 'ubersite', sitename: 'Uber Site' };
                    delete result.groups;
                    delete result.nodes;
                    result.should.eql(data);
                    done();
                });
        });

        it("should create a schedule without dates in definition using credentials", function (done) {
            var data = {
                name: "Test Schedule without dates in definition",
                description: "This is a test schedule",
                events: [{
                    days: ["mon", "tue", "wed", "thu", "fri"],
                    actions: [{
                        "time": "sunset",
                        "level": 100
                    }, {
                        "time": "sunrise+30",
                        "level": 0
                    }, {
                        "time": "23:00:00",
                        "level": 50
                    }, {
                        "time": "05:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise",
                        "level": 50
                    }, {
                        "time": "sunset-30",
                        "level": 50
                    }, {
                        "time": "sunset-20",
                        "level": 50
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }, {
                    days: ["sat", "sun"],
                    actions: [{
                        "time": "05:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise+30",
                        "level": 0
                    }, {
                        "time": "sunset",
                        "level": 100
                    }, {
                        "time": "23:00:00",
                        "level": 0
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }],
                network: {
                    highTime: "sunset",
                    highLevel: 80,
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }
            };

            var req = request.post(version + '/customers/uberorg/sites/ubersite/schedules');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    var scheduleid = result.scheduleid;
                    delete result.scheduleid;
                    delete result.sites;
                    //data.sites = { siteid: 'ubersite', sitename: 'Uber Site' };
                    delete result.groups;
                    delete result.nodes;
                    result.should.eql(data);

                    // Delete it
                    var delete_req = request.delete(version + '/customers/uberorg/sites/ubersite/schedules/' + scheduleid);
                    agent.attachCookies(delete_req);
                    delete_req.set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {

                            return done();

                            var get_all_req = request.get('/customers/uberorg/sites/ubersite/schedules');
                            agent.attachCookies(get_all_req);
                            get_all_req.set('Accept', 'application/json')
                                //.expect('Content-Type', /json/)
                                .expect(204)
                                .end(function (err, res) {
                                    res.body.length.should.eql(1);
                                    done();
                                });
                        });

                });
        });

        it("should fail to create a schedule with wrong highTime/lowTime", function (done) {
            var data = {
                name: "Test Schedule",
                description: "This is a test schedule",
                events: [{
                    days: ["mon", "tue", "wed", "thu", "fri", "sat", "sun"],
                    actions: [{
                        "time": "sunset",
                        "level": 100
                    }]
                }],
                network: {
                    highTime: "w",
                    highLevel: 60,
                    lowTime: "06:00:00",
                    lowLevel: 0
                },
            };

            var req = request.post(version + '/customers/uberorg/sites/ubersite/schedules');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.exist(err);
                    done();
                });
        });

        it("should fail to create a schedule with mutiple time same day", function (done) {
            var data = {
                name: "Test Schedule",
                description: "This is a test schedule",
                events: [{
                    days: ["mon", "tue", "wed", "thu", "fri", "sat", "sun"],
                    actions: [{
                        "time": "sunset",
                        "level": 100
                    }],
                    photocell_enabled: false,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }, {
                    days: ["mon"],
                    actions: [{
                        "time": "sunset",
                        "level": 0
                    }],
                    photocell_enabled: false,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }],
                network: {
                    highTime: "08:00:00",
                    highLevel: 60,
                    lowTime: "06:00:00",
                    lowLevel: 0,
                    photocell_enabled: false,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                },
            };

            var req = request.post(version + '/customers/uberorg/sites/ubersite/schedules');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.exist(err);
                    done();
                });
        });

        it("should fail to create a schedule with no highTime and highLevel in network for non-photocell mode", function (done) {
            var data = {
                name: "Test Schedule",
                description: "This is a test schedule",
                events: [{
                    days: ["sat", "sun"],
                    actions: [{
                        "time": "05:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise+30",
                        "level": 0
                    }, {
                        "time": "sunset",
                        "level": 100
                    }, {
                        "time": "23:00:00",
                        "level": 0
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }, {
                    days: ["mon", "tue", "wed", "thu", "fri"],
                    actions: [{
                        "time": "sunset",
                        "level": 100
                    }, {
                        "time": "sunrise+30",
                        "level": 0
                    }, {
                        "time": "23:00:00",
                        "level": 50
                    }, {
                        "time": "05:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise",
                        "level": 50
                    }, {
                        "time": "sunset-30",
                        "level": 50
                    }, {
                        "time": "sunset-10",
                        "level": 50
                    }, {
                        "time": "sunset-5",
                        "level": 75
                    }, {
                        "time": "sunset-3",
                        "level": 80
                    }, {
                        "time": "sunset-2",
                        "level": 90
                    }, {
                        "time": "sunset-1",
                        "level": 95
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }, {
                    date: "2015-12-31",
                    actions: [{
                        "time": "00:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise",
                        "level": 0
                    }, {
                        "time": "sunset",
                        "level": 100
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }, {
                    date: "2015-12-30",
                    actions: [{
                        "time": "00:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise",
                        "level": 0
                    }, {
                        "time": "sunset",
                        "level": 100
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }],
                network: {
                    "photocell_enabled": false                 
                }
            };

            var req = request.post(version + '/customers/uberorg/sites/ubersite/schedules');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should get, update, and delete a schedule using credentials", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/schedules";
            var data = {
                name: "Test Schedule",
                description: "This is a test schedule",
                events: [{
                    days: ["mon", "tue", "wed", "thu", "fri"],
                    actions: [{
                        "time": "sunset",
                        "level": 100
                    }, {
                        "time": "sunrise+30",
                        "level": 0
                    }, {
                        "time": "23:00:00",
                        "level": 50
                    }, {
                        "time": "05:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise",
                        "level": 50
                    }, {
                        "time": "sunset-30",
                        "level": 50
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }, {
                    days: ["sat", "sun"],
                    actions: [{
                        "time": "05:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise+30",
                        "level": 0
                    }, {
                        "time": "sunset",
                        "level": 100
                    }, {
                        "time": "23:00:00",
                        "level": 0
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }, {
                    date: "2015-12-31",
                    actions: [{
                        "time": "00:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise",
                        "level": 0
                    }, {
                        "time": "sunset",
                        "level": 100
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }],
                network: {
                    highTime: "sunset",
                    highLevel: 80,
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }
            };
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var result = res.body.filter(function (schedule) {
                        return schedule.name != "Default Schedule"
                    })[0];
                    var scheduleid = result.scheduleid;
                    delete result.scheduleid;
                    delete result.sites;
                    delete result.groups;
                    delete result.nodes;

                    var get_req = request.get(uri + "/" + scheduleid);
                    agent.attachCookies(get_req);
                    get_req.set('Accept', 'application/json')
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            delete res.body.scheduleid;
                            delete res.body.sites;
                            delete res.body.groups;
                            delete res.body.nodes;

                            res.body.should.eql(data);

                            var delete_req = request.delete(uri + "/" + scheduleid);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {

                                    var get_all_req = request.get(uri);
                                    agent.attachCookies(get_all_req);
                                    get_all_req.set('Accept', 'application/json')
                                        //.expect('Content-Type', /json/)
                                        .expect(204)
                                        .end(function (err, res) {
                                            res.body.length.should.eql(1);
                                            done();
                                        });
                                });
                        });
                });
        });

        it("should create a schedule and group and apply schedule to the group", function (done) {
            var schdata = {
                name: "Test Schedule",
                description: "This is a test schedule",
                events: [{
                    days: ["mon", "tue", "wed", "thu", "fri"],
                    actions: [{
                        "time": "sunset",
                        "level": 100
                    }, {
                        "time": "sunrise+30",
                        "level": 0
                    }, {
                        "time": "23:00:00",
                        "level": 50
                    }, {
                        "time": "05:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise",
                        "level": 50
                    }, {
                        "time": "sunset-30",
                        "level": 50
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }, {
                    days: ["sat", "sun"],
                    actions: [{
                        "time": "05:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise+30",
                        "level": 0
                    }, {
                        "time": "sunset",
                        "level": 100
                    }, {
                        "time": "23:00:00",
                        "level": 0
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }, {
                    date: "2015-12-31",
                    actions: [{
                        "time": "00:00:00",
                        "level": 100
                    }, {
                        "time": "sunrise",
                        "level": 0
                    }, {
                        "time": "sunset",
                        "level": 100
                    }],
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }],
                network: {
                    highTime: "08:00:00",
                    highLevel: 60,
                    lowTime: "06:00:00",
                    lowLevel: 0,
                    photocell_enabled: true,
                    photocell_highLevel: 100,
                    photocell_lowLevel: 0
                }
            },
                scheduleid, groupid, nodeid = 'N0de2835chdld';

            var req = request.post(version + '/customers/uberorg/sites/ubersite/schedules');
            agent.attachCookies(req);
            req.send(schdata)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    scheduleID = result.scheduleid;
                    scheduleid = result.scheduleid;

                    // should create an empty node (N0de2835chdld) using credentials
                    var data = {
                        "nodeid": nodeid,
                        "model": "unode-v2"
                    };
                    var req = request.post(version + '/nodes/N0de2835chdld');
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

                            // Assign node
                            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes' + '/' + nodeid + '/assign');
                            agent.attachCookies(req);
                            req //.send(data)
                                .set('Content-Type', 'application/json')
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);

                                    // Should create a group
                                    var groupdata = {
                                        nodeList: [nodeid],
                                        name: "My sch group",
                                        type: "lighting",
                                        description: "My schedule test group"
                                    };
                                    req = request.post(version + '/customers/uberorg/sites/ubersite/groups');
                                    agent.attachCookies(req);
                                    req.send(groupdata)
                                        .set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect('Content-Type', /json/)
                                        .expect(200)
                                        .end(function (err, res) {
                                            should.not.exist(err);
                                            var result = res.body;
                                            var groupid = result.groupid;

                                            // Should fail to apply schedule to the non existing group
                                            req = request.post(version + '/customers/uberorg/sites/ubersite/schedules/' + 'ImN0r3a1' + '/apply/groups/' + groupid);
                                            agent.attachCookies(req);
                                            req.send('')
                                                .set('Content-Type', 'application/json')
                                                .set('Accept', 'application/json')
                                                .set('X-CSRF-Token', csrfToken)
                                                .expect('Content-Type', /json/)
                                                .expect(404)
                                                .end(function (err, res) {
                                                    should.not.exist(err);
                                                    // Should fail to apply schedule to the non existing group
                                                    req = request.post(version + '/customers/uberorg/sites/ubersite/schedules/' + scheduleid + '/apply/groups/' + 'ImN0r3a1');
                                                    agent.attachCookies(req);
                                                    req.send('')
                                                        .set('Content-Type', 'application/json')
                                                        .set('Accept', 'application/json')
                                                        .set('X-CSRF-Token', csrfToken)
                                                        .expect('Content-Type', /json/)
                                                        .expect(404)
                                                        .end(function (err, res) {
                                                            should.not.exist(err);

                                                            // Should apply schedule to the group
                                                            req = request.post(version + '/customers/uberorg/sites/ubersite/schedules/' + scheduleid + '/apply/groups/' + groupid);
                                                            agent.attachCookies(req);
                                                            req.send('')
                                                                .set('Content-Type', 'application/json')
                                                                .set('Accept', 'application/json')
                                                                .set('X-CSRF-Token', csrfToken)
                                                                .expect('Content-Type', /json/)
                                                                .expect(200)
                                                                .end(function (err, res) {
                                                                    should.not.exist(err);
                                                                    res.body.groups[0].groupid.should.eql(groupid);

                                                                    // Update schedule
                                                                    schdata.events[0] = {
                                                                        days: ["mon", "tue", "wed", "thu", "fri"],
                                                                        actions: [{
                                                                            "time": "sunset",
                                                                            "level": 100
                                                                        }, {
                                                                            "time": "sunrise+30",
                                                                            "level": 0
                                                                        }, {
                                                                            "time": "23:00:00",
                                                                            "level": 51
                                                                        }, {
                                                                            "time": "05:00:00",
                                                                            "level": 99
                                                                        }, {
                                                                            "time": "sunrise",
                                                                            "level": 50
                                                                        }, {
                                                                            "time": "sunset-30",
                                                                            "level": 50
                                                                        }],
                                                                        photocell_enabled: true,
                                                                        photocell_highLevel: 100,
                                                                        photocell_lowLevel: 0
                                                                    };

                                                                    var req = request.post(version + '/customers/uberorg/sites/ubersite/schedules/' + scheduleid);
                                                                    agent.attachCookies(req);
                                                                    req.send(schdata)
                                                                        .set('Accept', 'application/json')
                                                                        .set('X-CSRF-Token', csrfToken)
                                                                        .expect('Content-Type', /json/)
                                                                        .expect(200)
                                                                        .end(function (err, res) {
                                                                            should.not.exist(err);
                                                                            var result = res.body;
                                                                            scheduleid = result.scheduleid;
                                                                            done();
                                                                        });
                                                                });

                                                        });
                                                });

                                        });

                                });
                        });
                });
        });

        it("should access schedules with login credentials", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/schedules');
            agent.attachCookies(req);
            req //.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(2);
                    done();
                });

        });
    });

    describe("GET /customers/{orgid}/sites/{siteid}/daylightharvesting", function () {

        it("should create a Daylight Harvesting profile using credentials", function (done) {
            var data = {
                "name": "Test DH Profile",
                "highLux": 100,
                "highDriver": 0,
                "lowLux": 50,
                "lowDriver": 20,
                "minLux": 10,
                "minDriver": 80,
                "fastPoll": 30,
                "slowPoll": 600,
                "scheduled": [{
                    "beginTime": "sunrise-90",
                    "endTime": "sunset+90"
                },
                {
                    "beginTime": "20:00:00",
                    "endTime": "23:00:00"
                }
                ]
            };

            var req = request.post(version + '/customers/uberorg/sites/ubersite/daylightharvesting');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    if (err && res.body) console.error(res.body);
                    if (err) console.error(err);
                    should.not.exist(err);
                    var result = res.body;
                    delete result.etdhprofileid;
                    result.should.eql(data);
                    done();
                });
        });

        it("should get all daylight harvesting profiles using credentials", function (done) {
            var data = {
                "name": "Test DH Profile",
                "highLux": 100,
                "highDriver": 0,
                "lowLux": 50,
                "lowDriver": 20,
                "minLux": 10,
                "minDriver": 80,
                "fastPoll": 30,
                "slowPoll": 600,
                "scheduled": [{
                    "beginTime": "sunrise-90",
                    "endTime": "sunset+90"
                },
                {
                    "beginTime": "20:00:00",
                    "endTime": "23:00:00"
                }
                ]
            };

            var uri = version + "/customers/uberorg/sites/ubersite/daylightHarvesting";
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    if (err && res.body) console.error(res.body);
                    if (err) console.error(err);
                    should.not.exist(err);
                    res.body.should.not.eql([]);

                    var etdhpid = res.body[0].etdhprofileid;
                    var get_req = request.get(uri + "/" + etdhpid);
                    agent.attachCookies(get_req);
                    get_req.set('Accept', 'application/json')
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            if (err && res.body) console.error(res.body);
                            if (err) console.error(err);
                            should.not.exist(err);
                            delete res.body.etdhprofileid;
                            delete res.body.sites;
                            delete res.body.groups;
                            delete res.body.nodes;
                            res.body.should.eql(data);

                            var update_req = request.put(uri + "/" + etdhpid);
                            agent.attachCookies(update_req);
                            data['minLux'] = 5;
                            update_req.send(data)
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    if (err && res.body) console.error(res.body);
                                    if (err) console.error(err);
                                    should.not.exist(err);
                                    delete res.body.etdhprofileid;
                                    delete res.body.sites;
                                    delete res.body.groups;
                                    delete res.body.nodes;
                                    res.body.should.eql(data);

                                    var get_req = request.get(uri + "/" + etdhpid);
                                    agent.attachCookies(get_req);
                                    get_req.set('Accept', 'application/json')
                                        .expect('Content-Type', /json/)
                                        .expect(200)
                                        .end(function (err, res) {
                                            if (err && res.body) console.error(res.body);
                                            if (err) console.error(err);
                                            should.not.exist(err);
                                            delete res.body.etdhprofileid;
                                            delete res.body.sites;
                                            delete res.body.groups;
                                            delete res.body.nodes;
                                            res.body.should.eql(data);

                                            var delete_req = request.delete(uri + "/" + etdhpid);
                                            agent.attachCookies(delete_req);
                                            delete_req.set('Accept', 'application/json')
                                                .set('X-CSRF-Token', csrfToken)
                                                //.expect('Content-Type', /json/)
                                                .expect(204)
                                                .end(function (err, res) {
                                                    if (err && res.body) console.error(res.body);
                                                    if (err) console.error(err);
                                                    should.not.exist(err);

                                                    var get_all_req = request.get(uri);
                                                    agent.attachCookies(get_all_req);
                                                    get_all_req.set('Accept', 'application/json')
                                                        .expect('Content-Type', /json/)
                                                        .expect(200)
                                                        .end(function (err, res) {
                                                            should.not.exist(err);
                                                            res.body.should.eql([]);
                                                            done();
                                                        });
                                                });
                                        });
                                });
                        });
                });
        });

        it("should create node dhtestnode (if not present)", function (done) {
            var nodeinfo = {
                nodeid: "dhtestnode",
                latitude: "1",
                longitude: "2",
                "model": "unode-v2"
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes');
            agent.attachCookies(req);
            req.send(nodeinfo)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    if (res.status != 400) {
                        should.not.exist(err);
                        nodeinfo.time_zone = 'America/Los_Angeles';
                        res.body.should.eql(nodeinfo);
                        done();
                    } else {
                        var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/dhtestnode/assign');
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
                    }
                });
        });

        it("should set daylight harvesting parameters for site", function (done) {
            var data = {
                "name": "Test DH Profile",
                "highLux": 100,
                "highDriver": 0,
                "lowLux": 50,
                "lowDriver": 20,
                "minLux": 10,
                "minDriver": 80,
                "fastPoll": 30,
                "slowPoll": 600,
                "scheduled": [{
                    "beginTime": "sunrise-90",
                    "endTime": "sunset+90"
                },
                {
                    "beginTime": "20:00:00",
                    "endTime": "23:00:00"
                }
                ]
            };

            var req = request.post(version + '/customers/uberorg/sites/ubersite/daylightharvesting');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    if (err && res.body) console.error(res.body);
                    if (err) console.error(err);
                    should.not.exist(err);
                    var result = res.body;
                    var etdhprofileid = result.etdhprofileid;
                    delete result.etdhprofileid;
                    result.should.eql(data);

                    req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/activate/dhtestnode');
                    agent.attachCookies(req);
                    req.send()
                        .set('Content-Type', 'application/json')
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(204)
                        .end(function (err, res) {
                            should.not.exist(err);
                            res.body.should.eql({});

                            // Should fail to apply non existing dh to site
                            req = request.post(version + '/customers/uberorg/sites/ubersite/daylightharvesting/' + 'ImN0r3a1' + '/site');
                            agent.attachCookies(req);
                            req.send('')
                                .set('Content-Type', 'application/json')
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(404)
                                .end(function (err, res) {
                                    if (err && res.body) console.error(res.body);
                                    if (err) console.error(err);
                                    should.not.exist(err);

                                    req = request.post(version + '/customers/uberorg/sites/ubersite/daylightharvesting/' + etdhprofileid + '/site');
                                    agent.attachCookies(req);
                                    req.send('')
                                        .set('Content-Type', 'application/json')
                                        .set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect('Content-Type', /json/)
                                        .expect(200)
                                        .end(function (err, res) {
                                            if (err && res.body) console.error(res.body);
                                            if (err) console.error(err);
                                            should.not.exist(err);
                                            res.body.sites[0].groupid.should.eql("ubersitelightinggroup");
                                            done();
                                        });
                                });

                        });
                });
        });

        it("should set daylight harvesting parameters for group", function (done) {
            var data = {
                "name": "Test DH Profile",
                "highLux": 100,
                "highDriver": 0,
                "lowLux": 50,
                "lowDriver": 20,
                "minLux": 10,
                "minDriver": 80,
                "fastPoll": 30,
                "slowPoll": 600,
                "scheduled": [{
                    "beginTime": "sunrise-90",
                    "endTime": "sunset+90"
                },
                {
                    "beginTime": "20:00:00",
                    "endTime": "23:00:00"
                }
                ]
            };

            var req = request.post(version + '/customers/uberorg/sites/ubersite/daylightharvesting');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    if (err && res.body) console.error(res.body);
                    if (err) console.error(err);
                    should.not.exist(err);
                    var result = res.body;
                    var etdhprofileid = result.etdhprofileid;
                    delete result.etdhprofileid;
                    result.should.eql(data);

                    data = {
                        nodeList: ["dhtestnode"],
                        name: "My group",
                        type: "lighting",
                        description: "My test group"
                    };
                    req = request.post(version + '/customers/uberorg/sites/ubersite/groups');
                    agent.attachCookies(req);
                    req.send(data)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);
                            var result = res.body;
                            var grpid = result.groupid;
                            delete result.groupid;
                            delete result.pdprofiles;
                            delete result.dhprofiles;
                            delete result.schedules;
                            result.should.eql(data);

                            // Should fail to apply non existing dh to group
                            req = request.post(version + '/customers/uberorg/sites/ubersite/daylightharvesting/' + etdhprofileid + '/groups/' + 'ImN0r3a1');
                            agent.attachCookies(req);
                            req.send('')
                                .set('Content-Type', 'application/json')
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(404)
                                .end(function (err, res) {
                                    if (err && res.body) console.error(res.body);
                                    if (err) console.error(err);
                                    should.not.exist(err);

                                    req = request.post(version + '/customers/uberorg/sites/ubersite/daylightharvesting/' + etdhprofileid + '/groups/' + grpid);
                                    agent.attachCookies(req);
                                    req.send('')
                                        .set('Content-Type', 'application/json')
                                        .set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect('Content-Type', /json/)
                                        .expect(200)
                                        .end(function (err, res) {
                                            if (err && res.body) console.error(res.body);
                                            if (err) console.error(err);
                                            should.not.exist(err);
                                            res.body.groups[0].groupid.should.eql(grpid);
                                            done();
                                        });

                                });

                        });
                });
        });

        it("should allow daylight harvesting trigger nodes to be manipulated", function (done) {
            var data = {
                "name": "Test DH Profile",
                "highLux": 100,
                "highDriver": 0,
                "lowLux": 50,
                "lowDriver": 20,
                "minLux": 10,
                "minDriver": 80,
                "fastPoll": 30,
                "slowPoll": 600,
                "scheduled": [{
                    "beginTime": "sunrise-90",
                    "endTime": "sunset+90"
                },
                {
                    "beginTime": "20:00:00",
                    "endTime": "23:00:00"
                }
                ]
            };

            var req = request.post(version + '/customers/uberorg/sites/ubersite/daylightharvesting');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    var etdhprofileid = result.etdhprofileid;
                    delete result.etdhprofileid;
                    result.should.eql(data);

                    data = {
                        nodeList: ["dhtestnode"],
                        name: "My group",
                        type: "lighting",
                        description: "My test group"
                    };

                    req = request.post(version + '/customers/uberorg/sites/ubersite/groups');
                    agent.attachCookies(req);
                    req.send(data)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);
                            var result = res.body;
                            var grpid = result.groupid;
                            delete result.groupid;
                            delete result.pdprofiles;
                            delete result.etdhprofiles;
                            delete result.dhprofiles;
                            delete result.schedules;
                            result.should.eql(data);

                            // Show no ETDHTriggers for ETDHProfile with no associated nodes.
                            req = request.get(version + '/customers/uberorg/sites/ubersite/daylightharvesting/' + etdhprofileid + '/triggers');
                            agent.attachCookies(req);
                            req.expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    res.body.should.eql([]);

                                    // Show failure to delete node not attached to profile
                                    req = request.delete(version + '/customers/uberorg/sites/ubersite/daylightharvesting/' + etdhprofileid + '/triggers/dhtestnode');
                                    agent.attachCookies(req);
                                    req.set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect('Content-Type', /json/)
                                        .expect(400)
                                        .end(function (err, res) {
                                            if (err && res && res.body) console.error(res.body);
                                            if (err) console.error(err);
                                            should.not.exist(err);
                                            res.body.should.eql({
                                                error: true,
                                                message: 'One or more nodes do not belong to the specified etdhprofile.',
                                                status: 400
                                            });

                                            req = request.post(version + '/customers/uberorg/sites/ubersite/daylightharvesting/' + etdhprofileid + '/groups/' + grpid);
                                            agent.attachCookies(req);
                                            req.send('')
                                                .set('Content-Type', 'application/json')
                                                .set('Accept', 'application/json')
                                                .set('X-CSRF-Token', csrfToken)
                                                .expect('Content-Type', /json/)
                                                .expect(200)
                                                .end(function (err, res) {
                                                    should.not.exist(err);
                                                    res.body.groups[0].groupid.should.eql(grpid);

                                                    // Show some ETDHTrigger for ETDHProfile now it's been assigned.
                                                    req = request.get(version + '/customers/uberorg/sites/ubersite/daylightharvesting/' + etdhprofileid + '/triggers');
                                                    agent.attachCookies(req);
                                                    req.expect('Content-Type', /json/)
                                                        .expect(200)
                                                        .end(function (err, res) {
                                                            should.not.exist(err);
                                                            res.body.should.eql([{
                                                                nodeid: 'dhtestnode',
                                                                latitude: '1',
                                                                longitude: '2'
                                                            }]);

                                                            // Remove ETDHTrigger from node and verify pass after ETDHProfile is attached
                                                            req = request.delete(version + '/customers/uberorg/sites/ubersite/daylightharvesting/' + etdhprofileid + '/triggers/dhtestnode');
                                                            agent.attachCookies(req);
                                                            req.set('Accept', 'application/json')
                                                                .set('X-CSRF-Token', csrfToken)
                                                                //.expect('Content-Type', /json/)
                                                                .expect(204)
                                                                .end(function (err, res) {
                                                                    should.not.exist(err);
                                                                    res.body.should.eql({});

                                                                    req = request.get(version + '/customers/uberorg/sites/ubersite/daylightharvesting/' + etdhprofileid + '/triggers');
                                                                    agent.attachCookies(req);
                                                                    req.expect('Content-Type', /json/)
                                                                        .expect(200)
                                                                        .end(function (err, res) {
                                                                            should.not.exist(err);
                                                                            res.body.should.eql([]);
                                                                            done();
                                                                        });
                                                                });
                                                        });
                                                });
                                        });
                                });
                        });
                });
        });
    });

    describe("GET /customers/{orgid}/sites/{siteid}/proximitydimming", function () {

        it("should create a Proximity Dimming profile using credentials", function (done) {
            var data = {
                name: "Test PD Profile",
                description: "This is a test proximity dimming profile",
                minLevel: 25, // Minimum driver level out of 100
                maxLevel: 75, // Maximum driver level out of 100
                beginTime: "00:00:00", // Daily time to turn ON/OFF (HH:MM:SS)  Single value for always.  Two values for ON/OFF.
                endTime: "23:59:59",
                mode: "radius",
                radius: 10, // Radius of detection around the node (meters?)
                detection_duration: 30 // Time to wait until next detection check (secs).
            };

            var req = request.post(version + '/customers/uberorg/sites/ubersite/proximitydimming');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    delete result.pdprofileid;
                    result.should.eql(data);
                    done();
                });
        });

        it("should get all proximity dimming profiles using credentials", function (done) {
            var data = {
                name: "Test PD Profile",
                description: "This is a test proximity dimming profile",
                minLevel: 25, // Minimum driver level out of 100
                maxLevel: 75, // Maximum driver level out of 100
                beginTime: "00:00:00", // Daily time to turn ON/OFF (HH:MM:SS)
                endTime: "23:59:59",
                mode: "radius",
                radius: 10, // Radius of detection around the node (meters?)
                detection_duration: 30 // Time to wait until next detection check (secs).
            };

            var uri = version + "/customers/uberorg/sites/ubersite/proximitydimming";
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.should.not.eql([]);

                    var pdpid = res.body[0].pdprofileid;
                    req = request.get(uri + "/" + pdpid);
                    agent.attachCookies(req);
                    req.set('Accept', 'application/json')
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            if (err) console.error(err);
                            should.not.exist(err);
                            data.pdprofileid = res.body.pdprofileid;
                            data.sites = res.body.sites;
                            data.groups = res.body.groups;
                            data.nodes = res.body.nodes;
                            res.body.should.eql(data);

                            var update_req = request.post(uri + "/" + pdpid);
                            agent.attachCookies(update_req);
                            data.detection_duration = 31;
                            update_req.send(data)
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    res.body.should.eql(data);

                                    var get_req = request.get(uri + "/" + pdpid);
                                    agent.attachCookies(get_req);
                                    get_req.set('Accept', 'application/json')
                                        .expect('Content-Type', /json/)
                                        .expect(200)
                                        .end(function (err, res) {
                                            should.not.exist(err);
                                            res.body.should.eql(data);

                                            var delete_req = request.delete(uri + "/" + pdpid);
                                            agent.attachCookies(delete_req);
                                            delete_req.set('Accept', 'application/json')
                                                .set('X-CSRF-Token', csrfToken)
                                                //.expect('Content-Type', /json/)
                                                .expect(204)
                                                .end(function (err, res) {
                                                    should.not.exist(err);

                                                    var get_all_req = request.get(uri);
                                                    agent.attachCookies(get_all_req);
                                                    get_all_req.set('Accept', 'application/json')
                                                        .expect('Content-Type', /json/)
                                                        .expect(200)
                                                        .end(function (err, res) {
                                                            should.not.exist(err);
                                                            res.body.should.eql([]);
                                                            done();
                                                        });
                                                });
                                        });
                                });
                        });
                });
        });

        it("should create node dhtestnode (if not present)", function (done) {
            var nodeinfo = {
                nodeid: "pdtestnode",
                latitude: "1",
                longitude: "2",
                "model": "unode-v2"
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes');
            agent.attachCookies(req);
            req.send(nodeinfo)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    if (res.status != 400) {
                        should.not.exist(err);
                        nodeinfo.time_zone = 'America/Los_Angeles';
                        res.body.should.eql(nodeinfo);
                        done();
                    } else {
                        var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/pdtestnode/assign');
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
                    }
                });
        });

        it("should set proximity dimming parameters for site", function (done) {
            var data = {
                name: "Test PD Profile",
                minLevel: 25, // Minimum driver level out of 100
                maxLevel: 75, // Maximum driver level out of 100
                beginTime: "00:00:00", // Daily time to turn ON/OFF (HH:MM:SS)  Single value for always.  Two values for ON/OFF.
                endTime: "23:59:59",
                mode: "radius",
                radius: 10, // Radius of detection around the node (meters?)
                detection_duration: 31 // Time to wait until next detection check (secs).
            };

            var req = request.post(version + '/customers/uberorg/sites/ubersite/proximitydimming');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    var pdprofileid = result.pdprofileid;
                    delete result.pdprofileid;
                    result.should.eql(data);

                    req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/activate/pdtestnode');
                    agent.attachCookies(req);
                    req.send()
                        .set('Content-Type', 'application/json')
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(204)
                        .end(function (err, res) {
                            should.not.exist(err);
                            res.body.should.eql({});

                            // Should fail with non existing PD
                            req = request.post(version + '/customers/uberorg/sites/ubersite/proximitydimming/' + 'NPD' + '/site');
                            agent.attachCookies(req);
                            req.send('')
                                .set('Content-Type', 'application/json')
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(404)
                                .end(function (err, res) {
                                    should.not.exist(err);

                                    req = request.post(version + '/customers/uberorg/sites/ubersite/proximitydimming/' + pdprofileid + '/site');
                                    agent.attachCookies(req);
                                    req.send('')
                                        .set('Content-Type', 'application/json')
                                        .set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect('Content-Type', /json/)
                                        .expect(400)
                                        .end(function (err, res) {
                                            if (err && res && res.body) console.error(res.body);
                                            if (err) console.error(err);
                                            should.not.exist(err);
                                            res.body.should.eql({
                                                error: true,
                                                message: 'Updating a site lighting group with a proximity dimming profile is not supported.',
                                                status: 400
                                            });
                                            done();
                                        });
                                });

                        });
                });
        });

        it("should set proximity dimming parameters for group", function (done) {
            var data = {
                name: "Test PD Profile",
                minLevel: 25, // Minimum driver level out of 100
                maxLevel: 75, // Maximum driver level out of 100
                beginTime: "00:00:00", // Daily time to turn ON/OFF (HH:MM:SS)  Single value for always.  Two values for ON/OFF.
                endTime: "23:59:59",
                mode: "radius",
                radius: 10, // Radius of detection around the node (meters?)
                detection_duration: 31 // Time to wait until next detection check (secs).
            };

            var req = request.post(version + '/customers/uberorg/sites/ubersite/proximitydimming');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    var pdprofileid = result.pdprofileid;
                    delete result.pdprofileid;
                    result.should.eql(data);

                    var egdata = {
                        nodeList: [],
                        name: "My empty group",
                        type: "lighting",
                        description: "My test empty groupgroup"
                    };
                    req = request.post(version + '/customers/uberorg/sites/ubersite/groups');
                    agent.attachCookies(req);
                    req.send(egdata)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);
                            var result = res.body;
                            var egrpid = result.groupid;

                            var gdata = {
                                nodeList: ["pdtestnode"],
                                name: "My group",
                                type: "lighting",
                                description: "My test group"
                            };
                            req = request.post(version + '/customers/uberorg/sites/ubersite/groups');
                            agent.attachCookies(req);
                            req.send(gdata)
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    var result = res.body;
                                    var grpid = result.groupid;
                                    delete result.groupid;
                                    delete result.pdprofiles;
                                    delete result.dhprofiles;
                                    delete result.schedules;
                                    result.should.eql(gdata);

                                    req = request.post(version + '/customers/uberorg/sites/ubersite/proximitydimming/' + pdprofileid + '/groups/' + egrpid);
                                    agent.attachCookies(req);
                                    req.send('')
                                        .set('Content-Type', 'application/json')
                                        .set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect('Content-Type', /json/)
                                        .expect(200)
                                        .end(function (err, res) {
                                            should.not.exist(err);
                                            //res.body.groups[0].groupid.should.eql(grpid);

                                            req = request.post(version + '/customers/uberorg/sites/ubersite/proximitydimming/' + pdprofileid + '/groups/' + grpid);
                                            agent.attachCookies(req);
                                            req.send('')
                                                .set('Content-Type', 'application/json')
                                                .set('Accept', 'application/json')
                                                .set('X-CSRF-Token', csrfToken)
                                                .expect('Content-Type', /json/)
                                                .expect(200)
                                                .end(function (err, res) {
                                                    should.not.exist(err);
                                                    res.body.groups[0].groupid.should.eql(grpid);
                                                    //done();
                                                    //Remove node from group
                                                    req = request.post(version + '/customers/uberorg/sites/ubersite/groups/' + grpid + '/remove/pdtestnode');
                                                    agent.attachCookies(req);
                                                    req.send('')
                                                        .set('Content-Type', 'application/json')
                                                        .set('Accept', 'application/json')
                                                        .set('X-CSRF-Token', csrfToken)
                                                        //.expect('Content-Type', /json/)
                                                        .expect(204)
                                                        .end(function (err, res) {
                                                            should.not.exist(err);
                                                            done();
                                                        });
                                                });
                                        });

                                });
                        });
                });
        });

        it("should be able to create a PDProfile with no radius", function (done) {
            var data = {
                name: "Test PD Profile",
                minLevel: 25, // Minimum driver level out of 100
                maxLevel: 75, // Maximum driver level out of 100
                beginTime: "00:00:00", // Daily time to turn ON/OFF (HH:MM:SS)  Single value for always.  Two values for ON/OFF.
                endTime: "23:59:59",
                mode: "no-radius",
                detection_duration: 31 // Time to wait until next detection check (secs).
            };

            var uri = version + '/customers/uberorg/sites/ubersite/proximitydimming';
            var req = request.post(uri);
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var pdpid = res.body.pdprofileid;
                    delete res.body.pdprofileid;
                    res.body.should.eql(data);

                    var delete_req = request.delete(uri + "/" + pdpid);
                    agent.attachCookies(delete_req);
                    delete_req.set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        //.expect('Content-Type', /json/)
                        .expect(204)
                        .end(function (err, res) {
                            should.not.exist(err);

                            done();
                        });
                });
        });

        it("should fail to access non existing schedule with login credentials", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/schedules/catchmeifyoucan');
            agent.attachCookies(req);
            req //.expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });

        });
    });



    let lssConsumer = null;
    describe("Schedule updates should be pushed to LSS for cnext nodes", function () {

        xit("should send schedule to lss on assigning newly created cnext node to site", function (done) {
            var nodeinfo = {
                nodeid: "mochacnextnode44",
                latitude: "1",
                longitude: "2",
                model: "cnext"
            };

            let isChecked = false;
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes');
            agent.attachCookies(req);
            req.send(nodeinfo)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    if (res.status != 400) {
                        should.not.exist(err);
                        nodeinfo.time_zone = 'America/Los_Angeles';
                        res.body.should.eql(nodeinfo);
                        console.log('created node')
                        compareConsumedEvent('ms.request.schedule', 'ubersite', function () {
                            if (!isChecked) {
                                isChecked = true;
                                done();
                            }

                        });

                    } else {
                        var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/mochacnextnode44/assign');
                        agent.attachCookies(req);
                        req
                            .set('Content-Type', 'application/json')
                            .set('Accept', 'application/json')
                            .set('X-CSRF-Token', csrfToken)
                            .expect('Content-Type', /json/)
                            .expect(200)
                            .end(function (err, res) {
                                should.not.exist(err);
                                console.log('assigned node')
                                compareConsumedEvent('ms.request.schedule', 'ubersite', function () {
                                    if (!isChecked) {
                                        isChecked = true;
                                        done();
                                    }
                                });
                            });
                    }
                });
        });

        xit("should send schedule to sts on assigning newly created cnext node node to site", function (done) {

            var nodeinfo = {
                nodeid: "mochacnextnode55",
                latitude: "1",
                longitude: "2",
                model: "cnext"
            };
            let isChecked = false;
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes');
            agent.attachCookies(req);
            req.send(nodeinfo)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    if (res.status != 400) {
                        should.not.exist(err);
                        nodeinfo.time_zone = 'America/Los_Angeles';
                        res.body.should.eql(nodeinfo);
                        compareConsumedEvent('schedule.command', 'LightingScheduledEvent', function () {
                            if (!isChecked) {
                                isChecked = true;
                                done();
                            }
                        });


                    } else {
                        var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/mochacnextnode55/assign');
                        agent.attachCookies(req);
                        req
                            .set('Content-Type', 'application/json')
                            .set('Accept', 'application/json')
                            .set('X-CSRF-Token', csrfToken)
                            .expect('Content-Type', /json/)
                            .expect(200)
                            .end(function (err, res) {
                                should.not.exist(err);

                                compareConsumedEvent('schedule.command', 'LightingScheduledEvent', function () {
                                    if (!isChecked) {
                                        isChecked = true;
                                        done();
                                    }
                                });
                            });
                    }
                });
        });
    });

    describe("Clean up after", function () {

        it("should clean up node after", function (done) {
            var nodeids = ['N0de2835chdld', 'dhtestnode', 'pdtestnode'];
//            var nodeids = ['N0de2835chdld', 'dhtestnode', 'pdtestnode', 'mochacnextnode44', 'mochacnextnode55'];
            var nodeid = null;
            var dirty = nodeids.length;
            while (nodeid = nodeids.shift()) {
                var uri = version + '/customers/uberorg/sites/ubersite/nodes';
                if (nodeid) {
                    console.log('Deleted Node:', nodeid);
                    var delete_req = request.delete(uri + "/" + nodeid);
                    agent.attachCookies(delete_req);
                    delete_req
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        //.expect(204)
                        .end(function (err, res) {
                            //should.not.exist(err);

                            if (!(--dirty))
                                done();
                        });
                }
            }
        });

        it("should clean up groups", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/groups";
            var getReq = request.get(uri);
            agent.attachCookies(getReq);
            getReq.set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var result = res.body;
                    if (result.length > 0) {
                        var groupIds = [];
                        //result.forEach(e => groupIds.push(e.groupid));
                        result.forEach(function (group) {
                            if (!group.name.includes('Site Lighting Group')) {
                                groupIds.push(group.groupid);
                            }
                        });
                        var groupId = null;
                        var dirty = groupIds.length;
                        while (groupId = groupIds.shift()) {
                            console.log('Deleted Group:', groupId);
                            var delete_req = request.delete(uri + "/" + groupId);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                //.expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    if (!(--dirty))
                                        done();
                                });
                        }
                    } else done();
                });
        });

        it("should clean up schedule", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/schedules";
            var delete_req = request.delete(uri + "/" + scheduleID);
            agent.attachCookies(delete_req);
            delete_req.set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    console.log('Deleted Schedule:', scheduleID);
                    done();
                });
        });

        it("should clean up daylight harvestings", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/daylightharvesting";
            var getReq = request.get(uri);
            agent.attachCookies(getReq);
            getReq.set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    if (result.length > 0) {
                        var dhIds = [];
                        result.forEach(e => dhIds.push(e.etdhprofileid));
                        var dhId = null;
                        var dirty = dhIds.length;
                        while (dhId = dhIds.shift()) {
                            console.log('Deleted Daylight Daylight:', dhId);
                            var delete_req = request.delete(uri + "/" + dhId);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect(204)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    if (!(--dirty))
                                        done();
                                });
                        }
                    } else done();
                });
        });

        it("should clean up proximity dimmings", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/proximitydimming";
            var getReq = request.get(uri);
            agent.attachCookies(getReq);
            getReq.set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var result = res.body;
                    if (result.length > 0) {
                        var pdIds = [];
                        result.forEach(e => pdIds.push(e.pdprofileid));
                        var pdId = null;
                        var dirty = pdIds.length;
                        while (pdId = pdIds.shift()) {
                            console.log('Deleted Proximity Dimming:', pdId);
                            var delete_req = request.delete(uri + "/" + pdId);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect(204)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    if (!(--dirty))
                                        done();
                                });
                        }
                    } else done();
                });
        });

    });
});