/**
 * Created by brefsdal on 12/16/15.
 */
"use strict";
var server_url = process.env.stack_url;
var should = require('should');
var request = require('supertest')(server_url);
var superagent = require('superagent');
var agent = superagent.agent();
var version = '/v3.0';
var uuid = require('uuid');

var mqtt = require('mqtt');
var configManager = require('kea-config');
configManager.setup('./config/');

const helpers = require('./../../utils/helpers');
let csrfToken = null;

/*
 Datadealer needs to be running for these tests to be successful
 % lein with-profile testjs run all

 Run suite of tests with
 % cd Farallones/api_service/ui_api
 % mocha --recursive
 */

var testuser_id = null,
    testuser_email = 'test_user1@sensity.com',
    testuser_phone = '+14083421182',
    mqttClient = null,
    simFailNotificationId1 = null,
    simFailNotificationId2 = null,
    disconnectNotificationId1 = null,
    disconnectNotificationId2 = null,
    //notificationMsg = "Got alert {{alert.name}} for node {{alert.nodeid}} with alert id: {{alert.alertid}}, org: {{alert.orgid}}, site: {{alert.siteid}} with message: {{alert.msg}} created {{alert.created}} updated {{alert.updated}}",
    notificationMsg = 'Got alert {{alert.name}} on {{alert.updated}} with message: {{alert.msg}}',
    coreNodeAlertId = null,
    videoNodeAlertId = null,
    coreNodeId = 'alertscorenodeid0012',
    videoNodeId = 'alertsvideonodeid0012',
    additionalEmails = ["test_additionalemail@sensity.com"];

describe('Notifications', function () {
    describe('hold_off_emails', function () {
        describe('Send email based on holdoff and resend interval', function () {

            it("should create mqtt client", function (done) {
                var subscription = {
                    uri: configManager.get('sse').url,
                    topics: ['/test/confirmation']
                }

                mqttClient = mqtt.connect(subscription.uri);

                /**
                 * On connect subscribe to all topics
                 */
                mqttClient.on('connect', function () {
                    console.log('Connected to %s', subscription.uri);
                    for (var j in subscription.topics) {
                        mqttClient.subscribe(subscription.topics[j]);
                        console.log('Subscribed to %s', subscription.topics[j]);
                    }
                });

                mqttClient.on('message', function (topic, message) {
                    try {
                        // message is Buffer
                        var response = JSON.parse(message.toString());
                        console.log('New message', topic, response);
                    } catch (e) {
                        console.log('Error parsing received message: %s', e.message, e);
                    }
                });
                done();
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


            it("should create a user using credentials", function (done) {
                var data = {
                    name: "Test User",
                    email: testuser_email,
                    title: "User of Test",
                    phone: testuser_phone,
                    roles: "customer_admin",
                    sites: "Test Site"
                };
                var req = request.post(version + '/customers/uberorg/users');
                agent.attachCookies(req);
                req.send(data)
                    .set('Accept', 'application/json')
                    .set('Content-Type', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        var result = res.body;
                        testuser_id = result.userid;
                        delete result.userid;
                        result.should.eql(data);
                        done();
                    });
            });

            it("should create a notification with hold off 0 sec and resend interval 0 sec", function (done) {
                var data = {
                    window: "* * *",
                    emailUsersList: [testuser_id],
                    smsUsersList: [],
                    additionalEmails: additionalEmails,
                    hold_off: 0,
                    resend_interval: 0,
                    active: true,
                    msg: notificationMsg,
                    name: "DeviceAlarm",
                    notificationtype: ["SimFail"],
                    severity: ["Critical", "Major", "Minor"]
                };
                var req = request.post(version + '/customers/uberorg/sites/ubersite/notifications');
                agent.attachCookies(req);
                req.send(data)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        var result = res.body;
                        simFailNotificationId1 = result.notificationid;
                        console.log('SimFail Notification 1:', simFailNotificationId1);
                        delete result.notificationid;
                        delete result.updated;
                        delete result.created;
                        delete result.orgid;
                        delete result.siteid;
                        result.scope.should.eql('ubersite');
                        delete result.scope;
                        result.should.eql(data);
                        done();
                    });
            });

            it("should create a notification with hold off 3 sec and resend interval 0 sec", function (done) {
                var data = {
                    window: "* * *",
                    emailUsersList: [testuser_id],
                    smsUsersList: [],
                    additionalEmails: additionalEmails,
                    hold_off: 3,
                    resend_interval: 0,
                    active: true,
                    msg: notificationMsg,
                    name: "DeviceAlarm",
                    notificationtype: ["SimFail"],
                    severity: ["Critical", "Major", "Minor"]
                };
                var req = request.post(version + '/customers/uberorg/sites/ubersite/notifications');
                agent.attachCookies(req);
                req.send(data)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        var result = res.body;                        
                        simFailNotificationId2 = result.notificationid;
                        console.log('SimFail Notification 2:', simFailNotificationId2);
                        delete result.notificationid;
                        delete result.updated;
                        delete result.created;
                        delete result.orgid;
                        delete result.siteid;
                        result.scope.should.eql('ubersite');
                        delete result.scope;
                        result.should.eql(data);
                        done();
                    });
            });

            it("should create a notification with hold off 0 sec and resend interval 3 sec", function (done) {
                var data = {
                    window: "* * *",
                    emailUsersList: [testuser_id],
                    smsUsersList: [],
                    additionalEmails: additionalEmails,
                    hold_off: 0,
                    resend_interval: 3,
                    active: true,
                    msg: notificationMsg,
                    name: "DeviceAlarm",
                    notificationtype: ["Disconnect"],
                    severity: ["Critical", "Major", "Minor"]
                };
                var req = request.post(version + '/customers/uberorg/sites/ubersite/notifications');
                agent.attachCookies(req);
                req.send(data)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        var result = res.body;
                        disconnectNotificationId1 = result.notificationid;
                        console.log('Disconnect Notification 1:', disconnectNotificationId1);
                        delete result.notificationid;
                        delete result.updated;
                        delete result.created;
                        delete result.orgid;
                        delete result.siteid;
                        result.scope.should.eql('ubersite');
                        delete result.scope;
                        result.should.eql(data);
                        done();
                    });
            });

            it("should create a notification with hold off 3 sec and resend interval 3 sec", function (done) {
                var data = {
                    window: "* * *",
                    emailUsersList: [testuser_id],
                    smsUsersList: [],
                    additionalEmails: additionalEmails,
                    hold_off: 3,
                    resend_interval: 3,
                    active: true,
                    msg: notificationMsg,
                    name: "DeviceAlarm",
                    notificationtype: ["Disconnect"],
                    severity: ["Critical", "Major", "Minor"]
                };
                var req = request.post(version + '/customers/uberorg/sites/ubersite/notifications');
                agent.attachCookies(req);
                req.send(data)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        var result = res.body;
                        disconnectNotificationId2 = result.notificationid;
                        console.log('Disconnect Notification 2:', disconnectNotificationId2);
                        delete result.notificationid;
                        delete result.updated;
                        delete result.created;
                        delete result.orgid;
                        delete result.siteid;
                        result.scope.should.eql('ubersite');
                        delete result.scope;
                        result.should.eql(data);
                        done();
                    });
            });

            it("should create a core node (alertscorenodeid001) and video node (alertsvideonodeid001) using credentials", function (done) {
                var createNodereq = request.put(version + '/nodes');
                agent.attachCookies(createNodereq);
                var payload = { csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude\n" + coreNodeId + ",unode-v4,uberorg,ubersite,44.4,41.2\n" + videoNodeId + ",falcon-q,uberorg,ubersite,44.4,41.2" };
                createNodereq.send(payload)
                    //.expect('Content-Type', /json/)
                    .set('X-CSRF-Token', csrfToken)
                    .expect(200)
                    .end(function (err, res2) {
                        should.not.exist(err);
                        done();
                    });
            });

            it("should create a core node alert (SimFail) using credentials", function (done) {
                var data = {
                    name: "DeviceAlarm",
                    nodeid: coreNodeId,
                    orgid: "uberorg",
                    siteid: "ubersite",
                    type: "SimFail",
                    severity: "Major",
                    category: "Sensor",
                    msg: "Core Node SimFail message from Sensity"
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
                        coreNodeAlertId = result.alertid;

                        //console.log('Publishing to %s', '/test/confirmation', result);
                        mqttClient.publish('/test/alert', JSON.stringify(result));

                        delete result.alertid;
                        delete result.nodehw;
                        delete result.updated;
                        delete result.created;
                        delete result.active;
                        delete result.ufname;
                        delete result.description;
                        delete result.displaytopartner;
                        delete result.displaytocustomer;
                        result.should.eql(data);
                        done();
                    });
            });

            it("should create a video node alert (Disconnect) using credentials", function (done) {
                var data = {
                    name: "DeviceAlarm",
                    nodeid: videoNodeId,
                    orgid: "uberorg",
                    siteid: "ubersite",
                    type: "Disconnect",
                    severity: "Critical",
                    msg: "Video Node Disconnect message from Sensity",
                    category: "Network"
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
                        videoNodeAlertId = result.alertid;
                        delete result.active;
                        //console.log('Publishing to %s', '/test/confirmation', result);
                        mqttClient.publish('/test/alert', JSON.stringify(result));
                        delete result.alertid;
                        delete result.nodehw;
                        delete result.updated;
                        delete result.created;
                        delete result.active;
                        delete result.ufname;
                        delete result.description;
                        delete result.displaytopartner;
                        delete result.displaytocustomer;
                        result.should.eql(data);
                        done();
                    });
            });
        });

        // --- Cleanup ---
        describe('Cleanup Notifications, Alerts, Nodes and Users ', function () {

            it("should delete notifications after", function (done) {
                var notificationIds = [simFailNotificationId1, simFailNotificationId2, disconnectNotificationId1, disconnectNotificationId2];
                var notificationId = null;
                var dirty = notificationIds.length;
                while (notificationId = notificationIds.shift()) {
                    var uri = version + '/customers/uberorg/sites/ubersite/notifications';
                    var delete_req = request.delete(uri + "/" + notificationId);
                    agent.attachCookies(delete_req);
                    delete_req
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);

                            if (!(--dirty))
                                done();
                        });
                }

            });

            it("should delete core node alert and node after", function (done) {
                var delete_req = request.delete(version + "/customers/uberorg/sites/ubersite/alerts/" + coreNodeAlertId);
                agent.attachCookies(delete_req);
                delete_req.set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        var deleteNodereq = request.delete(version + '/customers/uberorg/sites/ubersite/nodes/' + coreNodeId);
                        agent.attachCookies(deleteNodereq);
                        deleteNodereq
                            .set('Content-Type', 'application/json')
                            .set('Accept', 'application/json')
                            .set('X-CSRF-Token', csrfToken)
                            .expect(204)
                            .end(function (err, res) {
                                // Core node deleted successfully
                                should.not.exist(err);
                                done();
                            });
                    });
            });

            it("should delete video node alert and node after", function (done) {
                var delete_req = request.delete(version + "/customers/uberorg/sites/ubersite/alerts/" + videoNodeAlertId);
                agent.attachCookies(delete_req);
                delete_req.set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        var deleteNodereq = request.delete(version + '/customers/uberorg/sites/ubersite/nodes/' + videoNodeId);
                        agent.attachCookies(deleteNodereq);
                        deleteNodereq
                            .set('Content-Type', 'application/json')
                            .set('Accept', 'application/json')
                            .set('X-CSRF-Token', csrfToken)
                            .expect(204)
                            .end(function (err, res) {
                                // Video node deleted successfully
                                should.not.exist(err);
                                done();
                            });
                    });
            });

            it("should delete user after", function (done) {
                var delete_req = request.delete(version + "/customers/uberorg/users/" + testuser_id);
                agent.attachCookies(delete_req);
                delete_req.set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        mqttClient.end();
                        done();
                    });
            });

        });
    });
});
