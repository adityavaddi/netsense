/**
 * Created by brefsdal on 12/16/15.
 */
"use strict";
var server_url = process.env.stack_url;// || require('../../../app.js');
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

var mqttClient = null,
    notification_id = null,
    coreNodeAlertId = null,
    videoNodeAlertId = null,
    coreNodeId = 'alertscorenodeid0011',
    videoNodeId = 'alertsvideonodeid0011',
    additionalEmails = ["test_additionalemail@sensity.com"];

describe('Notifications', function () {
    describe('deactivated_notification', function () {
        describe('Deactivate notification for scheduled alert', function () {
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
                    // TODO: Subscribe to alert topics
                    //var supported_alerts = notification_manager.getSupportedAlerts();
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

            it("should create a notification using credentials", function (done) {
                var data = {
                    window: "* * *",
                    emailUsersList: [],
                    smsUsersList: [],
                    additionalEmails: additionalEmails,
                    active: true,
                    msg: "Got alert {{alert.name}} on {{alert.updated}} with message: {{alert.msg}}. If you reveive it, test has failed",
                    hold_off: 2,
                    name: "DeviceAlarm",
                    notificationtype: ["HWFail_EEPROM"],
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
                        notification_id = result.notificationid;
                        console.log('Notification ID:', notification_id);
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

            it("should create a core node alert using credentials", function (done) {
                var data = {
                    //alertid: "alert1",
                    name: "DeviceAlarm",
                    nodeid: coreNodeId,
                    orgid: "uberorg",
                    siteid: "ubersite",
                    type: "HWFail_EEPROM",
                    severity: "Major",
                    category: "Sensor",
                    msg: "Core Node Up"
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

            it("should create a video node alert using credentials", function (done) {
                var data = {
                    name: "DeviceAlarm",
                    nodeid: videoNodeId,
                    orgid: "uberorg",
                    siteid: "ubersite",
                    type: "HWFail_EEPROM",
                    severity: "Major",
                    msg: "Video Node Up",
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
                        delete result.ufname;
                        delete result.description;
                        delete result.displaytopartner;
                        delete result.displaytocustomer;
                        result.should.eql(data);
                        done();
                    });
            });

            it("should deactivate notification", function (done) {
                var uri = version + "/customers/uberorg/sites/ubersite/notifications";

                var updated = {
                    window: "* * *",
                    //resend_interval: 25,
                    emailUsersList: [],
                    smsUsersList: [],
                    additionalEmails: additionalEmails,
                    active: false,
                    msg: "Got alert {{alert.name}} on {{alert.updated}} with message: {{alert.msg}}. If you reveive it, test has failed",
                    hold_off: 2,
                    name: "DeviceAlarm",
                    notificationtype: ["HWFail_EEPROM"],
                    severity: ["Critical", "Major", "Minor"]
                };
                var req2 = request.post(uri + "/" + notification_id);
                agent.attachCookies(req2);
                req2.send(updated)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res2) {
                        should.not.exist(err);

                        var result = res2.body;
                        delete result.notificationid;
                        delete result.updated;
                        delete result.created;
                        delete result.nodeid;
                        delete result.orgid;
                        delete result.siteid;
                        result.scope.should.eql('ubersite');
                        delete result.scope;
                        //delete result.severity;
                        result.should.eql(updated);

                        done();
                    });
            });

        });

        // --- Cleanup ---
        describe('Cleanup Notifications, Alerts, Nodes and Users ', function () {
            it("should delete notification after", function (done) {
                var delete_req = request.delete(version + "/customers/uberorg/sites/ubersite/notifications/" + notification_id);
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
        });
    });
});
