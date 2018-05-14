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

let notificationId = null;

describe('Notifications', function () {
    describe('GET /customers/{orgid}/sites/{siteid}/notifications', function () {
        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers/uberorg/sites/ubersite/notifications')
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
            var data = { email: "uberuser@sensity.com", password: "ubeR$23" };
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

        it("should not create a notification with incorrect params", function (done) {
            var data = {
                description: "test notification",
                emailUsersList: [],
                smsUsersList: [],
                additionalEmails: [],
                active: true,
                hold_off: -5,
                resend_interval: -25,
                scope: 'ubersite',
                msg: "Got alert {{alert.name}} on {{alert.updated}}",
                name: "DeviceAlarm"
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/notifications');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    console.log("Received message when sending negative hold_off and resend_interval:", res.body.message);
                    done();
                });
        });

        it("should create a notification using credentials", function (done) {
            var data = {
                description: "test notification",
                emailUsersList: [],
                smsUsersList: [],
                additionalEmails: [],
                active: true,
                severity: ['Major'],
                notificationtype: ['Disconnect'],
                orgid: "uberorg",
                siteid: "ubersite",
                msg: "Got alert {{alert.name}} on {{alert.updated}}",
                name: "DeviceAlarm"
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
                    notificationId = result.notificationid;
                    delete result.notificationid;
                    delete result.updated;
                    delete result.created;
                    delete result.window;
                    result.scope.should.eql('ubersite');
                    delete result.scope;
                    result.should.eql(data);
                    done();
                });
        });


        it("should get, update, and delete notification using credentials", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/notifications";
            var data = {
                description: "test notification",
                emailUsersList: [],
                smsUsersList: [],
                additionalEmails: [],
                scope: 'ubersite',
                active: true,
                severity: ['Major'],
                notificationtype: ['Disconnect'],
                orgid: "uberorg",
                siteid: "ubersite",
                msg: "Got alert {{alert.name}} on {{alert.updated}}",
                name: "DeviceAlarm"
            };
            var req = request.get(uri + '/' + notificationId);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    delete res.body.notificationid;
                    delete res.body.updated;
                    delete res.body.created;
                    delete res.window;
                    delete res.body.nodeid;
                    res.body.should.eql(data);
                    var updated = {
                        description: "updated_test_notification",
                        hold_off: 5,
                        resend_interval: 6,
                        emailUsersList: [],
                        smsUsersList: [],
                        additionalEmails: [],
                        active: false,
                        orgid: "uberorg",
                        siteid: "ubersite",
                        scope: 'ubersite',
                        msg: "Got alert {{alert.name}} on {{alert.updated}}",
                        name: "DeviceAlarm",
                        notificationtype: ["HWFail_ISL29023", "HWFail_MMA8451"],
                        severity: ["Major", "Minor"]
                    };
                    var req2 = request.post(uri + "/" + notificationId);
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
                            delete result.window;
                            delete result.nodeid;
                            result.should.eql(updated);
                            var delete_req = request.delete(uri + "/" + notificationId);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    var get_req = request.get(uri + "/" + notificationId);
                                    agent.attachCookies(get_req);
                                    get_req.set('Accept', 'application/json')
                                        .expect('Content-Type', /json/)
                                        .expect(404)
                                        .end(function (err, res) {
                                            should.not.exist(err);
                                            done();
                                        });
                                });
                        });
                });
        });

        it("should access notifications with login credentials", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/notifications');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });
    });
});