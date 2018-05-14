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
var nodeId = "uf_alarm_nodeid";
let mappingId = null;
let allMappingIds = [];
var configManager = require('kea-config');
configManager.setup('./config/');

const helpers = require('./../../utils/helpers');
let csrfToken = null;


/*
Required Components: Kafka, Cassandra, Interface Service, ACL Service, Alert Service

 Run suite of tests with
 % cd Farallones/api_tests/tests/api
 % mocha --recursive
 */


describe('UF_Alarms', function () {
    describe('GET /manage/alarms', function () {
        it('should fail without login credentials', function (done) {
            request
                .get(version + '/manage/alarms')
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
                    var req = request.get(version + '/manage/alarms');
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
                nodeId + ",falcon-q,uberorg,ubersite,44.4,41.2\n"
            };
            createNodeReq.send(payload)
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res2) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should not create user friendly alarm ", function (done) {
            var data = {
                nodemodels: ["falcon-q", "merlin", "vdkmaster"],
                description: "This is for Mocha Tests",
                displaytocustomer: true,
                displaytopartner: true
            };
            var req = request.post(version + '/manage/alarms');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    done();
                });
        });

        it("should create an user friendly alarm ", function (done) {
            var data = {
                alarmtype: "mocha_type",
                nodemodels: ["falcon-q", "merlin", "vdkmaster"],
                ufname: "Mocha UF Name",
                description: "This is for Mocha Tests",
                displaytocustomer: true,
                displaytopartner: true
            };
            var req = request.post(version + '/manage/alarms');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    mappingId = result.mappingid;
                    console.log('UF Alarm Mapping ID: ', mappingId)
                    delete result.mappingid;
                    delete result.created;
                    delete result.updated;
                    result.should.eql(data);
                    done();
                });
        });

        it("should get user friendly alarm", function (done) {
            var uri = version + "/manage/alarms/" + mappingId;
            var data = {
                alarmtype: "mocha_type",
                nodemodels: ["falcon-q", "merlin", "vdkmaster"],
                ufname: "Mocha UF Name",
                description: "This is for Mocha Tests",
                displaytocustomer: true,
                displaytopartner: true
            };
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var result = res.body;
                    delete result.mappingid;
                    delete result.created;
                    delete result.updated;
                    result.should.eql(data);
                    console.log('Fetched UF Alarm: ', mappingId);
                    done();
                });
        });

        it("should create alert and return uf name", function (done) {
            var data = {
                name: "DeviceAlarm",
                nodeid: nodeId,
                orgid: "uberorg",
                siteid: "ubersite",
                type: "mocha_type",
                category: "Network",
                severity: "Major",
                msg: "Up",
                ufname: "Mocha UF Name",
                description:"This is for Mocha Tests",
                displaytocustomer: true,
                displaytopartner: true
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
                    result.should.eql(data);
                    done();
                });
        });

        it("should get all, get and delete alert with uf name", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/alerts";
            var data = {
                name: "DeviceAlarm",
                nodeid: nodeId,
                orgid: "uberorg",
                siteid: "ubersite",
                type: "mocha_type",
                category: "Network",
                severity: "Major",
                msg: "Up",
                ufname: "Mocha UF Name",
                description:"This is for Mocha Tests",
                displaytocustomer: true,
                displaytopartner: true
            };
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var alertid = res.body[0].alertid;
                    console.log('UF Alarms AlertID: ', alertid)
                    delete res.body[0].alertid;
                    delete res.body[0].updated;
                    delete res.body[0].created;
                    delete res.body[0].active;
                    delete res.body[0].nodehw;
                    delete res.body[0].bssid;
                    delete res.body[0].sitename;
                    res.body[0].should.eql(data);
                    var delete_req = request.delete(uri + "/" + alertid);
                    agent.attachCookies(delete_req);
                    delete_req.set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            console.log('Deleted UF Alarms Alert: ', alertid);
                            var get_req = request.get(uri + "/" + alertid);
                            agent.attachCookies(get_req);
                            get_req.set('Accept', 'application/json')
                                .expect('Content-Type', /json/)
                                .expect(404)
                                .end(function (err, res) {
                                    done();
                                });
                        });
                });
        });

        it("should update user friendly alarm", function (done) {
            var uri = version + "/manage/alarms/" + mappingId;
            var updated_data = {
                alarmtype: "mocha_type",
                nodemodels: ["falcon-q", "merlin", "vdkmaster"],
                ufname: "Mocha UF Name Updated",
                description: "This is for Mocha Tests",
                displaytocustomer: true,
                displaytopartner: true
            };
            var update_req = request.put(uri);
            agent.attachCookies(update_req);
            update_req.send(updated_data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    delete result.mappingid;
                    delete result.created;
                    delete result.updated;
                    result.should.eql(updated_data);
                    console.log('Updated UF Alarm: ', mappingId);
                    done();
                });
        });

        it("should reset user friendly alarm", function (done) {
            var uri = version + "/manage/alarms/" + mappingId + "/reset";
            var alarm_type = "mocha_type"
            var updated_data = {
                alarmtype: alarm_type,
                nodemodels: [],
                ufname: alarm_type,
                displaytocustomer: false,
                displaytopartner: false
            };
            var reset_req = request.put(uri);
            agent.attachCookies(reset_req);
            reset_req.send(updated_data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    delete result.mappingid;
                    delete result.description;
                    delete result.created;
                    delete result.updated;
                    result.should.eql(updated_data);
                    console.log('Reset UF Alarm: ', mappingId);
                    done();
                });
        });

        it("should delete user friendly alarm", function (done) {
            var uri = version + "/manage/alarms/" + mappingId;
            var delete_req = request.delete(uri);
            agent.attachCookies(delete_req);
            delete_req.set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    console.log('Deleted UF Alarm: ', mappingId);
                    var get_req = request.get(uri);
                    agent.attachCookies(get_req);
                    get_req.set('Accept', 'application/json')
                        .expect('Content-Type', /json/)
                        .expect(404)
                        .end(function (err, res) {
                            done();
                        });
                });
        });

        it('should not create bulk uf alarms', function (done) {
            var createReq = request.put(version + '/manage/bulkalarms');
            agent.attachCookies(createReq);
            var payload = [{
                           "nodemodels": ["falcon-q", "merlin"],
                           "ufname": "Mocha UF Name1",
                           "description": "Mocha Description1",
                           "displaytocustomer": true,
                           "displaytopartner": false
                           },
                           {
                           "alarmtype": "mocha_type2",
                           "nodemodels": ["unode", "cnext"],
                           "description": "Mocha Description2",
                           "displaytocustomer": false,
                           "displaytopartner": true
                           }]
            createReq.send(payload)
                .set('X-CSRF-Token', csrfToken)
                .expect(400)
                .end(function (err, res) {
                    done();
                });
        });

        it('should create bulk uf alarms', function (done) {
            var createReq = request.put(version + '/manage/bulkalarms');
            agent.attachCookies(createReq);
            var payload = [{
                           "alarmtype": "mocha_type1",
                           "nodemodels": ["falcon-q", "merlin"],
                           "ufname": "Mocha UF Name1",
                           "description": "Mocha Description1",
                           "displaytocustomer": true,
                           "displaytopartner": false
                           },
                           {
                           "alarmtype": "mocha_type2",
                           "nodemodels": ["unode", "cnext"],
                           "ufname": "Mocha UF Name2",
                           "description": "Mocha Description2",
                           "displaytocustomer": false,
                           "displaytopartner": true
                           }]
            createReq.send(payload)
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    allMappingIds = result.map(item => item.ufalarm.mappingid);
                    console.log('Bulk UF Alarm MappingIds: ', allMappingIds);
                    done();
                });
        });

        it('should access all user friendly alarms with login credentials', function (done) {
            var req = request.get(version + '/manage/alarms');
            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.not.eql(0);
                    done();
                });
        });

        it('should get Notification Types for Sensity', function (done) {
            var req = request.get(version + '/notificationtypes');
            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.not.eql(0);
                    done();
                });
        });

        it('should delete user friendly alarms', function (done) {
            var mappingid = null;
            var dirty = allMappingIds.length;
            while (mappingid = allMappingIds.shift()) {
                var uri = version + '/manage/alarms';
                var delete_req = request.delete(uri + "/" + mappingid);
                agent.attachCookies(delete_req);
                delete_req
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        if (!(--dirty))
                            done();
                    });
            }
        });
    });

    describe("Clean up nodes", function (done) {
        it('should clean up nodes after', function (done) {
            var nodeids = [nodeId];
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
