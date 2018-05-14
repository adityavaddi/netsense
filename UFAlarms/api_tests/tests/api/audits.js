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

const helpers = require('./../../utils/helpers');
let csrfToken = null;

/*
 Datadealer needs to be running for these tests to be successful
 % lein with-profile testjs run all

 Run suite of tests with
 % cd Farallones/api_service/ui_api
 % mocha --recursive
 */

describe('Audits', function() {
    var autids_cnt = 0;

    describe('GET /customers/{orgid}/sites/{siteid}/audits/from/{datemin}/to/{datemax}/', function() {
        it('should fail without login credentials', function(done) {
            var fromDate = new Date(Date.now() - 15000);
            var toDate = new Date(Date.now());
            var from = fromDate.toISOString();
            var to = toDate.toISOString();
            request
                .get(version + '/customers/uberorg/sites/ubersite/audits/from/' + from + '/to/' + to)
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function(err, res) {
                    should.not.exist(err);
                    res.body.should.eql({
                        error: true,
                        message: 'Access denied',
                        status: 403
                    });
                    done();
                });
        });

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

        it("should query audits using credentials", function(done) {
            var fromDate = new Date(Date.now() - 15000);
            var toDate = new Date(Date.now());
            var from = fromDate.toISOString();
            var to = toDate.toISOString();
            var req = request.get(version + '/customers/uberorg/sites/ubersite/audits/from/' + from + '/to/' + to);
            agent.attachCookies(req);
            req.set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function(err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    autids_cnt = result.length;
                    done();
                });
        });

        it("should failt to query audits with incorect date", function(done) {
            var fromDate = new Date(Date.now() - 15000);
            var toDate = new Date(Date.now());
            var from = 'X' + fromDate.toISOString();
            var to = toDate.toISOString() + 'X';
            var req = request.get(version + '/customers/uberorg/sites/ubersite/audits/from/' + from + '/to/' + to);
            agent.attachCookies(req);
            req.set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function(err, res) {
                    should.not.exist(err);
                    var result = res.body;

                    done();
                });
        });


        it('should create auditnode using credentials', function(done) {
            var createNodereq = request.put(version + '/nodes');
            agent.attachCookies(createNodereq);
            var payload = {
                csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude\nauditnode,falcon-q,uberorg,ubersite,44.4,41.2"
            };
            createNodereq.send(payload)
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function(err, res2) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should assign a node using credentials', function(done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/auditnode/assign');
            agent.attachCookies(req);
            req
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function(err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        var orgid = null;
        var starttime = new Date(Date.now());
        var laptime = null;
        var stoptime = null;

        it("should query audits using credentials 2", function(done) {
            var fromDate = new Date(Date.now() - 15000);
            var toDate = new Date(Date.now());
            var from = fromDate.toISOString();
            var to = toDate.toISOString();
            laptime = to;
            var req = request.get(version + '/customers/uberorg/sites/ubersite/audits/from/' + from + '/to/' + to);
            agent.attachCookies(req);
            req.set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function(err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    result.length.should.eql(1 + autids_cnt);
                    autids_cnt = result.length;
                    done();
                });
        });

        it("should do some cleanup and log", function(done) {
            var deleteNodeReq = request.delete(version + '/customers/uberorg/sites/ubersite/nodes/auditnode');
            agent.attachCookies(deleteNodeReq);
            deleteNodeReq
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function(err, res) {
                    // auditnode deleted successfully
                    should.not.exist(err);
                    done();
                });
        });
        it("should query audits using credentials 3", function(done) {
            var fromDate = new Date(Date.now() - 15000);
            var toDate = new Date(Date.now());
            var from = fromDate.toISOString();
            var to = toDate.toISOString();
            starttime = fromDate
            stoptime = toDate;
            var req = request.get(version + '/customers/uberorg/sites/ubersite/audits/from/' + from + '/to/' + to);
            agent.attachCookies(req);
            req.set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function(err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    result.length.should.eql(1 + autids_cnt);
                    autids_cnt = result.length;
                    done();
                });
        });

        it("should query audits and check returned entries", function(done) {
            var fromDate = starttime;
            var toDate = stoptime;
            var from = fromDate.toISOString();
            var to = toDate.toISOString();
            var req = request.get(version + '/customers/uberorg/sites/ubersite/audits/from/' + from + '/to/' + to);
            console.log('req url', version + '/customers/uberorg/sites/ubersite/audits/from/' + from + '/to/' + to);
            agent.attachCookies(req);
            req.set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function(err, res) {
                    should.not.exist(err);
                    var result = res.body.slice(-2);
                    result.length.should.eql(2);
                    var activities = ['assignNode', 'deleteNode'];
                    for (var i = 0; i < result.length; i++) {
                        var entry = result[i];
                        should.exist(entry.when);
                        should.exist(entry.targetid);
                        should.exist(entry.message);
                        entry.siteid.should.eql('ubersite');
                        //entry.targettype.should.eql('alert');
                        entry.activity.toLowerCase().should.eql(activities[i].toLowerCase());
                    }
                    done();
                });
        });
    });
});