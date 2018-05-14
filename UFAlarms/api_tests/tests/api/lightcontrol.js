/**
 * Created by dignjatic on 12/18/15.
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

describe('LightControl', function () {
    describe('POST /customers/uberorg/sites/ubersite/lightcontrol/node/ubernode', function () {
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


        it("should create an empty node (N01334ltc) using credentials", function (done) {
            var data = {
                "nodeid": "N01334ltc",
                "model": "unode-v2"
            };
            var req = request.post(version + '/nodes/N01334ltc');
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
                    done();
                });
        });


        it('it should set light level for a single node', function (done) {

          var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/N01334ltc/assign');
          agent.attachCookies(req);
          req
              .set('Content-Type', 'application/json')
              .set('Accept', 'application/json')
              .set('X-CSRF-Token', csrfToken)
              .expect('Content-Type', /json/)
              .expect(200)
              .end(function (err, res) {
                  should.not.exist(err);

                var req = request.post(version + '/customers/uberorg/sites/ubersite/lightcontrol/node/N01334ltc');
                agent.attachCookies(req);
                var data = { level: 50, timeout: 30 };
                req.send(data)
                    .set('Content-Type', 'application/json')
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect(204)
                    .end(function (err, res) {
                        should.not.exist(err);
                        agent.saveCookies(res);
                        done();
                    });
            });
        });

        it('it should set light level for a site', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/lightcontrol');
            agent.attachCookies(req);
            var data = { level: 50, timeout: 30 };
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    done();
                });
        });

        it('it should set light level for a group', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/lightcontrol/groups/ubergroup');
            agent.attachCookies(req);
            var data = { level: 50, timeout: 30 };
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    done();
                });
        });


        it('it should clear for a single node', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/lightcontrol/node/N01334ltc');
            agent.attachCookies(req);
            var data = { level: 50, timeout: 30, clear: true };
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    done();
                });
        });

        it('it should clear for a site', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/lightcontrol');
            agent.attachCookies(req);
            var data = { level: 50, timeout: 30, clear: true };
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    done();
                });
        });

        it('it should clear for a group', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/lightcontrol/groups/ubergroup');
            agent.attachCookies(req);
            var data = { level: 50, timeout: 30, clear: true };
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    done();
                });
        });



        it("should create an empty node empty cnext(mochacnextnodeLFS) node using credentials", function (done) {
            var data = {
                "nodeid": "mochacnextnodeLFS",
                "model": "cnext"
            };
            var req = request.post(version + '/nodes/mochacnextnodeLFS');
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


                    done();
                });
        });


        it('it should set light level for a single cnext(mochacnextnodeLFS) node', function (done) {

          var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/mochacnextnodeLFS/assign');
          agent.attachCookies(req);
          req
              .set('Content-Type', 'application/json')
              .set('Accept', 'application/json')
              .set('X-CSRF-Token', csrfToken)
              .expect('Content-Type', /json/)
              .expect(200)
              .end(function (err, res) {
                  should.not.exist(err);

                var req = request.post(version + '/customers/uberorg/sites/ubersite/lightcontrol/node/mochacnextnodeLFS');
                agent.attachCookies(req);
                var data = { level: 50, timeout: 30 };
                req.send(data)
                    .set('Content-Type', 'application/json')
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect(204)
                    .end(function (err, res) {
                        should.not.exist(err);
                        agent.saveCookies(res);
                        done();
                    });
            });
        });


        it('it should set light level for a group with cnext node', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/lightcontrol/groups/ubergroup');
            agent.attachCookies(req);
            var data = { level: 50, timeout: 30 };
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    done();
                });
        });



    });

    describe("Clean up after", function () {

        it("should clean up node after", function (done) {
            var nodeids = ['N01334ltc', 'mochacnextnodeLFS'];
            //var nodeids = ['N0de2835chdld'];
            var nodeid = null;
            var dirty = nodeids.length;
            while (nodeid = nodeids.shift()) {
                var uri = version + '/customers/uberorg/sites/ubersite/nodes';
                if (nodeid) {
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
    });
});

