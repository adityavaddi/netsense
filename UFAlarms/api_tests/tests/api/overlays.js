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


describe('Overlays', function () {
    describe('GET /customers/{orgid}/sites/{siteid}/overlays', function () {
        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers/uberorg/sites/ubersite/overlays')
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
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    csrfToken = helpers.getCsrfToken(res);
                    done();
                });

        });

        it("should create a overlay using credentials", function (done) {
            var data = {
                fileName: "filename",
                buildingLevel: "1",
                description: "My overlay",
                users: "admin",
                imageBounds: "30x30",
                imageType: "png",
                imageData: "data"
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/overlays');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    delete result.overlayid;
                    result.should.eql(data);
                    done();
                });
        });

        it("should get, update, and delete overlay using credentials", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/overlays";
            var data = {
                fileName: "filename",
                buildingLevel: "1",
                description: "My overlay",
                users: "admin",
                imageBounds: "30x30",
                imageType: "png",
                imageData: "data"
            };
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var overlayid = res.body[0].overlayid;
                    delete res.body[0].overlayid;
                    res.body[0].should.eql(data);
                    var updated = {
                        fileName: "filename",
                        buildingLevel: "2",
                        description: "My overlay updated",
                        users: "admin",
                        imageBounds: "60x60",
                        imageType: "jpg",
                        imageData: "data updated"
                    };
                    var req2 = request.post(uri + "/" + overlayid);
                    agent.attachCookies(req2);
                    req2.send(updated)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res2) {
                            should.not.exist(err);
                            var result = res2.body;
                            delete result.overlayid;
                            result.should.eql(updated);

                            var delete_req = request.delete(uri + "/" + overlayid);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {

                                    var get_req = request.get(uri + "/" + overlayid);
                                    agent.attachCookies(get_req);
                                    get_req.set('Accept', 'application/json')
                                        .expect('Content-Type', /json/)
                                        .expect(404)
                                        .end(function (err, res) {
                                            should.not.exist(err);

                                            var get_all_req = request.get(uri);
                                            agent.attachCookies(get_all_req);
                                            get_all_req.set('Accept', 'application/json')
                                                .expect('Content-Type', /json/)
                                                .expect(200)
                                                .end(function (err, res) {
                                                    should.not.exist(err);
                                                    done();
                                                });
                                        });
                                });
                        });
                });
        });

        it("should access overlays with login credentials", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/overlays');
            agent.attachCookies(req);
            req//.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    //res.body.length.should.eql(0);
                    done();
                });

        });
    });
});
