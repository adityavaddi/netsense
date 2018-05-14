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

describe('Sites', function () {
    var siteid = '';

    describe('GET /customers/<orgid>/sites', function () {
        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers/uberorg/sites')
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
            this.timeout(1e5);
            var data = {
                email: 'uberuser@sensity.com',
                password: 'ubeR$23'
            };
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

        it('should create a site using credentials', function (done) {
            this.timeout(1e5);
            var data1 = {
                name: 'Test Site',
                street1: 'street1',
                street2: 'street2',
                city: 'city',
                state: 'state',
                postal_code: 'postal_code',
                country: 'country',
                latitude: '37.325944790869585',
                longitude: '-121.94748993526446',
                altitude: '5'
            },
                req = request.post(version + '/customers/uberorg/sites');

            agent.attachCookies(req);
            req.send(data1)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    var result = res.body;

                    siteid = result.siteid;
                    delete result.siteid;
                    data1.country_code = 'US';
                    data1.time_zone = 'America/Los_Angeles';

                    result.should.eql(data1);
                    done();
                });
        });

        it('should deactivate and then reactivate a site using credentials', function (done) {
            var orgid = 'uberorg',
                uri = version + '/customers/' + orgid + '/suspended-sites/' + siteid,
                req = request.put(uri);

            agent.attachCookies(req);
            req.set('Accept', 'application/json')
                .set('Content-Type', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);

                    var orgid = 'uberorg',
                        uri = version + '/customers/' + orgid + '/suspended-sites',
                        req = request.get(uri);

                    agent.attachCookies(req);
                    req.expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);
                            var resSite = res.body.filter(item => item.name == 'Test Site');
                            resSite.should.not.eql(null);
                            //res.body.length.should.eql(1);

                            var orgid = 'uberorg',
                                uri = version + '/customers/' + orgid + '/suspended-sites/' + siteid,
                                req = request.delete(uri);

                            agent.attachCookies(req);
                            req.set('Accept', 'application/json')
                                .set('Content-Type', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect(204)
                                .end(function (err, res) {
                                    should.not.exist(err);

                                    var orgid = 'uberorg',
                                        uri = version + '/customers/' + orgid + '/suspended-sites',
                                        req = request.get(uri);

                                    agent.attachCookies(req);
                                    req.expect('Content-Type', /json/)
                                        .expect(200)
                                        .end(function (err, res) {
                                            should.not.exist(err);
                                            var resSite = res.body.filter(item => item.name == 'Test Site');
                                            resSite.length.should.eql(0);
                                            //res.body.length.should.eql(0);
                                            done();
                                        });

                                });
                        });

                });
        });

        it('should get and update a site using credentials', function (done) {
            var uri = version + '/customers/uberorg/sites',
                data = {
                    name: 'Test Site',
                    street1: 'street1',
                    street2: 'street2',
                    city: 'city',
                    state: 'state',
                    postal_code: 'postal_code',
                    country: 'country',
                    latitude: '37.325944790869585',
                    longitude: '-121.94748993526446',
                    altitude: '5',
                    country_code: 'US',
                    time_zone: 'America/Los_Angeles'
                },
                req = request.get(uri);

            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    var resSites = res.body.filter(item => item.name == 'Test Site');
                    resSites.length.should.not.eql(0);
                    var resSite = resSites[0];
                    var siteid = resSite.siteid;
                    delete resSite.siteid;
                    resSite.should.eql(data);

                    var updated = {
                        city: 'San Francisco',
                        country: 'USA',
                        name: 'Test Site 2',
                        postal_code: '94107',
                        state: 'CA',
                        street1: '1455 Market St.',
                        street2: 'Suite 200',
                        latitude: '37.325944790869585',
                        longitude: '-121.94748993526446',
                        altitude: '5'
                    },
                        req2 = request.post(uri + '/' + siteid);

                    agent.attachCookies(req2);
                    req2.send(updated)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res2) {
                            should.not.exist(err);
                            var result = res2.body;
                            delete result.siteid;

                            updated.country_code = 'US';
                            updated.time_zone = 'America/Los_Angeles';
                            result.should.eql(updated);

                            done();
                            /*var delete_req = request.delete(uri + '/' + siteid);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                //.expect('Content-Type', /json/)
                                .expect(204)
                                .end(function(err, res) {
                                    done();
                                });*/
                        });
                });
        });

        it('should access sites with login credentials', function (done) {
            var req = request.get(version + '/customers/uberorg/sites');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.be.above(0);
                    done();
                });

        });

        it('should fail create a site using wrong lat/long', function (done) {
            this.timeout(1e5);
            var data = {
                name: 'Test Site',
                street1: 'street1',
                street2: 'street2',
                city: 'city',
                state: 'state',
                postal_code: 'postal_code',
                country: 'country',
                latitude: ' 37.325944790869585',
                longitude: '-121.94748993526446',
                altitude: '5'
            },
                req = request.post(version + '/customers/uberorg/sites');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    data.longitude = "-123."
                    var req1 = request.post(version + '/customers/uberorg/sites');

                    agent.attachCookies(req1);
                    req1.send(data)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(400)
                        .end(function (err, res) {
                            done();
                        });

                });
        });

        it("should clean up created site", function (done) {
            var uri = version + '/customers/uberorg/suspended-sites/' + siteid;
            var req = request.put(uri);
            agent.attachCookies(req);
            req.set('Accept', 'application/json')
                .set('Content-Type', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });
    });
});