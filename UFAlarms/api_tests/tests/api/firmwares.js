/**
 * Created by brefsdal on 12/16/15.
 */
"use strict";
var server_url = process.env.stack_url;
var should = require('should'),
    request = require('supertest')(server_url),
    superagent = require('superagent'),
    agent = superagent.agent(),
    version = '/v3.0';

const helpers = require('./../../utils/helpers');
let csrfToken = null, siteid = null;

/*
 Datadealer needs to be running for these tests to be successful
 % lein with-profile testjs run all

 Run suite of tests with
 % cd Farallones/api_service/ui_api
 % mocha --recursive
 */


describe('Firmwares', function () {

    describe('GET /firmwares', function () {
        it('should fail without login credentials', function (done) {
            request
                .get(version + '/firmwares')
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

        it('should create a firmware using credentials', function (done) {
            var data = {
                name: 'My firmware',
                release: '1',
                released: true,
                commit: '6d5ce96',
                deprecated: false,
                checksum: 'sum1',
                builder: 'My Builder',
                build_date: 'today',
                image_size: 1024
            },
                req = request.post(version + '/firmwares');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    delete result.firmwareid;
                    result.should.eql(data);
                    done();
                });
        });

        it('should get, update, and delete firmware using credentials', function (done) {
            var firmwareid = null;
            var uri = version + '/firmwares',
                data = {
                    name: 'My firmware',
                    release: '1',
                    released: true,
                    commit: '6d5ce96',
                    deprecated: false,
                    version: '1.6d5ce96',
                    checksum: 'sum1',
                    builder: 'My Builder',
                    build_date: 'today',
                    image_size: 1024
                },
                req = request.get(uri);

            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    firmwareid = res.body[0].firmwareid;
                    delete res.body[0].firmwareid;
                    delete res.body[0].nodes;
                    res.body[0].should.eql(data);

                    var updated = {
                        name: 'My firmware Updated',
                        release: '2',
                        released: false,
                        commit: '6d5ce96',
                        deprecated: false,
                        version: '2.6d5ce96',
                        checksum: 'sum1',
                        builder: 'My Builder',
                        build_date: 'today',
                        image_size: 2048
                    },
                        req2 = request.post(uri + '/' + firmwareid);
                    agent.attachCookies(req2);
                    req2.send(updated)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res2) {
                            should.not.exist(err);
                            var result = res2.body;
                            delete result.firmwareid;
                            delete result.nodes;
                            result.should.eql(updated);
                            var delete_req = request.delete(uri + '/' + firmwareid);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect(204)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    var get_req = request.get(uri + '/' + firmwareid);

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
        });

        it('should access firmwares with login credentials', function (done) {
            var req = request.get(version + '/firmwares');
            agent.attachCookies(req);
            req.expect(200)
                .end(function (err, res) {
                    // We just want to see if request is 200 OK as we use multiple services
                    should.not.exist(err);
                    done();
                });

        });
    });

    describe('POST /firmwares/{firmwareid}/assign/...', function () {
        it('should sign in with correct credentials', function (done) {
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
            var data1 = {
                name: 'Test Firmware Site',
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
                    result.should.eql(data1);
                    done();
                });
        });

        it('should create a firmware v4-site using credentials', function (done) {
            var data = {
                name: 'v4 Site',
                release: '1',
                released: true,
                commit: 'firmwaresite',
                deprecated: false,
                checksum: 'sum1',
                builder: 'My Builder',
                build_date: 'today',
                image_size: 1024
            },
                req = request.post(version + '/firmwares');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    delete result.firmwareid;
                    delete result.nodes;

                    result.should.eql(data);
                    done();
                });
        });

        it('should create a firmware v4-node using credentials', function (done) {
            var data = {
                name: 'v4 Node',
                release: '1',
                released: true,
                commit: 'firmwarenode',
                deprecated: false,
                checksum: 'sum1',
                builder: 'My Builder',
                build_date: 'today',
                image_size: 1024
            },
                req = request.post(version + '/firmwares');

            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;

                    delete result.firmwareid;
                    delete result.nodes;

                    result.should.eql(data);
                    done();
                });
        });

        it('should create a node using credentials', function (done) {
            var createNodereq = request.put(version + '/nodes');
            agent.attachCookies(createNodereq);
            var nodeid = "N013341e1";
            var payload = { csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude,\nN013341e1,unode-v4,uberorg,ubersite,44.4,41.2" };
            createNodereq.send(payload)
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    done();
                });
        });

        it('should activate a node using credentials', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/' + siteid + '/nodes/activate/N013341e1');
            agent.attachCookies(req);
            req.send()
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should assign a node using credentials', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/' + siteid + '/nodes/N013341e1/assign');
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

        it('should assign firmware to node', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/' + siteid + '/firmwares/firmwarenode/assign/node/N013341e1');
            agent.attachCookies(req);
            req.send({})
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should assign firmware to site', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/' + siteid + '/firmwares/firmwaresite/assign/site');
            agent.attachCookies(req);
            req.send({})
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should fail to assign unknown firmware', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/' + siteid + '/firmwares/unknown/assign/site');
            agent.attachCookies(req);
            req.send({})
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(404)
                .end(function (err, res) {
                    done();
                });
        });
    });

    describe("Clean up firmwares and site", function () {
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

        it('should clean up the created firmwares', function (done) {
            var get_req = request.get(version + '/firmwares');
            agent.attachCookies(get_req);
            get_req
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exists(err);
                    var count = res.body.length;

                    res.body.forEach(function (item) {
                        var delete_req = request.delete(version + '/firmwares/' + item.firmwareid);
                        agent.attachCookies(delete_req);
                        delete_req.set('Accept', 'application/json')
                            .set('X-CSRF-Token', csrfToken)
                            .expect(204)
                            .end(function (err, res) {
                                should.not.exist(err);
                                var get_req = request.get(version + '/firmwares/' + item.firmwareid);
                                agent.attachCookies(get_req);
                                get_req.set('Accept', 'application/json')
                                    .expect('Content-Type', /json/)
                                    .expect(404)
                                    .end(function (err, res) {
                                        count -= 1;
                                        if (count === 0) {
                                            done();
                                        }
                                    });
                            });
                    });

                });
        });
    });
});