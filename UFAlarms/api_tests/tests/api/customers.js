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

describe('Customers', function () {
    describe('GET /customers', function () {
        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers')
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

        it("should access customers with login credentials", function (done) {
            var req = request.get(version + '/customers');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.be.above(0);
                    done();
                });

        });

        var orgid = null;
        it("should fail to create customer with invalid params", function (done) {
            var data = {
                "name": "Test Org",
                "type": "defaultx",
                "street1": "street1",
                "street2": "street2",
                "city": "city",
                "state": "state",
                "postal_code": "postal_code",
                "country": "country",
                "contact_email": "",
                "contact_phone": "",
                "contact_name": ""
            };
            var req = request.post(version + '/customers');
            agent.attachCookies(req);
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    var result = res.body;
                    if (result && result.message)
                        console.log(">> Response message: ", result.message);
                    should.not.exist(err);

                    done();
                });
        });

        var orgid = null;
        it("should create customer using credentials", function (done) {
            var data = {
                "name": "Test Org",
                "type": "partner",
                "street1": "street1",
                "street2": "street2",
                "city": "city",
                "state": "state",
                "postal_code": "postal_code",
                "country": "country",
                "contact_email": "",
                "contact_phone": "",
                "contact_name": ""
            };
            var req = request.post(version + '/customers');
            agent.attachCookies(req);
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var result = res.body;
                    if (result && result.message)
                        console.log(">> Response message: ", result.message);
                    should.not.exist(err);
                    orgid = result.orgid;
                    delete result.orgid;
                    delete result.po;
                    result.should.eql(data);

                    done();
                });
        });

        var childorgid = null;

        it("should create child customer using credentials", function (done) {
            var data = {
                "name": "Test Child Org",
                "type": "default",
                "street1": "street1",
                "street2": "street2",
                "city": "city",
                "state": "state",
                "postal_code": "postal_code",
                "country": "country",
                "contact_email": "",
                "contact_phone": "",
                "contact_name": ""
            };
            var req = request.post(version + '/customers/' + orgid + "/of");
            agent.attachCookies(req);
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var result = res.body;
                    if (result && result.message)
                        console.log(">> Response message: ", result.message);
                    should.not.exist(err);
                    childorgid = result.orgid;
                    done();
                });
        });

        it("should fail to create child child customer using credentials", function (done) {
            var data = {
                "name": "Test Child Child Org",
                "type": "default",
                "street1": "street1",
                "street2": "street2",
                "city": "city",
                "state": "state",
                "postal_code": "postal_code",
                "country": "country",
                "contact_email": "",
                "contact_phone": "",
                "contact_name": ""
            };
            var req = request.post(version + '/customers/' + childorgid + "/of");
            agent.attachCookies(req);
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.exist(err);
                    done();
                });
        });

        it("should deactivate and then reactivate a org using credentials", function (done) {
            //var orgid = "testorg";
            var uri = version + "/suspended-customers/" + orgid;
            var req = request.put(uri);

            agent.attachCookies(req);
            req.set('Accept', 'application/json')
                .set('Content-Type', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);

                    //var orgid = "testorg";
                    var uri = version + "/suspended-customers/";
                    var req = request.get(uri);
                    agent.attachCookies(req);
                    req.expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);

                            var items = res.body;
                            items.length.should.eql(1);

                            //var orgid = "testorg";
                            var uri = version + "/suspended-customers/" + orgid;
                            var req = request.delete(uri);
                            agent.attachCookies(req);
                            req.set('Accept', 'application/json')
                                .set('Content-Type', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect(204)
                                .end(function (err, res) {
                                    should.not.exist(err);

                                    //var orgid = "testorg";
                                    var uri = version + "/suspended-customers";
                                    var req = request.get(uri);
                                    agent.attachCookies(req);
                                    req.expect('Content-Type', /json/)
                                        .expect(200)
                                        .end(function (err, res) {
                                            should.not.exist(err);

                                            var items = res.body;
                                            items.length.should.eql(0);
                                            done();
                                        });

                                });
                        });

                });
        });

        it("should get, update, and delete a customer using credentials", function (done) {
            var uri = version + "/customers";
            //var orgid = "testorg";
            var data = {
                //"orgid": "testorg",
                name: 'Test Org',
                type: 'partner',
                street1: 'street1',
                street2: 'street2',
                city: 'city',
                state: 'state',
                postal_code: 'postal_code',
                country: 'country',
                contact_email: '',
                contact_phone: '',
                contact_name: ''
            };
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    for (var ii = 0; ii < res.body.length; ii++) {
                        if (res.body[ii].name == 'Test Org') {
                            var resOrg = res.body[ii];
                        }
                    }
                    var orgid = resOrg.orgid;
                    delete resOrg.orgid;
                    delete resOrg.po;
                    resOrg.should.eql(data);

                    var updated = {
                        name: 'Uber Org',
                        type: "default",
                        city: 'New York',
                        country: 'USA',
                        postal_code: '10012',
                        state: 'NY',
                        street1: 'Broadway',
                        street2: 'Suite 400',
                        contact_email: '',
                        contact_phone: '',
                        contact_name: ''
                    }
                    var req2 = request.post(uri + "/" + orgid);
                    agent.attachCookies(req2);
                    req2.send(updated)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res2) {
                            should.not.exist(err);
                            var result = res2.body;
                            should.exist(result.po);
                            should.exist(result.pname);
                            delete result.orgid;
                            delete result.po;
                            delete result.pname;
                            result.should.eql(updated);
                            should.not.exist(err);

                            var delete_req = request.delete(uri + "/" + childorgid);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                //.expect('Content-Type', /json/)
                                .expect(204)
                                .end(function (err, res) {
                                    should.not.exist(err);


                                    var delete_req = request.delete(uri + "/" + orgid);
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
                });
        });

        it("should access customers with login credentials", function (done) {
            var req = request.get(version + '/customers');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.be.above(0);
                    done();
                });

        });
    });
});