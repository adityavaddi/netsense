/**
 * Created by Rajitha on 01/03/18.
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
const uri = version + '/customers/uberorg/sites/ubersite/businessalerts';
var businessAlertId = 'businessalertid';

describe('Business Alerts', function () {
    describe('GET /customers/{orgid}/sites/{siteid}/businessalerts', function () {

        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers/uberorg/sites/ubersite/businessalerts')
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.should.eql({
                        error: true,
                        message: 'Access denied',
                        "status": 403
                    });
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

        it('should get an empty list for all business alerts for invalid orgid', function (done) {
            var req = request.get(version + '/customers/XXXXX/sites/ubersite/businessalerts');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should get all business alerts with login credentials', function (done) {
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });
    });
    describe('GET /customers/{orgid}/sites/{siteid}/businessalerts/{businessalertid}', function () {

        it("should get, dismiss business alert using credentials", function (done) {
            var get_req = request.get(uri + "/" + businessAlertId);
            agent.attachCookies(get_req);
            get_req.expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });
    });
    describe('DISMISS /customers/{orgid}/sites/{siteid}/businessalerts/{businessalertid}', function () {
        it("should not found error for invalid businessalertid", function (done) {
            var update_req = request.put(uri + "/" + businessAlertId + "/dismiss");
            agent.attachCookies(update_req);
            update_req.send()
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res2) {
                    should.not.exist(err);
                    done()
                });
        });
    });

    describe('SEARCH /customers/{orgid}/sites/{siteid}/businessalerts', function () {

        it("should search business alert using credentials", function (done) {
            var get_req = request.get(uri + '?filter=active eq ' + 'true');
            agent.attachCookies(get_req);
            get_req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    done();
                });
        });

        it("should search fail for invalid search criteria using credentials", function (done) {
            var get_req = request.get(uri + '?filter=active eq ' + 'trued');
            agent.attachCookies(get_req);
            get_req.expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    console.log('error message' + JSON.stringify(res.body.message))
                    res.body.should.eql({
                        error: true,
                        message: 'active could be either true or false',
                        "status": 400
                    });

                    done();
                });
        });
        it("should search fail for invalid search criteria using credentials", function (done) {
            var get_req = request.get(uri + '?filter=invalidkey eq ' + 'trued');
            agent.attachCookies(get_req);
            get_req.expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    console.log('error message' + JSON.stringify(res.body.message))
                    res.body.should.eql({
                        error: true,
                        message: 'search can be performed on any one of these severity, active, triggerName, resourceId ',
                        "status": 400
                    });

                    done();
                });
        });
    });
});
