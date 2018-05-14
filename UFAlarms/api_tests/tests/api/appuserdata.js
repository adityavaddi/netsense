/**
 * Created by Senthil on 10/10/17.
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

/* Datadealer needs to be running for these tests to be successful
 % lein with-profile testjs run all

 Run suite of tests with
 % cd Farallones/api_service/ui_api
 % mocha --recursive
 */


var appUserDataId = 'uberappuserdataid',
    userName = 'Uber User', //uberuser user name
    appUserData = {
        "appid": "uberappid",
        "userid": "uberuser",
        "datavalue": "New App User Data Value"
    };

describe('App Userdata', function () {
    describe('GET /applications/{appid}/users/{userid}/userdata', function () {

        it('should fail without login credentials', function (done) {
            request
                .get(version + '/applications/appid/users/userid/userdata')
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

        it("should create a app user data using credentials", function (done) {
            var req = request.post(version + '/applications/uberappid/users/uberuser/userdata');
            agent.attachCookies(req);
            req.send(appUserData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    console.log('Created ' + appUserDataId + ' with name:', res.body.datavalue);
                    appUserDataId = result.userdataid
                    delete result.userdataid;
                    delete result.createdon;
                    delete result.lastupdated;
                    delete result.isdeleted;
                    result.userName.should.eql(userName); // assert username
                    delete result.userName;
                    result.should.eql(appUserData);
                    done();
                });
        });

        it('should get all app user data with login credentials', function (done) {
            var req = request.get(version + '/applications/uberappid/users/uberuser/userdata');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    delete res.body[0].userdataid;
                    delete res.body[0].createdon;
                    delete res.body[0].lastupdated;
                    delete res.body[0].isdeleted;
                    res.body[0].userName.should.eql(userName); // assert username
                    delete res.body[0].userName;
                    res.body[0].should.eql(appUserData);
                    done();
                });

        });
    });

    describe('GET /applications/{appid}/users/{userid}/userdata/{userdataid}', function () {

        it("should get, update and delete app user data using credentials", function (done) {
            var uri = version + "/applications/uberappid/users/uberuser/userdata/" + appUserDataId;
            var get_req = request.get(uri);
            agent.attachCookies(get_req);
            get_req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    delete res.body.userdataid;
                    delete res.body.createdon;
                    delete res.body.lastupdated;
                    delete res.body.isdeleted;
                    res.body.userName.should.eql(userName); // assert username
                    delete res.body.userName;
                    res.body.should.eql(appUserData);

                    var updatedData = appUserData;
                    updatedData.datavalue = "Updated App User Data Value";
                    var update_req = request.put(uri);
                    agent.attachCookies(update_req);
                    update_req.send(updatedData)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res2) {
                            console.log('Updated ' + appUserDataId + ' with data value:', res2.body.datavalue);
                            should.not.exist(err);
                            delete res2.body.userdataid;
                            delete res2.body.createdon;
                            delete res2.body.lastupdated;
                            delete res2.body.isdeleted;
                            res2.body.userName.should.eql(userName); // assert username
                            delete res2.body.userName;
                            res2.body.should.eql(updatedData);

                            var delete_req = request.delete(uri);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    console.log('Deleted ' + appUserDataId);
                                    var get_all_req = request.get(version + '/applications/uberappid/users/uberuser/userdata');
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

        it("should get not found error for invalid app user data id", function (done) {
            var uri = version + "/applications/uberappid/users/uberuser/userdata/" + appUserDataId;
            var get_req = request.get(uri);
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
