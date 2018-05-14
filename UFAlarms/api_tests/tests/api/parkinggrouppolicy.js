/**
 * Created by nagarna on 10/06/17.
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

var parkingGroupId = null;
var parkingPolicyId = null;

var parkingPolicyData = {
    "name": "New Uber Parking Policy For Group",
    "userId": "uberuser",
    "description": "New Policy description",
    "timeZone": "America/Chicago",
    "tags": [

    ],
    "policyLevelViolations": [{
        "policyViolationId": "violation ID",
        "policyViolationName": "violation Name",
        "policyViolationDescription": "violation Description",
        "policyViolationType": "ppv",
        "policyViolationFee": 0,
        "policyViolationFeeRecurrence": "once"
    }],
    "policyRule": [{
        "policyRuleId": "",
        "name": "rule name",
        "description": "rule description",
        "priority": 1,
        "parkingSchedule": {
            "description": "schedule description",
            "daysOfWeek": [
                "mon"
            ],
            "timeRange": [{
                "startTime": "19:31:03",
                "endTime": "19:32:03"
            }]
        },
        "parkingRule": {
            "description": "parking rule",
            "parkingAllowed": true,
            "parkingCharge": {
                "name": "charge name",
                "description": "charge description",
                "chargeDuration": [{
                    "name": "duration name",
                    "coarseDuration": 10,
                    "sliceDuration": 2,
                    "units": "hours",
                    "price": 0
                }],
                "maxDuration": {
                    "duration": 24,
                    "units": "hours"
                },
                "maxCharge": 60
            },
            "parkingPenalty": {
                "description": "penalty description",
                "violationFine": [{
                    "violationType": "ppv",
                    "violationFee": 0,
                    "recurrence": "once"
                }]
            }
        }
    }]
};
const userName = 'Uber User'; //uberuser user name

describe('Parking Group Policy', function () {
    describe('GET /customers/{orgid}/sites/{siteid}/parkinggroups', function () {
        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers/uberorg/sites/ubersite/parkinggroups')
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.should.eql({
                        error: true,
                        message: 'Access not granted',
                        status: 403
                    });
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
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    csrfToken = helpers.getCsrfToken(res);
                    done();
                });

        });

        it("should create a parking policy using credentials", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/parkingpolicies');
            agent.attachCookies(req);
            req.send(parkingPolicyData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    console.log('Created parking policy ' + result.policyId + ' with name:', res.body.name);
                    parkingPolicyId = result.policyId;
                    delete result.policyId;
                    delete result.version;
                    delete result.createdOn;
                    delete result.lastUpdated;
                    delete result.isDeleted;
                    delete result.isLibraryPolicy;
                    delete result.hashValue;
                    delete result.siteId;
                    delete result.orgId;
                    delete result.state;
                    result.policyRule[0].policyRuleId = "";
                    result.userName.should.eql(userName); // assert username
                    delete result.userName;
                    result.should.eql(parkingPolicyData);
                    done();
                });
        });
    });
    describe('PUT /customers/{orgid}/sites/{siteid}/parkingpolicies/{parkingpolicyid}/associatedparkinggroups}', function () {
        var uri = version + "/customers/uberorg/sites/ubersite/parkingpolicies/";

        it("set a default group name", function (done) {
            var getGroupPolicyIdByName = {
                "name": "ubergroup"
            };
            var groupReq = request.post(version + "/customers/uberorg/sites/ubersite/parkinggroups");

            agent.attachCookies(groupReq);
            groupReq.send(getGroupPolicyIdByName)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, resp) {
                    should.not.exist(err);
                    parkingGroupId = resp.body.parkinggroupid
                    console.log('Created new parking Group Id: ' + parkingGroupId)
                    done();
                });
        });

        it("should associate parking group policy using credentials", function (done) {
            var groupPolicyData = {
                "parkinggroupid": parkingGroupId
            };
            var success = {
                "success": true
            };
            var req = request.put(uri + parkingPolicyId + '/associatedparkinggroups');
            agent.attachCookies(req);
            req.send(groupPolicyData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    console.log('Associated group Id' + parkingGroupId + ' with policyid ' + parkingPolicyId);
                    result.should.eql(success);
                    done();
                });
        });

    });

    describe('GET /customers/{orgid}/sites/{siteid}/parkingpolicies/{parkingpolicyid}/associatedparkinggroups', function () {
        var uri = version + "/customers/uberorg/sites/ubersite/parkingpolicies/";
        it("should get associated parking groups for a parking policy Id in a site", function (done) {
            var req = request.get(uri + parkingPolicyId + '/associatedparkinggroups');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    console.log('Got Parking Group Ids associated with ' + parkingPolicyId)
                    delete res.body.startTime
                    delete res.body.endTime
                    delete res.body.version
                    done();
                });
        });

    });

    describe('GET /customers/{orgid}/sites/{siteid}/parkinggroups/{parkinggroupid}/associatedpolicies/fromtime/{fromtime}/totime/{totime}', function () {
        var uri = version + "/customers/uberorg/sites/ubersite/parkinggroups/";
        it("Get active parking policies of a parking group Id for a timeline", function (done) {
            var startTime = (new Date).getTime();
            var endTime = (startTime + 200000);
            var req = request.get(uri + parkingGroupId + '/associatedpolicies/fromtime/' + startTime + '/totime/' + endTime);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    console.log("Active policies for Group policy Id " + parkingGroupId + " with timeline " + startTime + " - " + endTime + " are " + JSON.stringify(res.body))
                    done();
                });
        });

        it("Return no elements - for Get active parking policies of a parking group when end timeline is less than start", function (done) {
            var startTime = (new Date).getTime();
            var endTime = (startTime - 200000);
            var req = request.get(uri + parkingGroupId + '/associatedpolicies/fromtime/' + startTime + '/totime/' + endTime);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    console.log("Active policies returned null for Group policy Id " + parkingGroupId + " with timeline " + startTime + " - " + endTime + " -- " + JSON.stringify(res.body))
                    done();
                });
        });

        it("Return error - for Get active parking policies of a parking group", function (done) {
            var startTime = (new Date).getTime();
            var endTime = (startTime + 200000);
            var errorGroupId = 'abcd-1234-wxyz-5678'
            var req = request.get(uri + errorGroupId + '/associatedpolicies/fromtime/' + startTime + '/totime/' + endTime);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    console.log("Error: Specified parking group - " + errorGroupId + " is not associated with any policy, Status: " + JSON.stringify(res.body.status))
                    done();
                });
        });

    });

    describe('GET /customers/{orgid}/sites/{siteid}/parkinggroups/{parkinggroupid}/activepolicy', function () {
        var uri = version + "/customers/uberorg/sites/ubersite/parkinggroups/";
        it("Get active parking policies of a parking group Id", function (done) {
            var req = request.get(uri + parkingGroupId + '/activepolicy');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    console.log("Active policy for Group policy Id " + parkingGroupId)
                    done();
                });
        });

    });


    describe('DELETE /customers/{orgid}/sites/{siteid}/parkingpolicies/{parkingpolicyid}/associatedparkinggroups', function () {
        var uri = version + "/customers/uberorg/sites/ubersite/parkingpolicies/";
        it("should disassociated parking groups from parking policy in a site", function (done) {
            var success = {
                "success": true
            };
            var groupPolicyDataForDelete = {
                "parkinggroupid": parkingGroupId
            };
            var req = request.delete(uri + parkingPolicyId + '/associatedparkinggroups');
            agent.attachCookies(req);
            req.send(groupPolicyDataForDelete)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    console.log('Deleted group policy associated with parkingId ' + parkingPolicyId)
                    res.body.should.eql(success);
                    done();
                });
        });

    });

});