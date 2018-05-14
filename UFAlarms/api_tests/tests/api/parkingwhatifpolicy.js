/**
 * Created by Senthil on 04/26/18.
 */
"use strict";
var server_url = process.env.stack_url;
var should = require('should');
var request = require('supertest')(server_url);
var superagent = require('superagent');
var agent = superagent.agent();
var version = '/v3.0';
const uuid = require('uuid');

const helpers = require('./../../utils/helpers');
let csrfToken = null;

/*
 Datadealer needs to be running for these tests to be successful
 % lein with-profile testjs run all

 Run suite of tests with
 % cd Farallones/api_service/ui_api
 % mocha --recursive
 */

const orgId = "uberorg",
 siteId = "ubersite";

const uri = version + '/customers/'+orgId+'/sites/'+siteId+'/parking-what-if-analysis/parkingpolicies/',
    tagUri = version + '/customers/'+orgId+'/sites/'+siteId+'/parking-what-if-analysis/tags/';

let parkingPolicyTagData = {
    name: "New Uber What-if Policy Tag"+ uuid.v4(),
    description: "New Uber What-if Parking Policy Tag"
};

let parkingPolicyData = {
    "name": "New Uber What-if Parking Policy"+ uuid.v4(),
    "userId": "uberuser",    
    "description": "New Policy description",
    "timeZone": "America/Chicago",
    "tags": [],
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
let parkingPolicyId = null,
    policyTagId = null;

describe('What-if Parking Policy', function () {
    describe('GET /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/parkingpolicies', function () {
        it('should fail without login credentials', function (done) {
            request
                .get(uri)
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
            const data = { email: "uberuser@sensity.com", password: "ubeR$23" };
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

        it("should create a what-if tag using credentials", function (done) {
            var req = request.post(tagUri);
            agent.attachCookies(req);
            req.send(parkingPolicyTagData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    console.log('Created what-if policy tag ' + result.tagId + ' with name:', res.body.name);
                    policyTagId = result.tagId;
                    delete result.tagId;
                    delete result.orgId;
                    delete result.siteId;
                    delete result.createdOn;
                    delete result.lastUpdated;
                    delete result.isDeleted;
                    result.should.eql(parkingPolicyTagData);
                    done();
                });
        });

        it("should fail to create a what-if parking policy for invalid siteid", function (done) {
            //Create parking policy
            var req = request.post(version + '/customers/uberorg/sites/ZZZZZ/parkingpolicies');
            agent.attachCookies(req);
            req.send(parkingPolicyData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should fail to create a what-if parking policy for invalid payload", function (done) {
            var req = request.post(uri);
            agent.attachCookies(req);
            req.send({})
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should create a what-if parking policy using credentials", function (done) {
            //Create parking policy
            var req = request.post(uri);
            agent.attachCookies(req);
            req.send(parkingPolicyData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    console.log('Created what-if parking policy ' + result.policyId + ' with name:', res.body.name);
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

        it('should get an empty list for all what-if parking policies for invalid orgid', function (done) {
            var req = request.get(version + '/customers/XXXXX/sites/ubersite/parking-what-if-analysis/parkingpolicies');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(0);
                    done();
                });
        });

        it('should get all what-if parking policies with login credentials', function (done) {
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    console.log('parkingPolicyId', parkingPolicyId);
                    let result = res.body.filter(function (policy) {
                        return policy.policyId == parkingPolicyId;
                    })[0];
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
                    result.userName.should.eql(userName); // assert username
                    delete result.userName;
                    result.policyRule[0].policyRuleId = "";
                    res.body[0].should.eql(parkingPolicyData);
                    done();
                });

        });

    });

    describe('GET /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/parkingpolicies/{policyid}/associatedtags', function () {

        it('should fail to associate a what-if tag to what-if parking policy for incorrect tag id', function (done) {
            var data = {
                "tagids": [
                    'XXXXX'
                ]
            };
            var req = request.put(uri + parkingPolicyId + "/associatedtags");
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });
        it('should associate a what-if tag to what-if parking policy with login credentials', function (done) {
            var data = {
                "tagids": [
                    policyTagId
                ]
            };
            var req = request.put(uri + parkingPolicyId + "/associatedtags");
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should fail to disassociate a tag to parking policy for incorrect tag id', function (done) {
            var data = {
                "tagids": [
                    'YYYYY'
                ]
            };
            var req = request.delete(uri + parkingPolicyId + "/associatedtags");
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should disassociate a tag to parking policy with login credentials', function (done) {
            var data = {
                "tagids": [
                    policyTagId
                ]
            };
            var req = request.delete(uri + parkingPolicyId + "/associatedtags");
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    parkingPolicyData.tags.pop(policyTagId);
                    done();
                });
        });
    });

    describe('GET /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/parkingpolicies/{policyid}', function () {

        it("should fail to get what-if parking policy for incorrect policyid", function (done) {
            let req = request.get(uri + 'XXXXX');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should get what-if parking policy using credentials", function (done) {
            let get_req = request.get(uri + parkingPolicyId);
            agent.attachCookies(get_req);
            get_req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    delete res.body.policyId;
                    delete res.body.version;
                    delete res.body.createdOn;
                    delete res.body.lastUpdated;
                    delete res.body.isDeleted;
                    delete res.body.isLibraryPolicy;
                    delete res.body.hashValue;
                    delete res.body.siteId;
                    delete res.body.orgId;
                    delete res.body.state;
                    res.body.userName.should.eql(userName); // assert username
                    delete res.body.userName;
                    res.body.policyRule[0].policyRuleId = "";
                    res.body.should.eql(parkingPolicyData);
                    done();
                });
        });

        it("should fail to update what-if parking policy for incorrect payload", function (done) {
            let update_req = request.put(uri + parkingPolicyId);
            agent.attachCookies(update_req);
            update_req.send({})
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should fail to update what-if parking policy for incorrect policy id", function (done) {
            let update_req = request.put(uri + 'YYYYY');
            agent.attachCookies(update_req);
            update_req.send({})
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should update what-if parking policy", function (done) {
            let updatedData = parkingPolicyData;
            updatedData.name = 'Updated Uber What-if Parking Policy'+uuid.v4();
            let update_req = request.put(uri + parkingPolicyId);
            agent.attachCookies(update_req);
            update_req.send(updatedData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    const result = res.body;
                    console.log('Updated ' + parkingPolicyId + ' with name:', result.name);
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
                    result.userName.should.eql(userName); // assert username
                    delete result.userName;
                    result.policyRule[0].policyRuleId = "";
                    result.should.eql(updatedData);
                    done();
                });
        });

        it("should fail to delete what-if parking policy for incorrect policyid", function (done) {
            let delete_req = request.delete(uri + 'ZZZZZ');
            agent.attachCookies(delete_req);
            delete_req.set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    done();
                });
        });

        it("should delete what-if parking policy after", function (done) {
            let delete_req = request.delete(uri + parkingPolicyId);
            agent.attachCookies(delete_req);
            delete_req.set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    console.log('Deleted What-if Parking Policy:', parkingPolicyId);
                    should.not.exist(err);
                    done();
                });
        });

        it("should get not found error for invalid parking policy id", function (done) {
            var get_req = request.get(uri + parkingPolicyId);
            agent.attachCookies(get_req);
            get_req.set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should delete what-if tag after", function (done) {
            let delete_req = request.delete(tagUri + policyTagId);
            agent.attachCookies(delete_req);
            delete_req.set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    console.log('Deleted What-if Policy Tag:', policyTagId);
                    should.not.exist(err);
                    done();
                });
        });
    });

});