/**
 * Created by Senthil on 04/26/18.
 */
const server_url = process.env.stack_url;
const should = require('should');
const request = require('supertest')(server_url);
const superagent = require('superagent');
const agent = superagent.agent();
const uuid = require('uuid');
const _ = require('lodash');

const version = '/v3.0';
const helpers = require('./../../utils/helpers');
let csrfToken = null;

const orgId = "uberorg",
 siteId = "ubersite";

const uri = version + '/customers/'+orgId+'/sites/'+siteId+'/parking-what-if-analysis/jobs',
    parkingPolicyUri = version + '/customers/'+orgId+'/sites/'+siteId+'/parking-what-if-analysis/parkingpolicies';

let jobId = null,
    parkingPolicyId = null;
    const jobName = "mocha what-if-job-" + uuid.v4(),
    policyName = "New Uber What-if Parking Policy-" + uuid.v4();

let fromDateTime = new Date(),
    toDateTime = new Date();
fromDateTime.setDate(fromDateTime.getDate() - 1);
toDateTime.setDate(toDateTime.getDate());

let whatIfJobData = {
    "name": jobName,
    "fromTime": fromDateTime.toISOString(),
    "toTime": toDateTime.toISOString(),
    "parkingPolicies": [],
    "parkingGroups": [
        "newparkinggroup"
    ],
    "mode": "as-is"
};

const parkingPolicyData = {
    "name": policyName,
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
const jobAssertProperties = ['name', 'fromTime', 'toTime', 'parkingPolicies', 'parkingGroups', 'mode', 'createdByUserName'];
describe('What-if Jobs', function () {
    describe('GET /customers/{orgId}/sites/{siteId}/parking-what-if-analysis/jobs', function () {

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
                        status: 403
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

        it("should create a what-if policy using credentials", function (done) {
            //Create parking policy
            var req = request.post(parkingPolicyUri);
            agent.attachCookies(req);
            req.send(parkingPolicyData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    console.log('Created what-if policy ' + result.policyId + ' with name:', res.body.name);
                    parkingPolicyId = result.policyId
                    whatIfJobData.parkingPolicies.push(result.policyId);
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

        it("should fail to create what-if job for invalid siteId", function (done) {
            let req = request.post(version + '/customers/uberorg/sites/XXXXX/parking-what-if-analysis/jobs');
            agent.attachCookies(req);
            req.send(whatIfJobData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should fail to create what-if job for incorrect payload", function (done) {
            let req = request.post(uri);
            agent.attachCookies(req);
            const invalidWhatIfJobData = {};
            req.send(invalidWhatIfJobData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should create what-if job " + jobName, function (done) {
            let req = request.post(uri);
            agent.attachCookies(req);
            req.send(whatIfJobData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    let result = res.body;
                    console.log(`Created what-if job ${result.jobId} with name ${result.name}`);

                    jobId = result.jobId;
                    // pick only the attributes that needs to be asserted
                    const resultObj = _.pick(result, jobAssertProperties);

                    resultObj.createdByUserName.should.eql(userName); // assert username
                    delete resultObj.createdByUserName;
                    resultObj.should.eql(whatIfJobData);
                    done();
                });
        });

        it("should fail to create what-if job with the same name " + jobName, function (done) {
            let req = request.post(uri);
            agent.attachCookies(req);
            req.send(whatIfJobData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should get an empty list for all what-if jobs for invalid orgid', function (done) {
            var req = request.get(version + '/customers/XXXXX/sites/ubersite/parking-what-if-analysis/jobs');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(0);
                    done();
                });
        });

        it('should get all what-if jobs and check if created what-if job is available', function (done) {
            let req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    let result = res.body.filter(function (job) {
                        return job.jobId == jobId;
                    })[0];
                    const resultObj = _.pick(result, jobAssertProperties);
                    resultObj.createdByUserName.should.eql(userName); // assert username
                    delete resultObj.createdByUserName;
                    resultObj.should.eql(whatIfJobData);
                    done();
                });
        });
    });

    describe('GET /customers/{orgId}/sites/{siteId}/parking-what-if-analysis/jobs/{jobId}', function () {

        it('should fail to get what-if job with incorrect job id', function (done) {
            let req = request.get(uri + '/DDDDD');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should get what-if job details for a valid job id', function (done) {
            let req = request.get(uri + '/' + jobId);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    const resultObj = _.pick(res.body, jobAssertProperties);
                    resultObj.createdByUserName.should.eql(userName); // assert username
                    delete resultObj.createdByUserName;
                    resultObj.should.eql(whatIfJobData);
                    done();
                });
        });

        it("should fail to update what-if job in pending status", function (done) {
            let req = request.put(uri + '/' + jobId);
            agent.attachCookies(req);
            const updatedWhatIfData = whatIfJobData;
            updatedWhatIfData.name = "mocha what-if-job";
            req.send(updatedWhatIfData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        // DELETE what-if job api is not supported in v3.0.8
        /*it("should fail to delete what-if job with invalid job id", function (done) {
            let req = request.delete(uri + '/ZZZZZ');
            agent.attachCookies(req);
            req
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should delete what-if job with valid job id", function (done) {
            let req = request.delete(uri + '/' + jobId);
            agent.attachCookies(req);
            req
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    console.log(`Deleted what-if job ${jobId}`);
                    done();
                });
        });

        it('should fail to get what-if job that just got deleted', function (done) {
            let req = request.get(uri + '/' + jobId);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });*/

        it('should fail to delete what-if policy which is linked to what-if job', function (done) {
            var delete_req = request.delete(parkingPolicyUri + '/' + parkingPolicyId);
            agent.attachCookies(delete_req);
            delete_req.set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });
    });
});