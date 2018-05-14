/**
 * Created by Senthil on 03/05/18.
 */
const server_url = process.env.stack_url;
const should = require('should');
const request = require('supertest')(server_url);
const superagent = require('superagent');
const agent = superagent.agent();
const uuid = require('uuid');
const version = '/v3.0';

const helpers = require('./../../utils/helpers');
let csrfToken = null;

const uri = version + '/customers/uberorg/sites/ubersite/triggers';
let triggerId = null;
const triggerName = "mocha turnover-"+ uuid.v4();
const updatedTriggerName = "mocha turnover-"+ uuid.v4();

let triggerData = {
    "triggerName": triggerName,
    "triggerCategory": "Business Alert - Parking",
    "triggerSubcategory": "group",
    "resourceList": [{
        "resourceId": "3881f299-2450-4f2c-a2d6-03635938b836",
        "resourceName": "Marietta"
    }],
    "timePeriod": "15min",
    "triggerVariable": "TotalViolationCount",
    "comparisonVariableOperator": "Percentage",
    "comparisonVariable": "Turnover",
    "comparisonOperator": "GT",
    "comparisonValue": "60",
    "userMessage": "Turnover % TotalViolationCount of parking group Marietta over time period 15min is > 60",
    "additionalUserMessage": "Take immediate action",
    "severity": "Major"
};
const userName = 'Uber User'; //uberuser user name

describe('Triggers', function () {
    describe('GET /customers/{orgId}/sites/{siteId}/triggers', function () {

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
            const data = { 
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

        it("should fail to create trigger for invalid siteId", function (done) {
            let req = request.post(version + '/customers/uberorg/sites/XXXXX/triggers');
            agent.attachCookies(req);
            req.send(triggerData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should fail to create trigger for incorrect payload", function (done) {
            let req = request.post(uri);
            agent.attachCookies(req);
            const invalidTriggerData = {};
            req.send(invalidTriggerData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should create trigger "+triggerName, function (done) {
            let req = request.post(uri);
            agent.attachCookies(req);
            req.send(triggerData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    let result = res.body;
                    console.log(`Created trigger ${result.triggerId} with name ${result.triggerName}`);

                    triggerId = result.triggerId;

                    delete result.triggerId;
                    delete result.orgId;
                    delete result.siteId;
                    delete result.userId;
                    delete result.createdOn;
                    delete result.lastUpdated;
                    delete result.isDeleted;
                    result.userName.should.eql(userName); // assert username
                    delete result.userName;
                    result.should.eql(triggerData);
                    done();
                });
        });

        it("should fail to create trigger with the same name "+triggerName, function (done) {
            let req = request.post(uri);
            agent.attachCookies(req);
            req.send(triggerData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should get all triggers and check if created trigger is available', function (done) {
            let req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    let result = res.body.filter(function (trigger) {
                        return trigger.triggerId == triggerId;
                    })[0];

                    delete result.triggerId;
                    delete result.orgId;
                    delete result.siteId;
                    delete result.userId;
                    delete result.createdOn;
                    delete result.lastUpdated;
                    delete result.isDeleted;
                    result.userName.should.eql(userName); // assert username
                    delete result.userName;
                    result.should.eql(triggerData);
                    done();
                });
        });

        it('should get 400 bad request for incorrect filter query `filter=AAAAA eq `BBBBB`', function (done) {
            let req = request.get(uri + "?filter=AAAAA eq 'BBBBB'");
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should get empty result for filter query `filter=severity eq `CCCCC` with invalid value', function (done) {
            let req = request.get(uri + "?filter=severity eq 'CCCCC'");
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(0);
                    done();
                });
        });

        it('should get all triggers for filter query `triggerName eq `mocha turnover`', function (done) {
            let req = request.get(uri + "?filter=triggerName eq '"+triggerName+"'");
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    let result = res.body.filter(function (trigger) {
                        return trigger.triggerId == triggerId;
                    })[0];

                    delete result.triggerId;
                    delete result.orgId;
                    delete result.siteId;
                    delete result.userId;
                    delete result.createdOn;
                    delete result.lastUpdated;
                    delete result.isDeleted;
                    result.userName.should.eql(userName); // assert username
                    delete result.userName;
                    result.should.eql(triggerData);
                    done();
                });
        });

        it('should get all triggers for filter query `triggerCategory eq `Business Alert - Parking`', function (done) {
            let req = request.get(uri + "?filter=triggerCategory eq 'Business Alert - Parking'");
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    let result = res.body.filter(function (trigger) {
                        return trigger.triggerId == triggerId;
                    })[0];

                    delete result.triggerId;
                    delete result.orgId;
                    delete result.siteId;
                    delete result.userId;
                    delete result.createdOn;
                    delete result.lastUpdated;
                    delete result.isDeleted;
                    result.userName.should.eql(userName); // assert username
                    delete result.userName;
                    result.should.eql(triggerData);
                    done();
                });
        });

        it('should get all triggers for filter query `severity eq `Major`', function (done) {
            let req = request.get(uri + "?filter=severity eq 'Major'");
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    let result = res.body.filter(function (trigger) {
                        return trigger.triggerId == triggerId;
                    })[0];

                    delete result.triggerId;
                    delete result.orgId;
                    delete result.siteId;
                    delete result.userId;
                    delete result.createdOn;
                    delete result.lastUpdated;
                    delete result.isDeleted;
                    result.userName.should.eql(userName); // assert username
                    delete result.userName;
                    result.should.eql(triggerData);
                    done();
                });
        });

    });
    describe('GET /customers/{orgId}/sites/{siteId}/triggers/{triggerid}', function () {

        it('should fail to get trigger with incorrect trigger id', function (done) {
            let req = request.get(uri + '/DDDDD');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should get trigger details for a valid trigger id', function (done) {
            let req = request.get(uri + '/' + triggerId);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    let result = res.body;
                    delete result.triggerId;
                    delete result.orgId;
                    delete result.siteId;
                    delete result.userId;
                    delete result.createdOn;
                    delete result.lastUpdated;
                    delete result.isDeleted;
                    result.userName.should.eql(userName); // assert username
                    delete result.userName;
                    result.should.eql(triggerData);
                    done();
                });
        });

        it("should fail to update trigger with invalid trigger id", function (done) {
            let req = request.put(uri + '/YYYYY');
            agent.attachCookies(req);
            //Update trigger name
            const updatedTriggerData = triggerData;
            updatedTriggerData.triggerName = updatedTriggerName;
            req.send(updatedTriggerData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });

        });

        it("should fail to update with invalid payload", function (done) {
            let req = request.put(uri + '/' + triggerId);
            agent.attachCookies(req);
            const updatedTriggerData = {};
            req.send(updatedTriggerData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should update trigger with valid payload", function (done) {
            let req = request.put(uri + '/' + triggerId);
            agent.attachCookies(req);
            const updatedTriggerData = triggerData;
            updatedTriggerData.triggerName = "Updated mocha turnover";
            req.send(updatedTriggerData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    let result = res.body;
                    delete result.triggerId;
                    delete result.orgId;
                    delete result.siteId;
                    delete result.userId;
                    delete result.createdOn;
                    delete result.lastUpdated;
                    delete result.isDeleted;
                    result.userName.should.eql(userName); // assert username
                    delete result.userName;
                    result.should.eql(updatedTriggerData);
                    console.log(`Updated trigger ${triggerId} with name Updated mocha turnover`);
                    done();
                });
        });

        it("should fail to delete with invalid trigger id", function (done) {
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

        it("should delete trigger with valid trigger id", function (done) {
            let req = request.delete(uri + '/' + triggerId);
            agent.attachCookies(req);
            req
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    console.log(`Deleted trigger ${triggerId}`);
                    done();
                });
        });

        it('should fail to get trigger that just got deleted', function (done) {
            let req = request.get(uri + '/' + triggerId);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });
    });
});