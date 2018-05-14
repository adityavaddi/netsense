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

const uri = version + '/customers/'+orgId+'/sites/'+siteId+'/parking-what-if-analysis/tags/';
var parkingPolicyTagData = {
    name: "New Uber Policy Tag",
    description: "New Uber Parking Policy Tag"
};

let tagId = null;

describe('What-if Policy Tag', function () {
    describe('GET /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/tags', function () {

        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers/uberorg/sites/ubersite/tags')
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

        it("should fail to create a parking policy tag for invalid siteid", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ZZZZZ/parking-what-if-analysis/tags');
            agent.attachCookies(req);
            req.send(parkingPolicyTagData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should fail to create a parking policy tag for invalid paylod", function (done) {
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

        it("should create a parking policy tag using credentials", function (done) {
            var req = request.post(uri);
            agent.attachCookies(req);
            req.send(parkingPolicyTagData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    let result = res.body;
                    console.log('Created policy tag ' + result.tagId + ' with name:', res.body.name);
                    tagId = result.tagId;
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

        it('should get an empty list for all what-if parking policy tags for invalid orgid', function (done) {
            var req = request.get(version + '/customers/XXXXX/sites/ubersite/parking-what-if-analysis/tags');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(0);
                    done();
                });
        });

        it('should get all parking policy tags with login credentials', function (done) {
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    let result = res.body.filter(function (tag) {
                        return tag.tagId == tagId;
                    })[0];
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
    });

    describe('GET /customers/{orgid}/sites/{siteid}/parking-what-if-analysis/tags/{tagid}', function () {

        it("should fail to get what-if parking policy tag for incorrect policyid", function (done) {
            let req = request.get(uri + 'XXXXX');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should get what-if parking policy tag using credentials", function (done) {
            let get_req = request.get(uri + tagId);
            agent.attachCookies(get_req);
            get_req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    delete res.body.tagId;
                    delete res.body.orgId;
                    delete res.body.siteId;
                    delete res.body.createdOn;
                    delete res.body.lastUpdated;
                    delete res.body.isDeleted;
                    res.body.should.eql(parkingPolicyTagData);
                    done();
                });
        });

        it("should fail to update what-if parking policy tag for incorrect payload", function (done) {
            let update_req = request.put(uri + tagId);
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

        it("should fail to update what-if parking policy tag for incorrect policy id", function (done) {
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

        it("should update what-if parking policy tag with login credentials", function (done) {
            var updatedData = parkingPolicyTagData;
            updatedData.name = "Updated Uber What-if Policy Tag";
            let update_req = request.put(uri + tagId);
            agent.attachCookies(update_req);
            update_req.send(updatedData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    const result = res.body;
                    console.log('Updated What-if tag ' + tagId + ' with name:', result.name);
                    delete res.body.tagId;
                    delete res.body.orgId;
                    delete res.body.siteId;
                    delete res.body.createdOn;
                    delete res.body.lastUpdated;
                    delete res.body.isDeleted;
                    res.body.should.eql(updatedData);
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
            let delete_req = request.delete(uri + tagId);
            agent.attachCookies(delete_req);
            delete_req.set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    console.log('Deleted What-if Parking Policy Tag:', tagId);
                    should.not.exist(err);
                    done();
                });
        });

        it("should get not found error for invalid tagid", function (done) {
            var get_req = request.get(uri + "invalidtagid");
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