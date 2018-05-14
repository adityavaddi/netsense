/**
 * Created by brefsdal on 12/16/15.
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

const fixtureRequestData = {
    name: "My fixture",
    fixtureType: "Lamp",
    MaxPower0: "0",
    MaxPower10: "10XXX",
    MaxPower50: "50YYY",
    MaxPower100: "ZZZ",
    MinPower100: "100",
    MinPower50: "50",
    MinPower10: "10",
    MinPower0: "0",
    PowerDraw: "draw",
    nemasocket: "1",
    manufacturer: "Sensity",
    manufacturersku: "sensity-sku-1",
    description: "fixturedesc",
    MinimumLightLevelForFailureDetection: "10",
    BallastCost: "10",
    BulbCost: "5",
    LegacyPowerDraw: "50",
    DailyOperatingTime: "5",
};

describe('Fixtures', function () {
    describe('GET /customers/{orgid}/sites/{siteid}/fixtures', function () {
        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers/uberorg/sites/ubersite/fixtures')
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
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    csrfToken = helpers.getCsrfToken(res);
                    done();
                });

        });

        it("should fail to create fixture with invalid Min, Max power values", function(done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/fixtures');
            agent.attachCookies(req);
            req.send(fixtureRequestData)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(400)
                .end(function(err, res) {
                    done();
                });
        });

        it("should create a fixture using credentials", function (done) {
            var data = {
                name: "My fixture",
                fixtureType: "Lamp",
                MaxPower0: "0",
                MaxPower10: "10",
                MaxPower50: "50",
                MaxPower100: "100",
                MinPower100: "100",
                MinPower50: "50",
                MinPower10: "10",
                MinPower0: "0",
                PowerDraw: "draw",
                nemasocket: "1",
                manufacturer: "Sensity",
                manufacturersku: "sensity-sku-1",
                description: "fixturedesc",
                MinimumLightLevelForFailureDetection: "10",
                BallastCost: "10",
                BulbCost: "5",
                LegacyPowerDraw: "50",
                DailyOperatingTime: "5",
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/fixtures');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    delete result.fixtureid;
                    result.should.eql(data);
                    done();
                });
        });

        it("should fail to update fixture with invalid Min, Max power values", function(done) {
            var uri = version + "/customers/uberorg/sites/ubersite/fixtures";
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function(err, res) {
                    should.not.exist(err);
                    var fixtureid = res.body[0].fixtureid;
                    var updateReq = request.post(uri + "/" + fixtureid);
                    agent.attachCookies(updateReq);
                    updateReq.send(fixtureRequestData)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(400)
                        .end(function(err2, res2) {
                            done();
                        });
                });
        });

        it("should apply a fixture to a node", function (done) {
            var createNodereq = request.put(version + '/nodes');
            agent.attachCookies(createNodereq);
            var nodeid = "fixture-node1";
            var payload = { csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude,\nfixture-node1,unode-v2,uberorg,ubersite,44.4,41.2" };
            createNodereq.send(payload)
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    //var nodeid = res.body.nodeid;
                    var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/activate/' + nodeid);
                    agent.attachCookies(req);
                    req.send()
                        .set('Content-Type', 'application/json')
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        //.expect('Content-Type', /json/)
                        .expect(204)
                        .end(function (err, res) {
                            should.not.exist(err);

                            req = request.get(version + '/customers/uberorg/sites/ubersite/fixtures');
                            agent.attachCookies(req);
                            req.expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    var fixtureid = res.body[0].fixtureid;
                                    req = request.post(version + '/customers/uberorg/sites/ubersite/fixtures/' + fixtureid + '/assign/node/' + nodeid);
                                    agent.attachCookies(req);
                                    req.send()
                                        .set('Content-Type', 'application/json')
                                        .set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect(200)
                                        .end(function (err, res) {
                                            should.not.exist(err);
                                            res.body.should.eql({
                                                fixtureid: fixtureid,
                                                nodeid: nodeid
                                            });
                                            req = request.post(version + '/customers/uberorg/sites/ubersite/fixtures/' + fixtureid + '/assign/site');
                                            agent.attachCookies(req);
                                            req.send()
                                                .set('Content-Type', 'application/json')
                                                .set('Accept', 'application/json')
                                                .set('X-CSRF-Token', csrfToken)
                                                .expect(200)
                                                .end(function (err, res) {
                                                    should.not.exist(err);
                                                    res.body.should.eql({
                                                        fixtureid: fixtureid,
                                                        siteid: "ubersite"
                                                    });

                                                    // Check is assigned to node
                                                    var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/' + nodeid);
                                                    agent.attachCookies(req);
                                                    req.expect('Content-Type', /json/)
                                                        .expect(200)
                                                        .end(function (err, res) {
                                                            var result = res.body,
                                                                nfixtureid = result.fixtureid;
                                                            nfixtureid.should.eql(fixtureid);

                                                            done();
                                                        });
                                                });
                                        });
                                });
                        });
                });
        });

        it("should create a list of nodes ", function (done) {
            var createNodereq = request.put(version + '/nodes');
            agent.attachCookies(createNodereq);
            var nodeid = "fixture-node1-of-2";
            var payload = { csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude,\nfixture-node1-of-2,unode-v2,uberorg,ubersite,44.4,41.2" };
            createNodereq.send(payload)
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var nodeid1 = "fixture-node1-of-2";
                    var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/activate/' + nodeid1);
                    agent.attachCookies(req);
                    req.send()
                        .set('Content-Type', 'application/json')
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(204)
                        .end(function (err, res) {
                            should.not.exist(err);
                            var createNodereq = request.put(version + '/nodes');
                            agent.attachCookies(createNodereq);
                            var nodeid = "fixture-node2-of-2";
                            var payload = { csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude,\nfixture-node2-of-2,unode-v2,uberorg,ubersite,44.4,41.2" };
                            createNodereq.send(payload)
                                .set('X-CSRF-Token', csrfToken)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    var nodeid2 = "fixture-node2-of-2";
                                    req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/activate/' + nodeid2);
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
                        });
                });
        });

        it("should apply a fixture to nodes created", (done) => {
            var nodeid1 = "fixture-node1-of-2";
            var nodeid2 = "fixture-node2-of-2";
            var req = request.get(version + '/customers/uberorg/sites/ubersite/fixtures');
            agent.attachCookies(req);
            req.expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var fixtureid = res.body[0].fixtureid;
                    var nodeids = {
                        nodeList: [nodeid1, nodeid2]
                    };
                    var req = request.post(version + '/customers/uberorg/sites/ubersite/fixtures/' + fixtureid + '/assign/nodes/');
                    agent.attachCookies(req);
                    req.send(nodeids)
                        .set('Content-Type', 'application/json')
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);
                            res.body.nodeids.indexOf(nodeid1).should.not.eql(-1);
                            res.body.nodeids.indexOf(nodeid2).should.not.eql(-1);

                            // Check is assigned to node
                            var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/' + nodeid1);
                            agent.attachCookies(req);
                            req.expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    var result = res.body,
                                        nfixtureid = result.fixtureid;
                                    nfixtureid.should.eql(fixtureid);
                                    done();
                                });
                        });
                });
        })

        it("should apply a fixture to a group", function (done) {
            // Create node that belongs to a site but not to a group
            var createNodereq = request.put(version + '/nodes');
            agent.attachCookies(createNodereq);
            var nodeid = "fixture-node3";
            var payload = { csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude,\nfixture-node3,unode-v2,uberorg,ubersite,45.4,43.2" };
            createNodereq.send(payload)
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var nodeid0 = "fixture-node3";
                    var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/activate/' + nodeid0);
                    agent.attachCookies(req);
                    req.send()
                        .set('Content-Type', 'application/json')
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(204)
                        .end(function (err, res) {
                            should.not.exist(err);
                            var createNodereq = request.put(version + '/nodes');
                            agent.attachCookies(createNodereq);
                            var nodeid = "fixture-node2";
                            var payload = { csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude,\nfixture-node2,unode-v2,uberorg,ubersite,44.4,41.2" };
                            createNodereq.send(payload)
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    var nodeid = "fixture-node2";
                                    req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/activate/' + nodeid);
                                    agent.attachCookies(req);
                                    req.send()
                                        .set('Content-Type', 'application/json')
                                        .set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect(204)
                                        .end(function (err, res) {
                                            should.not.exist(err);
                                            // Find a fixture
                                            req = request.get(version + '/customers/uberorg/sites/ubersite/fixtures');
                                            agent.attachCookies(req);
                                            req.expect(200)
                                                .end(function (err, res) {
                                                    should.not.exist(err);
                                                    //  Create Group
                                                    var fixtureid = res.body[0].fixtureid;
                                                    var group = {
                                                        name: "My Fixture group",
                                                        type: "organizational",
                                                        nodeList: ["fixture-node2"]
                                                    };
                                                    req = request.post(version + '/customers/uberorg/sites/ubersite/groups');
                                                    agent.attachCookies(req);
                                                    req.send(group)
                                                        .set('Accept', 'application/json')
                                                        .set('X-CSRF-Token', csrfToken)
                                                        .expect('Content-Type', /json/)
                                                        .expect(200)
                                                        .end(function (err, res) {
                                                            should.not.exist(err);
                                                            // Assign fixture to a group
                                                            var groupid = res.body.groupid;
                                                            req = request.post(version + '/customers/uberorg/sites/ubersite/fixtures/' + fixtureid + '/assign/groups/' + groupid);
                                                            agent.attachCookies(req);
                                                            req.send()
                                                                .set('Content-Type', 'application/json')
                                                                .set('Accept', 'application/json')
                                                                .set('X-CSRF-Token', csrfToken)
                                                                .expect(200)
                                                                .end(function (err, res) {
                                                                    should.not.exist(err);
                                                                    res.body.should.eql({
                                                                        fixtureid: fixtureid,
                                                                        groupids: [groupid]
                                                                    });

                                                                    // Check is it assigned to a node
                                                                    var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/' + nodeid);
                                                                    agent.attachCookies(req);
                                                                    req.expect('Content-Type', /json/)
                                                                        .expect(200)
                                                                        .end(function (err, res) {
                                                                            var result = res.body;
                                                                            fixtureid.should.eql(result.fixtureid);

                                                                            // Check is it not assigned to other node
                                                                            var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/' + nodeid0);
                                                                            agent.attachCookies(req);
                                                                            req.expect('Content-Type', /json/)
                                                                                .expect(200)
                                                                                .end(function (err, res) {
                                                                                    var result = res.body;
                                                                                    should.not.exist(result.fixtureid);

                                                                                    // Delete group
                                                                                    req = request.delete(version + '/customers/uberorg/sites/ubersite/groups/' + groupid);
                                                                                    agent.attachCookies(req);
                                                                                    req.set('Accept', 'application/json')
                                                                                        .set('X-CSRF-Token', csrfToken)
                                                                                        .expect('Content-Type', /json/)
                                                                                        .expect(200)
                                                                                        .end(function (err, res) {
                                                                                            should.not.exist(err);
                                                                                            done();
                                                                                        });
                                                                                })
                                                                        })
                                                                });
                                                        });
                                                });
                                        });
                                });
                        });
                });
        });


        it("should get, update, and delete fixture using credentials", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/fixtures";
            var data = {
                name: "My fixture",
                fixtureType: "Lamp",
                MaxPower0: "0",
                MaxPower10: "10",
                MaxPower50: "50",
                MaxPower100: "100",
                MinPower100: "100",
                MinPower50: "50",
                MinPower10: "10",
                MinPower0: "0",
                PowerDraw: "draw",
                nemasocket: "1",
                manufacturer: "Sensity",
                manufacturersku: "sensity-sku-1",
                description: "fixturedesc",
                MinimumLightLevelForFailureDetection: "10",
                BallastCost: "10",
                BulbCost: "5",
                LegacyPowerDraw: "50",
                DailyOperatingTime: "5",
            };
            var nodeid = 'fixture-node1';
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var fixtureid = res.body[0].fixtureid;
                    delete res.body[0].fixtureid;
                    res.body[0].should.eql(data);
                    var updated = {
                        name: "My fixture updated",
                        fixtureType: "Lamp",
                        MaxPower0: "10",
                        MaxPower10: "100",
                        MaxPower50: "500",
                        MaxPower100: "1000",
                        MinPower100: "1000",
                        MinPower50: "500",
                        MinPower10: "100",
                        MinPower0: "10",
                        PowerDraw: "draw x2",
                        nemasocket: "1",
                        manufacturer: "Sensity",
                        manufacturersku: "sensity-sku-1",
                        description: "fixturedesc",
                        MinimumLightLevelForFailureDetection: "100",
                        BallastCost: "15",
                        BulbCost: "10",
                        LegacyPowerDraw: "100",
                        DailyOperatingTime: "0",
                    };
                    var req2 = request.post(uri + "/" + fixtureid);
                    agent.attachCookies(req2);
                    req2.send(updated)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res2) {
                            should.not.exist(err);
                            var result = res2.body;
                            delete result.fixtureid;
                            delete result.site;
                            delete result.groups;
                            result.should.eql(updated);

                            var delete_req = request.delete(uri + "/" + fixtureid);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect(204)
                                .end(function (err, res) {
                                    should.not.exist(err);

                                    var get_req = request.get(uri + "/" + fixtureid);
                                    agent.attachCookies(get_req);
                                    get_req.set('Accept', 'application/json')
                                        .expect('Content-Type', /json/)
                                        .expect(404)
                                        .end(function (err, res) {
                                            should.not.exist(err);

                                            var get_all_req = request.get(uri);
                                            agent.attachCookies(get_all_req);
                                            get_all_req.set('Accept', 'application/json')
                                                .expect(200)
                                                .end(function (err, res) {
                                                    should.not.exist(err);

                                                    // Check is removed from node
                                                    var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/' + nodeid);
                                                    agent.attachCookies(req);
                                                    req.expect('Content-Type', /json/)
                                                        .expect(200)
                                                        .end(function (err, res) {
                                                            var result = res.body,
                                                                nodeid = result.nodeid;
                                                            should.not.exist(result.fixtureid);
                                                            done();
                                                        });
                                                });
                                        });
                                });
                        });
                });
        });

        it("should access fixtures with login credentials", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/fixtures');
            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(0);
                    done();
                });
        });
    });

    describe('Cleanup Nodes ', function () {
        it('should clean up nodes after', function (done) {
            var nodeids = ['fixture-node1', 'fixture-node2', 'fixture-node3', 'fixture-node1-of-2', 'fixture-node2-of-2'];
            var nodeid = null;
            var dirty = nodeids.length;
            while (nodeid = nodeids.shift()) {
                var uri = version + '/customers/uberorg/sites/ubersite/nodes';
                var delete_req = request.delete(uri + "/" + nodeid);
                agent.attachCookies(delete_req);
                delete_req
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect(204)
                    .end(function (err, res) {
                        should.not.exist(err);
                        if (!(--dirty))
                            done();
                    });
            };
        });
    });
});