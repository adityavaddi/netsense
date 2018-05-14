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

describe('Groups', function () {
    describe('GET /customers/{orgid}/sites/{siteid}/groups', function () {
        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers/uberorg/sites/ubersite/groups')
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

        var mochaGroupId = null;

        it("should fail to create a group without name using credentials", function (done) {
            var data = {
                nodeList: [""],
                name: "",
                type: "organizational",
                description: " "
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/groups');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    result.message.should.eql("Validation errors");
                    done();
                });
        });

        it("should create a group using credentials", function (done) {
            var data = {
                nodeList: [],
                name: "My group",
                type: "organizational",
                description: "My test group"
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/groups');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    mochaGroupId = result.groupid;
                    delete result.groupid;
                    delete result.pdprofiles;
                    delete result.dhprofiles;
                    delete result.schedules;
                    result.should.eql(data);
                    done();
                });
        });

        it("should create a node using credentials", function (done) {
            var data = {
                name: "ub2",
                nodeid: "ubernode2",
                latitude: "37.325944790869585",
                longitude: "-121.94748993526446",
                ip: "192.168.1.1",
                model: "unode-v2",
                building: "2",
                level: "1",
                meshId: "Mxeralux1",
                note: "Node Note",
                baseStation: "Mac address",
                publicKey: "public key",
                signature: "signature",
                remoteNetwork: "XeraL",
                bssid: "bssid",
                configToken: "token",
                softwareVersion: "e16b568",
                mfgDate: "2014-03-06T22:15:12.074Z",
                circuit: "1"
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes');
            agent.attachCookies(req);
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .end(function (err, res) {
                    if (res.status === 400) {
                        var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/ubernode2/assign');
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
                    } else {
                        should.not.exist(err);
                        done();
                    }

                });
        });

        it("should create a node using credentials", function (done) {
            var data = {
                name: "ub3",
                nodeid: "ubernode3",
                latitude: "37.325944790869585",
                longitude: "-121.94748993526446",
                ip: "192.168.1.1",
                model: "unode-v2",
                building: "2",
                level: "1",
                meshId: "Mxeralux1",
                note: "Node Note",
                baseStation: "Mac address",
                publicKey: "public key",
                signature: "signature",
                remoteNetwork: "XeraL",
                bssid: "bssid",
                configToken: "token",
                softwareVersion: "e16b568",
                mfgDate: "2014-03-06T22:15:12.074Z",
                circuit: "1"
            };
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes');
            agent.attachCookies(req);
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .end(function (err, res) {
                    if (res.status === 400) {
                        var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/ubernode3/assign');
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
                    } else {
                        should.not.exist(err);
                        done();
                    }
                });
        });

        it("should add a node to group using credentials", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/groups/' + mochaGroupId + '/add/ubernode2');
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

        it("should add a node to group using credentials", function (done) {
            var req = request.post(version + "/customers/uberorg/sites/ubersite/groups/" + mochaGroupId + "/add/ubernode3");
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

        it("should check group membership ", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/groups/' + mochaGroupId);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.nodeList.should.eql(["ubernode2", "ubernode3"]);
                    done();
                });
        });

        it("should check node membership ", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/ubernode3');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    const mygroupName = "My group";
                    const siteLightningGroupName = "Site Lighting Group";
                    result.groupnamelist[0].indexOf(mygroupName).should.not.eql(-1);
                    result.groupnamelist[1].indexOf(siteLightningGroupName).should.not.eql(-1);
                    done();
                });
        });

        it("should remove a node from group using credentials", function (done) {
            var req = request.post(version + "/customers/uberorg/sites/ubersite/groups/" + mochaGroupId + "/remove/ubernode2");
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

        it("should check group membership ", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/groups/' + mochaGroupId);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.nodeList.should.eql(["ubernode3"]);
                    done();
                });
        });

        it("should fail to modify site lighting group membership", function (done) {
            var grpid = 'ubersitelightinggroup';
            var req = request.get(version + '/customers/uberorg/sites/ubersite/groups/' + grpid);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.nodeList.indexOf("ubernode2").should.not.eql(-1);
                    res.body.nodeList.indexOf("ubernode3").should.not.eql(-1);
                    var allnodes = res.body.nodeList;
                    var data = res.body;
                    data.name = "Site Lighting Group was modified";
                    data.description = "My site-lighting group";

                    // Try to add one and remove one
                    //data.nodeList = ["ubernode3", "ubernode4"];
                    data.nodeList = allnodes.slice(0); // clone
                    data.nodeList.pop();
                    data.nodeList.push("ubernode4");
                    var uri = version + '/customers/uberorg/sites/ubersite/groups' + '/' + grpid;
                    var req = request.post(uri);
                    agent.attachCookies(req);
                    req.send(data)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(400)
                        .end(function (err, res) {
                            should.not.exist(err);
                            var result = res.body;
                            should.exist(result.message);

                            // Try to remove one
                            //data.nodeList = ["ubernode2"];
                            data.nodeList = allnodes.slice(0); // clone
                            data.nodeList.pop();
                            var req = request.post(uri);
                            agent.attachCookies(req);
                            req.send(data)
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(400)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    var result = res.body;
                                    should.exist(result.message);

                                    // Try to add one
                                    //data.nodeList = ["ubernode2", "ubernode3", "ubernode4"];
                                    data.nodeList = allnodes.slice(0); // clone
                                    data.nodeList.push("ubernode4");
                                    var req = request.post(uri);
                                    agent.attachCookies(req);
                                    req.send(data)
                                        .set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect('Content-Type', /json/)
                                        .expect(400)
                                        .end(function (err, res) {
                                            should.not.exist(err);
                                            var result = res.body;
                                            should.exist(result.message);

                                            // Modify some other data
                                            data.nodeList = allnodes.slice(0); // clone
                                            var req = request.post(uri);
                                            agent.attachCookies(req);
                                            req.send(data)
                                                .set('Accept', 'application/json')
                                                .set('X-CSRF-Token', csrfToken)
                                                .expect('Content-Type', /json/)
                                                .expect(200)
                                                .end(function (err, res) {
                                                    should.not.exist(err);
                                                    var result = res.body;
                                                    should.not.exist(result.message);
                                                    result.name.should.eql("Site Lighting Group was modified");
                                                    done();
                                                });
                                        });
                                });
                        });
                });
        });

        it("should get, update, and delete group using credentials", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/groups";
            var data = {
                nodeList: ["ubernode3"],
                name: "My group",
                type: "organizational",
                description: "My test group"
            };
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var result = res.body.filter(function (group) { return group.groupid == mochaGroupId; })[0];
                    delete result.groupid;
                    delete result.pdprofiles;
                    delete result.etdhprofiles;
                    delete result.dhprofiles;
                    delete result.schedules;
                    result.should.eql(data);
                    var updated = {
                        nodeList: ['ubernode2'],
                        name: "My group updated",
                        type: "organizational",
                        description: "My test group"
                    };
                    var req2 = request.post(uri + "/" + mochaGroupId);
                    agent.attachCookies(req2);
                    req2.send(updated)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res2) {
                            var result = res2.body;
                            delete result.groupid;
                            delete result.pdprofiles;
                            delete result.etdhprofiles;
                            delete result.dhprofiles;
                            delete result.schedules;
                            delete result.nodes;
                            result.should.eql(updated);

                            var delete_req = request.delete(uri + "/" + mochaGroupId);
                            agent.attachCookies(delete_req);
                            delete_req.set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {

                                    var get_req = request.get(uri + "/" + mochaGroupId);
                                    agent.attachCookies(get_req);
                                    get_req.set('Accept', 'application/json')
                                        .expect('Content-Type', /json/)
                                        .expect(404)
                                        .end(function (err, res) {
                                            should.not.exist(err);

                                            var get_all_req = request.get(uri);
                                            agent.attachCookies(get_all_req);
                                            get_all_req.set('Accept', 'application/json')
                                                .expect('Content-Type', /json/)
                                                .expect(204)
                                                .end(function (err, res) {
                                                    //res.body.should.eql([]);
                                                    done();
                                                });
                                        });
                                });
                        });
                });
        });

        describe('Cleanup Nodes', function () {
            it('should clean up nodes after', function (done) {
                var nodeids = ['ubernode2', 'ubernode3'];
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

            /* it('should clean up groups after', function (done) {
                var uri = version + "/customers/uberorg/sites/ubersite/groups";
                var getReq = request.get(uri);
                agent.attachCookies(getReq);
                getReq.set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect('Content-Type', /json/)
                    .expect(200)
                    .end(function (err, res) {
                        var result = res.body;
                        if (result.length > 0) {
                            var groupIds = [];
                            //result.forEach(e => groupIds.push(e.groupid));
                            var groupIds = result.filter(group => group.name != 'Site Lighting Group');
                            var group = null;
                            var dirty = groupIds.length;
                            while (group.groupid = groupIds.shift()) {
                                console.log('Deleted Group:', group.groupid);
                                var delete_req = request.delete(uri + "/" + group.groupid);
                                agent.attachCookies(delete_req);
                                delete_req.set('Accept', 'application/json')
                                    .set('X-CSRF-Token', csrfToken)
                                    .expect(204)
                                    .end(function (err, res) {
                                        should.not.exist(err);
                                        if (!(--dirty))
                                            done();
                                    });
                            }
                        } else done();
                    });
            }); */
        });
    });
});