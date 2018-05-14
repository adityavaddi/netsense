/**
 * Created by brefsdal on 12/16/15.
 */
"use strict";
var server_url = process.env.stack_url;
var should = require('should');
var request = require('supertest')(server_url);
var superagent = require('superagent');
var path = require('path');
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
var alertid1 = '',
    alertid2 = '',
    alertid3 = '',
    videoAlertId1 = '',
    videoAlertId2 = '';

describe('Nodes', function () {
    describe('GET /customers/<orgid>/sites/<siteid>/nodes', function () {
        it("should clean up nodes before", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/nodes";
            var getReq = request.get(uri);
            agent.attachCookies(getReq);
            getReq.set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var result = res.body;
                    if (result.length > 0) {
                        var nodeIds = [];
                        result.forEach(e => nodeIds.push(e.nodeid));
                        var nodeId = null;
                        var dirty = nodeIds.length;
                        while (nodeId = nodeIds.shift()) {
                            //console.log('Deleted Node:', nodeId);
                            var delete_req = request.delete(uri + "/" + nodeId);
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
        });

        it('should fail without login credentials', function (done) {
            request
                .get(version + '/customers/uberorg/sites/ubersite/nodes')
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

        it("should create an empty node (N013341c0) using credentials", function (done) {

            var data = {
                "nodeid": "N013341c0",
                "model": "unode-v2"
            };
            var req = request.post(version + '/nodes/N013341c0');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    result.should.eql(data);
                    done();
                });
        });

        it("should assign a node (N013341c0) using credentials", function (done) {

            var nodeid = "N013341c0";
            var uri = version + '/customers/uberorg/sites/ubersite/nodes';
            var req = request.post(uri + '/' + nodeid + '/assign');
            agent.attachCookies(req);
            req //.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);

                    // Fail to delete with wrong orgid
                    var furi = version + '/customers/uberorgxyz/sites/ubersite/nodes';
                    var delete_req = request.delete(furi + "/" + nodeid);
                    agent.attachCookies(delete_req);
                    delete_req
                        .set('Content-Type', 'application/json')
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(404)
                        .end(function (err, res) {

                            // Fail to delete with wrong siteid
                            var furi = version + '/customers/uberorg/sites/ubersitexyz/nodes';
                            var delete_req = request.delete(furi + "/" + nodeid);
                            agent.attachCookies(delete_req);
                            delete_req
                                .set('Content-Type', 'application/json')
                                .set('Accept', 'application/json')
                                .set('X-CSRF-Token', csrfToken)
                                .expect('Content-Type', /json/)
                                .expect(404)
                                .end(function (err, res) {

                                    // Delete
                                    var delete_req = request.delete(uri + "/" + nodeid);
                                    agent.attachCookies(delete_req);
                                    delete_req
                                        .set('Content-Type', 'application/json')
                                        .set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect('Content-Type', /json/)
                                        .expect(204)
                                        .end(function (err, res) {
                                            var get_req = request.get(uri + "/" + nodeid);
                                            agent.attachCookies(get_req);
                                            get_req
                                                .set('Content-Type', 'application/json')
                                                .set('Accept', 'application/json')
                                                .expect('Content-Type', /json/)
                                                .expect(404)
                                                .end(function (err, res) {
                                                    should.not.exist(err);

                                                    // getAll
                                                    var get_all_req = request.get(uri);
                                                    agent.attachCookies(get_all_req);
                                                    get_all_req
                                                        .set('Content-Type', 'application/json')
                                                        .set('Accept', 'application/json')
                                                        //.expect('Content-Type', /json/)
                                                        .expect(204)
                                                        .end(function (err, res) {
                                                            res.body.filter(function (elem) {
                                                                return elem.nodeid === 'N013341c0';
                                                            }).should.eql([]);

                                                            done();
                                                        });
                                                });
                                        });
                                });
                        });
                });
        });

        it("should create N013341e0 node using credentials", function (done) {
            var data = {
                name: "Test Node",
                nodeid: "N013341e0",
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
                        var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/N013341e0/assign');
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

        it("should re-apply a schedule using credentials", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/N013341e0/schedule_push');
            agent.attachCookies(req);
            req.send()
                .set('Accept', 'application/json')
                .set('Content-Type', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should deactivate N013341e0 node using credentials", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/deactivate/N013341e0');
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

        it("should activate N013341e0 node using credentials", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/activate/N013341e0');
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

        it('should get minimal subset of fields for all nodes (just N013341e0)', function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/minnodes');
            var data = {
                level: '1',
                latitude: '37.325944790869585',
                name: 'Test Node',
                model: 'unode-v2',
                nodeid: 'N013341e0',
                building: '2',
                longitude: '-121.94748993526446'
            };
            agent.attachCookies(req);
            req.send()
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    var nodeDetails;
                    result.forEach(function(node) {
                        if(node.nodeid === 'N013341e0') {
                            nodeDetails = node;
                        }
                    });
                    delete nodeDetails.fixtureid;
                    nodeDetails.should.eql(data);
                    done();
                });
        });

        it("should get and update N013341e0 node using credentials", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/nodes";
            var data = {
                name: "Test Node",
                nodeid: "N013341e0",
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
                circuit: "1",
                groupidlist: ['ubersitelightinggroup'],
                //groupnamelist: ['Site Lighting Group'],    // Dependency from previous test
                scheduleid: 'uberschedule',
                schedulename: 'Default Schedule'
            };
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var nodeid = "N013341e0";
                    var result = res.body.filter(function (node) {
                        return node.nodeid == nodeid;
                    })[0];

                    data.time_zone = 'America/Los_Angeles';
                    delete result.fixtureid;
                    delete result.groupnamelist;    // Dependency from previous test
                    result.should.eql(data);

                    //Update
                    var updated = {
                        nodeid: nodeid,
                        name: 'Test Node Updated',
                        building: "2",
                        level: "3",
                        latitude: "37.325944790869585",
                        longitude: "-121.94748993526446",
                        ip: "192.168.1.2",
                        model: "unode-v2",
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
                    var req2 = request.post(uri + "/" + nodeid);
                    agent.attachCookies(req2);
                    req2.send(updated)
                        .set('Content-Type', 'application/json')
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res2) {
                            var result = res2.body;
                            delete result.groupidlist;
                            delete result.groupnamelist;
                            delete result.schedulename;
                            delete result.scheduleid;
                            delete result.fixtureid;
                            updated.time_zone = 'America/Los_Angeles';
                            updated.country_code = 'US';
                            result.should.eql(updated);

                            done();
                        });
                });
        });

        it("should update back to original values and delete", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/nodes";
            var nodeid = "N013341e0";
            //Update
            var updated = {
                nodeid: nodeid,
                name: 'Test Node',
                building: "2",
                level: "1",
                latitude: "37.325944790869585",
                longitude: "-121.94748993526446",
                ip: "192.168.1.1",
                model: "unode-v2",
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
            var req2 = request.post(uri + "/" + nodeid);
            agent.attachCookies(req2);
            req2.send(updated)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res2) {
                    var result = res2.body;
                    delete result.groupidlist;
                    delete result.groupnamelist;
                    delete result.schedulename;
                    delete result.scheduleid;
                    delete result.fixtureid;
                    updated.time_zone = 'America/Los_Angeles';
                    updated.country_code = 'US';
                    result.should.eql(updated);

                    // Delete
                    var delete_req = request.delete(uri + "/" + nodeid);
                    agent.attachCookies(delete_req);
                    delete_req
                        .set('Content-Type', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(204)
                        .end(function (err, res) {
                            should.not.exist(err);
                            done();
                        });
                });
        });

        it("should create a Carrier Pigeon node (N034452d1) using credentials", function (done) {

            var data = {
                name: "Carrier Pigeon test node",
                nodeid: "N034452d1",
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
                circuit: "1",
                apn: "apn",
                iccid: "iccid",
                imei: "imei",
                imsi: "imsi"
            };
            var uri = version + '/customers/uberorg/sites/ubersite/nodes';
            var req = request.post(uri);
            agent.attachCookies(req);
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(400)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;

                    data['model'] = 'unode-v5';
                    var uri = version + '/customers/uberorg/sites/ubersite/nodes';
                    var req = request.post(uri);
                    agent.attachCookies(req);
                    req.send(data)
                        .set('Content-Type', 'application/json')
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        //.expect(200)
                        .end(function (err, res) {
                            if (res.status === 400) {
                                console.log('Node already exist');
                                done();
                            } else {
                                should.not.exist(err);
                                var result = res.body;

                                data.time_zone = 'America/Los_Angeles';
                                result.should.eql(data);

                                var nodeid = result.nodeid;

                                var get_req = request.get(uri + "/" + nodeid);
                                agent.attachCookies(get_req);
                                get_req
                                    .set('Content-Type', 'application/json')
                                    .set('Accept', 'application/json')
                                    .expect('Content-Type', /json/)
                                    .expect(200)
                                    .end(function (err, res) {
                                        should.not.exist(err);
                                        result = res.body;

                                        delete result.groupidlist;
                                        delete result.groupnamelist;
                                        delete result.schedulename;
                                        delete result.scheduleid;
                                        delete result.configStatus;

                                        data.country_code = 'US';

                                        result.should.eql(data);

                                        data.apn += "2";
                                        var update_req = request.post(uri + "/" + nodeid);
                                        agent.attachCookies(update_req);
                                        update_req.send(data)
                                            .set('Content-Type', 'application/json')
                                            .set('Accept', 'application/json')
                                            .set('X-CSRF-Token', csrfToken)
                                            .expect('Content-Type', /json/)
                                            .expect(200)
                                            .end(function (err, res) {
                                                should.not.exist(err);
                                                result = res.body;
                                                delete result.groupidlist;
                                                delete result.groupnamelist;
                                                delete result.schedulename;
                                                delete result.scheduleid;
                                                delete result.configStatus;
                                                data.time_zone = 'America/Los_Angeles';
                                                result.should.eql(data);

                                                get_req = request.get(uri + "/" + nodeid);
                                                agent.attachCookies(get_req);
                                                get_req
                                                    .set('Content-Type', 'application/json')
                                                    .set('Accept', 'application/json')
                                                    .expect('Content-Type', /json/)
                                                    .expect(200)
                                                    .end(function (err, res) {
                                                        should.not.exist(err);
                                                        result = res.body;
                                                        delete result.groupidlist;
                                                        delete result.groupnamelist;
                                                        delete result.schedulename;
                                                        delete result.scheduleid;
                                                        delete result.configStatus;
                                                        result.should.eql(data);
                                                        var delete_req = request.delete(uri + "/" + nodeid);
                                                        agent.attachCookies(delete_req);
                                                        delete_req
                                                            .set('Content-Type', 'application/json')
                                                            .set('Accept', 'application/json')
                                                            .set('X-CSRF-Token', csrfToken)
                                                            .expect('Content-Type', /json/)
                                                            .expect(204)
                                                            .end(function (err, res) {

                                                                done();
                                                            });
                                                    });
                                            });
                                    });
                            }
                        });
                });
        });


        it("should access nodes with login credentials", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes');
            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.should.eql([]);
                    res.body.length.should.eql(0);
                    done();
                });
        });

        it("should add two empty nodes", function (done) {
            var req = request.post(version + '/nodes');
            agent.attachCookies(req);
            req
                .set('X-CSRF-Token', csrfToken)
                .attach('csvNodeList', path.resolve(__dirname, 'nodesempty.csv'))
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should assign two nodes", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/assign');
            agent.attachCookies(req);
            req
                .set('X-CSRF-Token', csrfToken)
                .attach('csvNodeList', path.resolve(__dirname, 'nodesassign.csv'))
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    setTimeout(function () {
                        // Give it some time to apply
                        done();
                    }, 500);
                });
        });

        it("should add nodes in bulk", function (done) {
            var req = request.put(version + '/nodes');
            agent.attachCookies(req);
            var payload = { csvNodeList: "nodeid,model,orgid,siteid,latitude,longitude\nmytestnodeid001,unode-v4,uberorg,ubersite,44.4,41.4\nmytestnodeid002,unode-v4,uberorg,ubersite,44.4,41.2" };
            req.send(payload)
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("get all lost and found nodes", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/_nosite_/nodes');
            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("get all lost and found node statuses", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/_nosite_/node_status');
            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should activate mytestnodeid001 node using credentials", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/activate/mytestnodeid001');
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

        it("should cold reset a node", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/deviceaction/0');
            agent.attachCookies(req);
            var payload = {
                nodeList: ["mytestnodeid001"]
            };
            req.send(payload)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should activate mytestnodeid002 node using credentials", function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/nodes/activate/mytestnodeid002');
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

        it('it should fail to override light level for a unknown node', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/lightcontrol/node/mytestnodeid003');
            agent.attachCookies(req);
            var data = {
                level: 50,
                timeout: 30,
                clear: false
            };
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

        it('it should override light level for a single node', function (done) {
            var req = request.post(version + '/customers/uberorg/sites/ubersite/lightcontrol/node/mytestnodeid002');
            agent.attachCookies(req);
            var data = {
                level: 50,
                timeout: 30,
                clear: false
            };
            req.send(data)
                .set('Content-Type', 'application/json')
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should get light status from a node", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/mytestnodeid002/light_status');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.policy.should.eql("override");
                    done();
                });
        });

        it("should access nodes with login credentials", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes');
            agent.attachCookies(req);
            req
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.length.should.eql(4);
                    done();
                });

        });

        it("should get sensor samples from a node", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/mytestnodeid002/sensors/RF/date/2017-03-07T14:07:40.00Z/limit/10');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.datapoints.length.should.eql(2);
                    done();
                });

        });

        it("should get sensor samples from a node for a specified period", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/mytestnodeid002/sensors/RF/from/2017-03-07T02:07:01.00Z/to/2017-03-12T14:07:41.00Z/limit/100');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.datapoints.length.should.eql(14);
                    done();
                });

        });

        it("should get energy sensor samples from a node for a specified period", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/mytestnodeid002/sensors/energy/from/2016-03-12T23:20:50.52Z/to/2016-03-12T23:20:55.52Z/limit/10');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });

        });

        it("should get energy sensor samples from a site for a specified period", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/all/sensors/energy/from/2016-03-12T23:20:50.52Z/to/2016-03-12T23:20:55.52Z/limit/10');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should get energy sensor aggregations per 15min from a node ", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/sensors/energy/from/2016-03-12T23:20:50.52Z/to/2016-03-12T23:20:55.52Z/limit/10/period/15min');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });

        });

        it("should get energy sensor aggregations per 15min from a site ", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/all/sensors/energy/from/2016-03-12T23:20:50.52Z/to/2016-03-12T23:20:55.52Z/limit/10/period/15min');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should get connection status from a node", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/nodes/mytestnodeid002/connection_status');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });


        it("should access nodes with login credentials", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/#/nodes');
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    //res.body.length.should.eql(0);
                    done();
                });

        });

        it("should add two empty nodes", function (done) {
            var req = request.post(version + '/nodes');
            agent.attachCookies(req);
            req
                .set('X-CSRF-Token', csrfToken)
                .attach('csvNodeList', path.resolve(__dirname, 'nodesempty.csv'))
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should create a video node (N2flcnqNODE) using credentials', function (done) {
            var data = { "csvNodeList": "nodeid,name,model,orgid,siteid\nN2flcnqNODE,N2flcnqNODE,falcon-q,uberorg,ubersite" },
                req = request.put(version + '/nodes');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    delete result.time_zone;
                    done();
                });
        });

        it("should get connection status with empty alerts", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/node_status');
            agent.attachCookies(req);
            req.expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    for (var i = 0; i < res.body.length; i++) {
                        var status = res.body[i];
                        var nodeid = status['nodeid'];
                        // Check returned alerts for node
                        if (nodeid == 'mytestnodeid003' || nodeid == 'mytestnodeid004' || nodeid == 'N2flcnqNODE') {
                            status.alerts.length.should.eql(0);
                        }
                    }
                    done();
                });
        });

        it("should create 3 core node alerts and 2 video node alerts", function (done) {
            // Create some core node alerts
            var adata = {
                name: "DeviceAlarm",
                nodeid: "mytestnodeid003",
                orgid: "uberorg",
                type: "HWFail_EEPROM",
                category: "Sensor",
                severity: "Major",
                msg: "Up"
            }
            var url = version + '/customers/uberorg/sites/ubersite/alerts';
            var req = request.post(url);
            agent.attachCookies(req);
            req.send(adata)
                .set('X-CSRF-Token', csrfToken)
                .expect(200).end(function (err, res) {
                    should.not.exist(err);
                    alertid1 = res.body.alertid;

                    adata['type'] = "HWFail_NIGHTHAWK";
                    var req = request.post(url);
                    agent.attachCookies(req);
                    req.send(adata)
                        .set('X-CSRF-Token', csrfToken)
                        .expect(200).end(function (err, res) {
                            should.not.exist(err);
                            alertid2 = res.body.alertid;

                            adata['nodeid'] = "mytestnodeid004";
                            var req = request.post(url);
                            agent.attachCookies(req);
                            req.send(adata)
                                .set('X-CSRF-Token', csrfToken)
                                .expect(200).end(function (err, res) {
                                    should.not.exist(err);
                                    alertid3 = res.body.alertid;

                                    // Create some core node alerts
                                    var videoAlertdata = {
                                        name: "DeviceAlarm",
                                        nodeid: "N2flcnqNODE",
                                        orgid: "uberorg",
                                        type: "HWFail_EEPROM",
                                        category: "Sensor",
                                        severity: "Major",
                                        msg: "Up",
                                        isVideoNode: true
                                    }
                                    var url = version + '/customers/uberorg/sites/ubersite/alerts';
                                    var req = request.post(url);
                                    agent.attachCookies(req);
                                    req.send(videoAlertdata)
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect(200).end(function (err, res) {
                                            should.not.exist(err);
                                            videoAlertId1 = res.body.alertid;

                                            videoAlertdata['type'] = "HWFail_NIGHTHAWK";
                                            var req = request.post(url);
                                            agent.attachCookies(req);
                                            req.send(videoAlertdata)
                                                .set('X-CSRF-Token', csrfToken)
                                                .expect(200).end(function (err, res) {
                                                    should.not.exist(err);
                                                    videoAlertId2 = res.body.alertid;
                                                    done();
                                                });
                                        });
                                });
                        });
                });
        });

        it("should fail to get nodes connection status with incorrect siteid", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/XXXXX/node_status');
            agent.attachCookies(req);
            req.expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it("should get core and video nodes connection status with alerts", function (done) {
            var req = request.get(version + '/customers/uberorg/sites/ubersite/node_status');
            agent.attachCookies(req);
            req.expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    for (var i = 0; i < res.body.length; i++) {
                        var status = res.body[i];
                        // Check returned alerts for node
                        if (status['nodeid'] == 'mytestnodeid003') {
                            status.alerts.length.should.eql(2);
                        } else if (status['nodeid'] == 'mytestnodeid004') {
                            status.alerts.length.should.eql(1);
                        } else if (status['nodeid'] == 'N2flcnqNODE') {
                            status.alerts.length.should.eql(2);
                        }
                    }
                    done();
                });
        });
    });

    // --- Cleanup ---
    describe('Clean up alerts, node', function () {
        it("should clean up alerts after", function (done) {
            var alertIds = [alertid1, alertid2, alertid3, videoAlertId1, videoAlertId2];
            var alertId = null;
            var dirty = alertIds.length;
            while (alertId = alertIds.shift()) {
                var uri = version + '/customers/uberorg/sites/ubersite/alerts';
                var delete_req = request.delete(uri + "/" + alertId);
                agent.attachCookies(delete_req);
                delete_req
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        if (!(--dirty))
                            done();
                    });
            }
        });

        it("should clean up nodes after", function (done) {
            var uri = version + "/customers/uberorg/sites/ubersite/nodes";
            var getReq = request.get(uri);
            agent.attachCookies(getReq);
            getReq.set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var result = res.body;
                    if (result.length > 0) {
                        var nodeIds = [];
                        result.forEach(e => nodeIds.push(e.nodeid));
                        var nodeId = null;
                        var dirty = nodeIds.length;
                        while (nodeId = nodeIds.shift()) {
                            //console.log('Deleted Node:', nodeId);
                            var delete_req = request.delete(uri + "/" + nodeId);
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
        });
    });
});