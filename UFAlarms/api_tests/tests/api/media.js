"use strict";
var server_url = process.env.stack_url;
//var server_url = 'https://netsense-drajput.sensity.com';
var should = require('should');
var request = require('supertest')(server_url);
var msgpack = require('msgpack5')();
var superagent = require('superagent');
var agent = superagent.agent();
var version = '/v3.0';
var mqtt = require('mqtt');
var node_id = "testsicmochanode"; // ?? simulator?
var stream = require('stream');
var configManager = require('kea-config');
configManager.setup('./config/');

describe('Media Service', function () {
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
                done();
            });
    });

    it('should fail SS without login credentials', function (done) {
        request
            .get('/streamv1/uberorg/ubersite/+/LoginReq')
            .set('Accept', 'application/json')
            .expect('Content-Type', /json/)
            .expect(403)
            .end(function (err, res) {
                should.not.exist(err);
                res.body.should.eql({ error: true, message: 'Please log in' });
                done();
            });
    });

    it('should fail SS without valid orgid and siteid', function (done) {
        var reqss = request.get('/streamv1/uberorgx/ubersitex/testsicmochanode/LoginReq');
        agent.attachCookies(reqss);
        reqss
            .set('Accept', 'application/json')
            .expect('Content-Type', /json/)
            .expect(403)
            .end(function (err, res) {
                should.not.exist(err);
                res.body.should.eql({ error: true, message: 'Not allowed to access requested org' });
                request
                    .get('/streamv1/uberorg/ubersitex/+/LoginReq')
                    .set('Accept', 'application/json')
                    .expect('Content-Type', /json/)
                    .expect(403)
                    .end(function (err, res) {
                        should.not.exist(err);
                        res.body.should.eql({ error: true, message: 'Please log in' });
                        done();
                    });
            });
    });

    /*  Enable after orchestration enabled for media-service:
    
        it("should fail to get a still image from a node without login", function (done) {
            var data = { "imgTS": "2018-01-09T08:05:43.258Z", "imgFormat": "jpg" } ;
            request
                .post(version + '/customers/uberorg/sites/ubersite/node/testsicmochanode/Media/img/0/1/L')
                .send(data)
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    res.body.should.eql({ error: true, message: 'Please log in' });
                    done();
                });
            });
    
        it("should get a still image from a node", function (done) {
            var data = { "imgTS": "2018-01-09T08:05:43.258Z", "imgFormat": "jpg" } ;
            var req = request.post(version + '/customers/uberorg/sites/ubersite/node/testsicmochanode/Media/img/0/1/L');
                agent.attachCookies(req);
                req.send(data)
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    console.log('Received Still Image');
                    should.not.exist(err);
                    res.body.datapoints.length.should.eql(1);
                    var results = res.body;
                    done();
                });
            });
    */

});