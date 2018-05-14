/**
 * Created by Senthil on 03/16/18.
 */
const server_url = process.env.stack_url;
const should = require('should');
const request = require('supertest')(server_url);
const superagent = require('superagent');
const agent = superagent.agent();
const uuid = require('uuid');
const version = '/v3.0';

describe('Login Sanity Test', function () {
    describe('POST /login', function () {
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
                    done();
                });
        });
    });
});