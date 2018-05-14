/**
 * User management test
 * @note LDAP must not contain testuser
 * $ ldapsearch -H ldap://localhost:389 -x -D "cn=admin,dc=sensity,dc=com" -w admin123  -LLL -b "ou=People, dc=sensity, dc=com" cn=testuser  objectclass=Person
 * $ ldapdelete -H ldap://localhost:389 -x -D "cn=admin,dc=sensity,dc=com" -w admin123   'cn=testuser,ou=People,dc=sensity,dc=com'  objectclass=*
 */
"use strict";
var server_url = process.env.stack_url;
var should = require('should');
var request = require('supertest')(server_url);
var superagent = require('superagent');
var agent = superagent.agent();
var ldap = require('ldapjs')
var ldapClient = require('../../ldap/ldap-client.js');
var version = '/v3.0';

var configManager = require('kea-config');
configManager.init('./config/main.conf.js');

const helpers = require('./../../utils/helpers');
let csrfToken = null;

/*
 Datadealer needs to be running for these tests to be successful
 % lein with-profile testjs run all

 Run suite of tests with
 % cd Farallones/api_service/ui_api
 % mocha --recursive
 */

var userid = "";

describe('Users', function () {
    describe('POST /customers/{orgid}/users', function () {
        it('should fail without login credentials', function (done) {
            var data = {
                password: "foobar",
                name: "Test Users1",
                email: "testusers1@sensity.com",
                title: "User of Test",
                phone: "8675309",
                roles: "customer_admin",
                sites: "Test Site"
            };
            request.post(version + '/customers/uberorg/users')
                .send(data)
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });

        it('should not sign in with incorrect credentials', function (done) {
            var data = {
                email: "UberUser@sensity.com",
                password: "ubeR$23x"
            };
            request
                .post(version + '/login')
                .send(data)
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(404)
                .end(function (err, res) {
                    should.not.exist(err);
                    //agent.saveCookies(res);
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
                    var user_data = res.body;

                    //user_data.login.last_seen.should.eql(null);
                    //user_data.login.login_attempts.should.eql(1);
                    agent.saveCookies(res);
                    csrfToken = helpers.getCsrfToken(res);
                    done();
                });

        });

        it('should sign in with uppercase used for email', function (done) {
            var data = {
                email: "UBERUSER@SENSITY.COM",
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
                    var user_data = res.body;

                    user_data.login.login_attempts.should.eql(0);
                    agent.saveCookies(res);
                    csrfToken = helpers.getCsrfToken(res);
                    done();
                });

        });

        it('should fail to create if email already in LDAP', function (done) {

            var data = {
                password: "foobar",
                name: "Test User",
                email: "testuser@sensity.com",
                title: "User of Test",
                phone: "8675309",
                roles: "customer_admin",
                sites: "Test Site"
            };

            var entry = {
                // dn:'cn='+data.email+','+configManager.get('ldap.suffixUsers'),
                objectClass: "inetOrgPerson",
                uid: 'someuserid',
                sn: data.email,
                mail: data.email,
                cn: data.email,
                userPassword: data.password
            };

            var client = ldapClient.get_client();

            var dn = 'cn=' + data.email + ',' + configManager.get('ldap.suffixUsers');
            client.add(dn, entry, function (err) {
                should.not.exist(err);
                var req = request.post(version + '/customers/uberorg/users');
                agent.attachCookies(req);
                req.send(data)
                    .set('Accept', 'application/json')
                    .set('X-CSRF-Token', csrfToken)
                    .set('Content-Type', 'application/json')
                    .expect(400)
                    .end(function (err, res) {
                        should.not.exist(err);
                        var result = res.body;
                        result.error.should.eql(true);
                        result.status.should.eql(400);

                        var orgid = "uberorg";
                        var uri = version + "/customers/" + orgid + "/users";
                        var req = request.get(uri);
                        agent.attachCookies(req);
                        req.expect('Content-Type', /json/)
                            .expect(200)
                            .end(function (err, res) {
                                should.not.exist(err);
                                var items = res.body;
                                items.length.should.eql(2);

                                // Delete entry in ldap only
                                var orgid = "uberorg";
                                var uri = version + "/customers/" + orgid + "/remove-user-from-ldap";
                                var odata = {
                                    email: data.email
                                }
                                var req = request.post(uri);
                                agent.attachCookies(req);
                                req.send(odata)
                                    .set('X-CSRF-Token', csrfToken)
                                    .expect(204)
                                    .end(function (err, res) {
                                        should.not.exist(err);

                                        // Try to find entry in ldap
                                        client.search(dn, {}, function (err, res) {
                                            try {
                                                res.on('searchEntry', function (entry) {
                                                    console.error('LDAP entry not deleted: ' + JSON.stringify(entry));
                                                    throw "LDAP entry not deleted: " + dn;
                                                });
                                                res.on('error', function (err) {
                                                    console.log('info: ' + err.message);
                                                    if (err.name === 'NoSuchObjectError')
                                                        done();
                                                    else
                                                        throw "LDAP error: " + err.message;
                                                });
                                            } catch (e) {
                                                console.error(e.message);
                                                throw "LDAP entry not deleted: " + e.message;
                                            }
                                        });
                                    });
                            });
                    });
            });

        });

        it("should create a user using credentials", function (done) {
            var data = {
                name: "Test Users1",
                email: "testusers1@sensity.com",
                title: "User of Test",
                phone: "8675309",
                roles: "customer_admin",
                sites: "Test Site"
            };
            var req = request.post(version + '/customers/uberorg/users');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .set('Content-Type', 'application/json')
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    userid = result.userid;
                    delete result.userid;
                    result.should.eql(data);

                    // Should fail on User name and id constraints
                    // No two users should have the exact name
                    var bad_req = request.post(version + '/customers/uberorg/users');
                    agent.attachCookies(bad_req);
                    bad_req.send(data)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(400)
                        .end(function (err, res) {
                            should.not.exist(err);
                        });
                    done();
                });
        });

        // Try to delete user from ldap only
        it("should fail to delete user from ldap if in database", function (done) {
            var client = ldapClient.get_client();

            // Delete entry in ldap
            var orgid = "uberorg";
            var uri = version + "/customers/" + orgid + "/remove-user-from-ldap";
            var odata = {
                email: "testusers1@sensity.com",
            }
            var dn = 'cn=' + odata.email + ',' + configManager.get('ldap.suffixUsers');;
            var req = request.post(uri);
            agent.attachCookies(req);
            req.send(odata)
                .set('X-CSRF-Token', csrfToken)
                .expect(403)
                .end(function (err, res) {
                    should.not.exist(err);
                    // Try to find entry in ldap
                    client.search(dn, {}, function (err, res) {
                        try {
                            res.on('searchEntry', function (entry) {
                                console.log('info: ' + JSON.stringify(entry.object));
                                done();
                            });
                            res.on('error', function (err) {
                                if (err.name === 'NoSuchObjectError')
                                    throw "LDAP entry deleted: " + err.message;
                                else
                                    throw "LDAP error: " + err.message;
                            });
                        } catch (e) {
                            console.error(e.message);
                            throw "LDAP error: " + e.message;
                        }
                    });
                });
        });

        it("should get all uberorg users", function (done) {
            var orgid = "uberorg";
            var uri = version + "/customers/" + orgid + "/users";
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    //var items = res.body;
                    //items.length.should.eql(3);
                    done();
                });
        });

        it("should get and update a user using credentials", function (done) {
            var orgid = "uberorg";
            var uri = version + "/customers/" + orgid + "/users/" + userid;
            var data = {
                name: "Test Users1",
                email: "testusers1@sensity.com",
                title: "User of Test",
                phone: "8675309",
                roles: "customer_admin",
                sites: "Test Site"
            };
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    var result = res.body;
                    delete result.userid;
                    delete result.created;
                    delete result.updated;
                    res.body.should.eql(data);

                    var updated = {
                        userid: userid,
                        name: "Test Users1 Updated",
                        email: "testusers1@sensity.com",
                        title: "User of Testing",
                        phone: "8675309",
                        roles: "customer_admin",
                        sites: "Test Site Updated"
                    };
                    var update_req = request.post(uri);
                    agent.attachCookies(update_req);
                    update_req.send(updated)
                        .set('Accept', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, update_res) {
                            should.not.exist(err);
                            var result1 = update_res.body;
                            var dcreated = result1.created;
                            var dupdated = result1.updated;
                            // TODO: Test if updated > created
                            delete result1.created;
                            delete result1.updated;
                            result1.should.eql(updated);

                            done();

                        });
                });
        });
        it("should create, use, and revoke API Key using credentials", function (done) {
            var odata = {
                email: "testusers1@sensity.com"
            }
            var req = request.post(version + '/user/generate-api-key');
            agent.attachCookies(req);
            req.send(odata)
                .set('Accept', 'application/json')
                .set('Content-Type', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    //console.log('err', err, JSON.stringify(res.body))
                    should.not.exist(err);
                    var result = res.body;
                    var key = result.api_key;
                    //console.log('Key', key)

                    // Get user info with  new API Key
                    request
                        .get(version + '/login')
                        .set('api_key', key)
                        .set('Content-Type', 'application/json')
                        .set('Accept', 'application/json')
                        .expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);
                            //console.log('Key', JSON.stringify(res.body))
                            res.body.login.email.should.eql(odata.email);

                            // Set user password
                            var uri = version + "/user/set-password";
                            var kdata = {
                                password: 'testX$23',
                            }
                            var update_req = request.post(uri);
                            update_req.send(kdata)
                                .set('api_key', key)
                                .set('Content-Type', 'application/json')
                                .set('Accept', 'application/json')
                                .expect(204)
                                .end(function (err, res) {
                                    should.not.exist(err);

                                    // Revoke api key
                                    var uri = version + "/user/revoke-api-key";
                                    var kdata = {
                                        key: key,
                                    }
                                    var update_req = request.post(uri);
                                    agent.attachCookies(update_req);
                                    update_req.send(kdata)
                                        .set('Accept', 'application/json')
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect(204)
                                        .end(function (err, res) {
                                            should.not.exist(err);
                                            // Get user info with revoked API Key
                                            request
                                                .get(version + '/login')
                                                .set('api_key', key)
                                                .set('Accept', 'application/json')
                                                .expect('Content-Type', /json/)
                                                .expect(404)
                                                .end(function (err, res) {
                                                    should.not.exist(err);
                                                    done();
                                                });


                                        });

                                });


                        });

                });
        });

        it("should deactivate and then reactivate a user using credentials", function (done) {
            var orgid = "uberorg";
            var uri = version + "/customers/" + orgid + "/suspended-users/" + userid;
            var req = request.put(uri);

            var updated = {
                userid: userid,
                name: "Test Users1 Updated",
                email: "testusers1@sensity.com",
                title: "User of Testing",
                phone: "8675309",
                roles: "customer_admin",
                sites: "Test Site Updated"
            };

            agent.attachCookies(req);
            req.set('Accept', 'application/json')
                .set('Content-Type', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);

                    var orgid = "uberorg";
                    var uri = version + "/customers/" + orgid + "/suspended-users";
                    var req = request.get(uri);
                    agent.attachCookies(req);
                    req.expect('Content-Type', /json/)
                        .expect(200)
                        .end(function (err, res) {
                            should.not.exist(err);

                            var items = res.body;
                            items.length.should.eql(1);

                            var orgid = "uberorg";
                            var uri = version + "/customers/" + orgid + "/suspended-users/" + userid;
                            var req = request.delete(uri);
                            agent.attachCookies(req);
                            req.expect('Content-Type', /json/)
                                .set('X-CSRF-Token', csrfToken)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    var result1 = res.body;
                                    var dcreated = result1.created;
                                    var dupdated = result1.updated;
                                    delete result1.created;
                                    delete result1.updated;
                                    result1.should.eql(updated);

                                    var orgid = "uberorg";
                                    var uri = version + "/customers/" + orgid + "/suspended-users";
                                    var req = request.get(uri);
                                    agent.attachCookies(req);
                                    req.expect('Content-Type', /json/)
                                        .expect(200)
                                        .end(function (err, res) {
                                            should.not.exist(err);

                                            var items = res.body;
                                            items.length.should.eql(0);
                                            done();
                                        });

                                });
                        });


                });
        });

        it("should reset testuser password", function (done) {
            // Change user password
            var urip = version + '/customers/uberorg/user/' + userid + '/reset-password';
            var newpass = "pa$$1A34";
            var email = {
                email: "testusers1@sensity.com"
            };
            var updatep_req = request.post(urip);
            agent.attachCookies(updatep_req);
            updatep_req.send(email)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, update_res) {
                    should.not.exist(err);

                    // Login with updated credentials
                    var data = {
                        email: "testusers1@sensity.com",
                        password: newpass
                    };
                    request
                        .post(version + '/login')
                        .send(data)
                        .set('Accept', 'application/json')
                        .expect('Content-Type', /json/)
                        .expect(404)
                        .end(function (err, res) {
                            should.not.exist(err);
                            //agent.saveCookies(res);
                            done();
                        });

                });

        });

        it("should delete a user using credentials", function (done) {
            var orgid = "uberorg";
            var uri = version + "/customers/" + orgid + "/users/" + userid;
            var req = request.delete(uri);
            agent.attachCookies(req);
            req //.expect('Content-Type', /json/)
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);

                    done();
                });
        });

        it("should count remaining uberorg users", function (done) {
            var orgid = "uberorg";
            var uri = version + "/customers/" + orgid + "/users";
            var req = request.get(uri);
            agent.attachCookies(req);
            req.expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var items = res.body;
                    items.length.should.eql(2);
                    done();
                });
        });

        it("should create user, api-key, delete user and api-key", function (done) {
            var data = {
                name: "Test Users1",
                email: "testusers1@sensity.com",
                title: "User of Test",
                phone: "8675309",
                roles: "customer_admin",
                sites: "Test Site"
            };
            var req = request.post(version + '/customers/uberorg/users');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .set('Content-Type', 'application/json')
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    userid = result.userid;
                    delete result.userid;
                    result.should.eql(data);

                    var odata = {
                        email: data.email
                    }
                    var req = request.post(version + '/user/generate-api-key');
                    agent.attachCookies(req);
                    req.send(odata)
                        .set('Accept', 'application/json')
                        .set('Content-Type', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(200)
                        .end(function (err, res) {
                            //console.log('err', err, JSON.stringify(res.body))
                            should.not.exist(err);
                            var result = res.body;
                            var key = result.api_key;
                            //console.log('Key', key)

                            // Get user info with  new API Key
                            request
                                .get(version + '/login')
                                .set('api_key', key)
                                .set('Content-Type', 'application/json')
                                .set('Accept', 'application/json')
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    //console.log('Key', JSON.stringify(res.body))
                                    res.body.login.email.should.eql(odata.email);

                                    // Delete user and verify is api key working
                                    var orgid = "uberorg";
                                    var uri = version + "/customers/" + orgid + "/users/" + userid;
                                    var req = request.delete(uri);
                                    agent.attachCookies(req);
                                    req //.expect('Content-Type', /json/)
                                        .set('X-CSRF-Token', csrfToken)
                                        .expect(204)
                                        .end(function (err, res) {
                                            should.not.exist(err);

                                            // Get user info with  new API Key
                                            request
                                                .get(version + '/login')
                                                .set('api_key', key)
                                                .set('Content-Type', 'application/json')
                                                .set('Accept', 'application/json')
                                                .expect('Content-Type', /json/)
                                                .expect(404)
                                                .end(function (err, res) {
                                                    should.not.exist(err);

                                                    done();
                                                });
                                        });

                                });
                        });
                });
        });

        it("should create sensity_user, api-key, fail to create user using api key, delete user and api-key", function (done) {
            var data = {
                name: "Test Users0",
                email: "testusers0@sensity.com",
                title: "User of Test",
                phone: "8675309",
                roles: "customer_admin",
                sites: "Test Site"
            };
            var req = request.post(version + '/customers/uberorg/users');
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('Content-Type', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    var result = res.body;
                    userid = result.userid;
                    delete result.userid;
                    result.should.eql(data);

                    var odata = {
                        email: data.email
                    }
                    var req = request.post(version + '/user/generate-api-key');
                    agent.attachCookies(req);
                    req.send(odata)
                        .set('Accept', 'application/json')
                        .set('Content-Type', 'application/json')
                        .set('X-CSRF-Token', csrfToken)
                        .expect(200)
                        .end(function (err, res) {
                            //console.log('err', err, JSON.stringify(res.body))
                            should.not.exist(err);
                            var result = res.body;
                            var key = result.api_key;
                            //console.log('Key', key)

                            // Get user info with new API Key
                            request
                                .get(version + '/login')
                                .set('api_key', key)
                                .set('Content-Type', 'application/json')
                                .set('Accept', 'application/json')
                                .expect('Content-Type', /json/)
                                .expect(200)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    //console.log('Key', JSON.stringify(res.body))
                                    res.body.login.email.should.eql(odata.email);

                                    // Try to create user
                                    var data2 = {
                                        name: "Test Users21",
                                        email: "testusers2@sensity.com",
                                        title: "User1 of Test",
                                        phone: "8675309",
                                        roles: "customer_admin",
                                        sites: "Test Site"
                                    };
                                    var req = request.post(version + '/customers/uberorg/users');
                                    agent.attachCookies(req);
                                    req.send(data)
                                        .set('Accept', 'application/json')
                                        .set('api_key', key)
                                        .set('Content-Type', 'application/json')
                                        .expect(403)
                                        .end(function (err, res) {

                                            // Login again with API Key
                                            var data2 = {
                                                email: "uberuser@sensity.com",
                                                password: "ubeR$23"
                                            };
                                            request
                                                .post(version + '/login')
                                                .send(data2)
                                                .set('Content-Type', 'application/json')
                                                .set('Accept', 'application/json')
                                                .expect('Content-Type', /json/)
                                                .expect(200)
                                                .end(function (err, res) {
                                                    agent.saveCookies(res);
                                                    csrfToken = helpers.getCsrfToken(res);
                                                    // Delete user and verify is api key working
                                                    var orgid = "uberorg";
                                                    var uri = version + "/customers/" + orgid + "/users/" + userid;
                                                    var req = request.delete(uri);
                                                    agent.attachCookies(req);
                                                    req //.expect('Content-Type', /json/)
                                                        .set('X-CSRF-Token', csrfToken)
                                                        .expect(204)
                                                        .end(function (err, res) {
                                                            should.not.exist(err);

                                                            // Get user info with  new API Key
                                                            request
                                                                .get(version + '/login')
                                                                .set('api_key', key)
                                                                .set('Content-Type', 'application/json')
                                                                .set('Accept', 'application/json')
                                                                .expect('Content-Type', /json/)
                                                                .expect(404)
                                                                .end(function (err, res) {
                                                                    should.not.exist(err);

                                                                    done();
                                                                });
                                                        });
                                                });
                                        });
                                });
                        });
                });
        });

        it("should send email for forgot password request", function (done) {
            var uri = version + '/forgot-password';
            var data = {
                email: "uberuser@sensity.com"
            }
            var req = request.post(uri);
            agent.attachCookies(req);
            req.send(data)
                .set('Accept', 'application/json')
                .set('X-CSRF-Token', csrfToken)
                .expect(204)
                .end(function (err, res) {
                    should.not.exist(err);
                    done();
                });
        });
        
        it("should change uberuser password", function (done) {
            // Change user password
            var urip = version + '/user/update-password';
            var newpass = "pa$$1ZE4";
            var updatedp = {
                password: newpass,
                old_password: 'ubeR$23' // Modified in previous test
            };
            var updatep_req = request.post(urip);
            agent.attachCookies(updatep_req);
            updatep_req.send(updatedp)
                .set('Accept', 'application/json')
                .expect(204)
                .end(function (err, update_res) {
                    should.not.exist(err);

                    // Login with updated credentials
                    var data = {
                        email: "uberuser@sensity.com",
                        password: newpass
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

        });


        it("should fail to update password with a simple one", function (done) {

            var urip = version + '/user/update-password';
            var newpass = "ubeR$23";
            var updatedp = {
                password: newpass,
                old_password: 'pa$$1ZE4'
            };
            var updatep_req = request.post(urip);
            agent.attachCookies(updatep_req);
            updatep_req.send(updatedp)
                .set('Accept', 'application/json')
                .expect(403)
                .end(function (err, update_res) {
                    should.not.exist(err);

                    done();
                });
        });

        it("should cleanup db using credentials", function (done) {

            var newpass = "ubeR$23";

            // Change it directly in ldap

            var client = ldapClient.get_client();

            var change = new ldap.Change({
                operation: 'replace',
                modification: {
                    userPassword: newpass
                }
            });

            var dn = 'cn=' + 'uberuser@sensity.com' + ',' + configManager.get('ldap.suffixUsers');;

            console.log("updating uberuser password in ldap", dn);
            client.modify(dn, change, function (err) {
                if (err) {
                    console.error("ldap operation mod failed", err.message);
                }
                done();
            });

        });

    });
});