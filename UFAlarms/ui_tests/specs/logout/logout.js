'use strict';
import Page from './page';

import { allCustomers as allCustomers } from './../../variables'
import { config as config } from './../../config';
import { customer as customer } from './../createCustomer/mockData'
var should = require('should');
var server_url = config.baseUrl + '/v3.0';
var request = require('supertest')(server_url);
var superagent = require('superagent');
var agent = superagent.agent();
let csrfToken = null;

describe('Logout application', () => {
    var logoutPage = new Page();

    it('User Logout', () => {
        logoutPage.notyContainer.isDisplayed().then(function (result) {
            if (result) {
                browser.wait(until.invisibilityOf(logoutPage.notyContainer), waitTimeout);
            }
        }, function (err) {
            //console.log('No Password Reset Dialog');
        });
        logoutPage.logout();
    });

    it('Delete customer', () => {
        after((done) => {
            var data = { email: "sensity_user@sensity.com", password: "admin123" };
            request
                .post('/login')
                .send(data)
                .set('Accept', 'application/json')
                .expect('Content-Type', /json/)
                .expect(200)
                .end(function (err, res) {
                    should.not.exist(err);
                    agent.saveCookies(res);
                    var getOrgIdReq = request.get('/customers');
                    agent.attachCookies(getOrgIdReq);
                    getOrgIdReq
                        .expect(200)
                        .end(function (err, res2) {
                            should.not.exist(err);
                            var data = res2.body;
                            //var resultJson = JSON.parse(data);                        
                            for (var i in data) {
                                if (data[i].name === customer.name) {
                                    allCustomers.orgid = data[i].orgid;
                                }
                            }
                            csrfToken = res.headers["netsense-csrf-token"] ? res.headers["netsense-csrf-token"] : "";
                            var deleteUserreq = request.delete("/customers/" + allCustomers.orgid);
                            agent.attachCookies(deleteUserreq);
                            deleteUserreq.set('X-CSRF-Token', csrfToken)
                                .expect(204)
                                .end(function (err, res) {
                                    should.not.exist(err);
                                    done();
                                });
                        });
                });
        });
    });
});