'use strict';
import createSitePom from './createSitePom';
import { sites as sites } from '../../variables';
import { customer as customer } from './../createCustomer/mockData'
import { nodeVariable1 as nodeVariable1 } from './../../variables'
import { nodeVariable2 as nodeVariable2 } from './../../variables'
import DataUtilCalls from './../apiCalls-trial/apiCalls'
import { allCustomers as allCustomers } from './../../variables'
import { config as config } from './../../config';

var should = require('should');
var server_url = config.baseUrl + '/v3.0';
var request = require('supertest')(server_url);
var superagent = require('superagent');
var agent = superagent.agent();
let csrfToken = null;

describe('Should Create a site under customer', () => {
    var createSite = new createSitePom();
    // Fetch all the customers to find the customer id and site id
    before((done) => {
        console.log("customerName in sites page", customer.name)
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
                        done();
                    });
            });
    });
    after((done) => {
        var createNodereq = request.post('/customers/' + allCustomers.orgid + '/sites/' + allCustomers.siteid + '/nodes');
        agent.attachCookies(createNodereq);
        createNodereq.send(nodeVariable1) //.expect('Content-Type', /json/)
            .set('X-CSRF-Token', csrfToken)
            .expect(200)
            .end(function (err, res) {
                should.not.exist(err);
                done();
            });
    });

    it('Should Pull up a new form to add a site', () => {
        createSite.addSiteBtn.click();
        createSite.form_name.getText().then(function (isNameEmpty) {
            assert.equal(isNameEmpty, '');
        });
    });

    it('Should Fill new Site form and validate address ', () => {
        createSite.fillForm(sites.siteVariable);
    });

    it('Should Hit save and wait for the pop up', (done) => {
        createSite.form_saveSite_btn.click();

        browser.wait(until.visibilityOf(createSite.siteTilesContainer), waitTimeout).then(function () {
            // It is visible  
            browser.wait(until.textToBePresentInElement(createSite.notyContainer, 'Site'), waitTimeout).then(function () {
                // It is visible  
                var getSiteIdReq = request.get('/customers/' + allCustomers.orgid + '/sites');
                agent.attachCookies(getSiteIdReq);
                getSiteIdReq
                    .expect(200)
                    .end(function (err, res) {
                        should.not.exist(err);
                        var data = res.body;
                        for (var i in data) {
                            if (data[i].name === sites.siteVariable.name) {
                                allCustomers.siteid = data[i].siteid;
                            }
                        }
                        done();
                    });
            });
        });
    });
});