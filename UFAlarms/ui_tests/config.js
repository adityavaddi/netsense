var sensity_admin = require('./suites/sensity_admin.js');
var sensity_user = require('./suites/sensity_user.js');
var partner_admin = require('./suites/partner_admin.js');
var end_user_admin = require('./suites/end_user_admin.js');
var single_test_sensity_user = require('./suites/single_test_sensity_user.js');
var randomName = require('./util/random.js');

exports.config = {
    framework: 'mocha',
    seleniumAddress: 'http://0.0.0.0:4444/wd/hub',
    baseUrl: process.env.ui_test_url || 'https://netsense-cicd.sensity.com',
    directConnect: true,
    multiCapabilities: [
        // {
        //     browserName: 'chrome',
        //     acceptSslCerts: true,
        //     trustAllSSLCertificates: true,
        //     specs: end_user_admin
        // }
        // ,
        // {
        //     browserName: 'chrome',
        //     acceptSslCerts: true,
        //     trustAllSSLCertificates: true,
        //     specs: sensity_admin
        // }
        // ,
        // {
        //     browserName: 'chrome',
        //     acceptSslCerts: true,
        //     trustAllSSLCertificates: true,
        //     specs: partner_admin
        // },
        {
            browserName: 'chrome',
            acceptSslCerts: true,
            trustAllSSLCertificates: true,
            specs: sensity_user,
            chromeOptions: {
                args: ["--headless", "--disable-gpu", "--window-size=800,600", "--no-sandbox"]
            }
        }
        // ,
        // {
        //     browserName: 'chrome',
        //     acceptSslCerts: true,
        //     trustAllSSLCertificates: true,
        //     specs: single_test_sensity_user
        // }
    ],

    onPrepare: () => {
        waitTimeout = 30000
        until = protractor.ExpectedConditions
        browser.ignoreSynchronization = true
        var width = 1650
        var height = 1650
        browser.driver.manage().window().setSize(width, height)
        require('babel-register')({
            "presets": ["es2015"]
        });
        chai = require('chai')
        expect = chai.expect
        should = chai.should
        assert = chai.assert
    },

    mochaOpts: {        
        reporter: "mocha-multi-reporters",
        reporterOptions: {
            "reporterEnabled": "xunit,mochawesome-screenshots",
            "xunitReporterOptions": {
                "output": "test-report/xunit.xml"
            },
            "mochawesomeScreenshotsReporterOptions": {
                reportDir: 'test-report/htmlreport',
                reportName: 'index',
                reportTitle: 'NetSense: Functional UI Test Report',
                reportPageTitle: 'NetSense',
                takePassedScreenshot: false,
                clearOldScreenshots: true,
                jsonReport: false,
                multiReport: false
            }           
        },
        timeout: 30000
    },

    allScriptsTimeout: 30000
}