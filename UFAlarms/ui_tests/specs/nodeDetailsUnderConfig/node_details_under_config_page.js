'use strict';

import {mockCustomer as mockCustomer} from '../../variables';

const NodeDetailsUnderConfig = function () {

    // elements
    this.nodeGrid = $('#Node-grid');
    this.siteTilesContainer = $('#site-tiles-container');
    this.viewScheduleButton = $('#viewSchedule');

    this.basicLabel = $('#basicIdentifiers');
    this.configLabel = $('#configlabel');
    this.reportsLabel = $('#reports');
    this.alertsAndAlarmsLabel = $('#alertsAndAlarms');

    this.panelGroup = $('.panel-group');
    this.accordianOpen = $('#collapseTwo');


    this.groupsLabel = $('#groupsAndProfilesLabel');
    this.wifiLabel = $('#wifiLabel');
    this.fixtureLabel = $('#fixtureLabel');
    this.locationLabel = $('#locationLabel');
    this.hardwareLabel = $('#hardwareLabel');
    this.lightingControlLabel = $('#lightingControlLabel');
    this.firmwareLabel = $('#firmwareLabel');
    this.configManagementLabel = $('#configManagementLabel');
    this.resetLabel = $('#resetLabel');


    this.expandConfig = function(){
        browser.wait(until.visibilityOf(this.panelGroup), waitTimeout);
        $('.panel-group > .panel-default:nth-child(2)').click();
        browser.wait(until.visibilityOf(this.accordianOpen), waitTimeout);
    };


    this.checkConfigLabel = function(){
        this.configLabel.getText().then(function (text) {
            expect(text).to.equal("Config");
        });
    };

    this.checkGroupsAndProfilesLabel = function(){
        this.groupsLabel.getText().then(function (text) {
            expect(text).include("Groups");
        });
    };

    this.checkWifiLabel = function(){
        this.wifiLabel.getText().then(function (text) {
             expect(text).include("Wifi");
        });
    };

    this.checkFixtureLabel = function(){
        this.fixtureLabel.getText().then(function (text) {
            expect(text).to.equal("Fixture");
        });
    };

    this.checkLocationLabel = function(){
        this.locationLabel.getText().then(function (text) {
            expect(text).to.equal("Location");
        });
    };

    this.checkHardwareLabel = function(){
        this.hardwareLabel.getText().then(function (text) {
            expect(text).to.equal("Hardware");
        });
    };

    this.checkLightningControlLabel = function(){
        this.lightingControlLabel.getText().then(function (text) {
            expect(text).to.equal("Lighting Control");
        });
    };

    this.checkFirmwareLabel = function(){
        this.firmwareLabel.getText().then(function (text) {
            expect(text).to.equal("Firmware");
        });
    };
    this.checkConfigurationManagementLabel = function(){
        this.configManagementLabel.getText().then(function (text) {
            expect(text).to.equal("Configuration Management");
        });
    };
    this.checkResetnLabel = function(){
        this.resetLabel.getText().then(function (text) {
            expect(text).to.equal("Reset");
        });
    };


};

module.exports = NodeDetailsUnderConfig;
