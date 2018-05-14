'use strict';

import {mockCustomer as mockCustomer} from '../../variables';

const LabelsOnNodesPage = function () {

    // elements
    this.nodeGrid = $('#Node-grid');
    this.siteTilesContainer = $('#site-tiles-container');
    this.viewScheduleButton = $('#viewSchedule');

    this.basicLabel = $('#basicIdentifiers');
    this.configLabel = $('#configlabel');
    this.reportsLabel = $('#reports');
    this.alertsAndAlarmsLabel = $('#alertsAndAlarms');

    this.checkBasicIdentifierLabel = function(){
        this.basicLabel.getText().then(function (text) {
            expect(text).to.equal("Basic Identifiers");
        });
    };

    this.checkConfigLabel = function(){
        this.configLabel.getText().then(function (text) {
            expect(text).to.equal("Config");
        });
    };

    this.checkReportsLabel = function(){
        this.reportsLabel.getText().then(function (text) {
            expect(text).to.equal("Reports");
        });
    };

    this.checkAlertsAndAlaramsLabel = function(){
        this.alertsAndAlarmsLabel.getText().then(function (text) {
            expect(text).to.equal("Alerts & Alarms");
        });
    };

};

module.exports = LabelsOnNodesPage;
