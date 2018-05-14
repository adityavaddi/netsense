'use strict';

import {mockCustomer as mockCustomer} from '../../variables';

const NodeNameChange = function () {

    // elements
    this.nodeGrid = $('#Node-grid');
    this.siteTilesContainer = $('#site-tiles-container');
    this.viewScheduleButton = $('#viewSchedule');
    this.basicLabel = $('#basicIdentifiers');
    this.saveNodeDetailsButton = $('#saveNodeDetails');
    this.panelGroup = $('.panel-group');
    this.name = $('#name');
    this.popUp = $('.noty_text');
    this.popUpClose = $('.noty_close');
    this.showHideDetailsButton = $('#showHideDetails');


    this.checkNameField = function(){
        this.name.getText().then(function (text) {
            expect(text).to.equal("");
        });
    };

    this.changeName = function(){
        this.name.clear();
        this.name.sendKeys("New Name 2");
    };

    this.checkPopUpText = function(){
        this.popUp.getText().then(function (text) {
            expect(text).include("updated");
        });
    }
};
module.exports = NodeNameChange;