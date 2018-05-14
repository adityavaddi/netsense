'use strict';

import { mockCustomer as mockCustomer } from '../../variables';

const createDaylightHarvestingPage = function () {

    // elements
    this.popUp = $(".noty_text");

    this.daylightGrid = $('#Daylight-grid');
    this.addDaylightButton = $('#addDh');
    this.modal = $('.modal-dialog');

    this.name = $('#name');
    this.highLux = $('#highLux');
    this.lowLux = $('#lowLux');
    this.lowDriver = $('#lowDriver');
    this.minDriver = $('#minDriver');
    this.saveDhProfile = $('#saveDhProfile');
    this.groupsGrid = $('#Group-grid');
    this.showHideDetailsButton = $('#showDetailsGroups');
    this.gropsDetailPanel = $('#group-detail-panel h2');
    this.etdhprofileDropDown = $('#etdhprofile');
    this.saveGroupsInfo = $('#save-group-button');
    this.notyContainer = $('#noty_topRight_layout_container');
    this.gridCanvas = $('.grid-canvas');

    this.fillForm = function (data) {

        this.name.sendKeys(data.name);
        this.highLux.clear();
        this.highLux.sendKeys(data.highLux);
        this.lowLux.clear();
        this.lowLux.sendKeys(data.lowLux);
        this.lowDriver.clear();
        this.lowDriver.sendKeys(data.lowDriver);
        this.minDriver.clear();
        this.minDriver.sendKeys(data.minDriver);
        this.saveDhProfile.click();       
    };

    this.searchGroup = function (data) {
        browser.wait(until.visibilityOf(this.groupsGrid), waitTimeout);
        $('.slick-headerrow-column:nth-child(2) input').sendKeys(data);
        $('.slick-row:first-child > .slick-cell:nth-child(2)').getText().then((data) => {
            expect(data).to.equal(data);
        }); 
        browser.sleep(1000);  
        browser.wait(until.elementToBeClickable($('.grid-canvas .slick-row')), waitTimeout).then(function(){
            $('.grid-canvas .slick-row').click();
        });
    };

    this.clickSaveGroup = function() {
        browser.actions().mouseMove(this.saveGroupsInfo).perform();
        this.saveGroupsInfo.click();
        browser.wait(until.stalenessOf(this.notyContainer), waitTimeout);
    }

};

module.exports = createDaylightHarvestingPage;
