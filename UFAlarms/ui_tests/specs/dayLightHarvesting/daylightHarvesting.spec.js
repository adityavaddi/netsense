'use strict';
import createDaylightHarvestingPage from './daylightHarvesting.page';
import { daylightHarvestingVariable as daylightHarvestingVariable } from '../../variables';
import { groups as groups } from '../../variables';
import SiteSelectionPage from '../sitePanel/site_selection.page.js';

describe('Create Daylight Harvesting profile under A Site', () => {
    var createDaylightHarvesting = new createDaylightHarvestingPage();
    var selectSite = new SiteSelectionPage();


    // Before the test cases executes - it needs to route back to the Sites page and then go to the Day Light Harvesting Page
    before(() => {
        browser.navigate().back();
        selectSite.getMoreOptions();
        selectSite.clickDayLightHarvesting();
    });

    // Add a New DH profile  and save it
    it('Should Pull up a new form to add a Daylight Harvesting Profile', () => {
        browser.wait(until.visibilityOf(createDaylightHarvesting.daylightGrid), waitTimeout);
        createDaylightHarvesting.addDaylightButton.click();
        browser.wait(until.visibilityOf(createDaylightHarvesting.modal), waitTimeout);
        createDaylightHarvesting.name.getText().then(function (isNameEmpty) {
            assert.equal(isNameEmpty, '');
        });
    });

    it('Should Fill new Daylight Harvesting Profile form and Save', (done) => {
        createDaylightHarvesting.fillForm(daylightHarvestingVariable);
        browser.wait(until.textToBePresentInElement(createDaylightHarvesting.notyContainer, 'Daylight Harvesting profile'), waitTimeout).then(function () {
            // It is visible  
            done();
        });
    });

    // Route to the Groups page to assign to a group
    it("Should Route to the Sites page and then to Groups page", () => {
        browser.navigate().back();
        selectSite.getMoreOptions();
        selectSite.clickGroupsLink();
    });

    it("Should search for the group and add the Dh profile for it that was just created ", () => {
        browser.wait(until.visibilityOf(createDaylightHarvesting.gridCanvas), waitTimeout).then(function () {
            // It is visible  
        }, function () {
            // It is not visible
            browser.refresh();                   
        });
        createDaylightHarvesting.searchGroup(groups.name);
        createDaylightHarvesting.showHideDetailsButton.click();
        browser.wait(until.visibilityOf(createDaylightHarvesting.gropsDetailPanel), waitTimeout);

    });

    it("Should Select the recent Daylight Harvesting profile and assign to the group", () => {
        createDaylightHarvesting.etdhprofileDropDown.sendKeys(daylightHarvestingVariable.name);
        createDaylightHarvesting.clickSaveGroup();
    });

});
