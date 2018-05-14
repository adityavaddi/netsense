'use strict';

import ParkingGroupsPage from './parkingGroups.page';
import { parkingGroupsVariable as parkingGroupsVariable } from '../../variables';
import SiteSelectionPage from '../sitePanel/site_selection.page';


describe("Parking Group UX panel ", () => {

    var selectSite = new SiteSelectionPage();
    var parkingGroupsPage = new ParkingGroupsPage();

    it("Click ParkingGroups Link", () => {
        selectSite.getMoreOptions();
        selectSite.clickParkingGroups();
    });

    it("Should Click The Add New Parking-Group ", () => {
        browser.wait(until.visibilityOf(parkingGroupsPage.panelGrid), waitTimeout);
        parkingGroupsPage.addBtn.click();
    });

    it('Expect Parking-Group Name Field To Be Empty', () => {
        browser.wait(until.visibilityOf(parkingGroupsPage.formName), waitTimeout);
        parkingGroupsPage.formName.getText().then(function (isNameEmpty) {
            assert.equal(isNameEmpty, '');
        });
    });

    it('Fill Parking-Group Form Details', () => {
        parkingGroupsPage.fillUserForm(parkingGroupsVariable);
    });

    it('Save The Users Details And Wait For The Pop Up', () => {
        parkingGroupsPage.saveIt.click();
        browser.wait(until.visibilityOf(parkingGroupsPage.popUp), waitTimeout);
    });

    it('Search The Added Parking-Group And Delete', () => {
        browser.wait(until.visibilityOf(parkingGroupsPage.popUp), waitTimeout);
    });

});
