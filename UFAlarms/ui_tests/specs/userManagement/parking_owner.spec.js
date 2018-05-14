'use strict';

import UserManagementPage from './user_management.page';
import { parkingOwnerVariable as parkingOwnerVariable } from '../../variables';
import SiteSelectionPage from '../sitePanel/site_selection.page';


describe("New User Management UX panel ", ()=>{

    var createUser = new UserManagementPage();
    var selectSite = new SiteSelectionPage();

    it("Should Click the create New user icon", ()=>{
        // createUser.userManagementLink.click();
        browser.wait(until.visibilityOf(createUser.addNewActiveUser), waitTimeout);
        createUser.addNewActiveUser.click();
    });

    it('Expect User Management Name Field to be Empty', () => {
        browser.wait(until.visibilityOf(createUser.formName), waitTimeout);
        createUser.formName.getText().then(function (isNameEmpty) {
            assert.equal(isNameEmpty, '');
        });
    });

    it('Fill User Management form details', () => {
        createUser.fillUserForm(parkingOwnerVariable);
    });

    it('Save the Users details and wait for the pop up', () => {
        createUser.saveUser.click();
        browser.wait(until.visibilityOf(createUser.popUp), waitTimeout);
    });

    it('Route back to sites page', () => {
        browser.navigate().back();
    })

    it("Click Notification Link", () => {
        selectSite.getMoreOptions();
        selectSite.clickNotifications();
        browser.sleep(5000);
    })
});
