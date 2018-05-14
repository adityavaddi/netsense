'use strict';

import UserWithPartnerApiPage from './createUserWithPartnerApi.page';
import { userWithPartnerApi as userWithPartnerApi } from '../../variables';
import SiteSelectionPage from '../sitePanel/site_selection.page';


describe("Create User With Partner API ", ()=>{

    var createUserWithPartnerApi = new UserWithPartnerApiPage();
    var selectSite = new SiteSelectionPage();

    //
    it('Route back to sites page and back to accounts page', () => {
        browser.navigate().back(); // sites page
        browser.navigate().back(); // customer page
    });

    it(" Go to Customer page and search for the partner", ()=>{
        browser.wait(until.visibilityOf(createUserWithPartnerApi.customerGrid), waitTimeout);
        $('.slick-headerrow-column:nth-child(2) input').sendKeys("partner");
        $('.slick-row:first-child .customer-sites-icon').click();

    });

    it("Click UserManagement Link", ()=>{
        browser.sleep(12000)
        createUserWithPartnerApi.userManagementLink.click();
    });

    it("Should Click the create New user icon", ()=>{
        browser.wait(until.visibilityOf(createUserWithPartnerApi.addNewActiveUser), waitTimeout);
        createUserWithPartnerApi.addNewActiveUser.click();
    });

    it('Expect User Management Name Field to be Empty', () => {
        browser.wait(until.visibilityOf(createUserWithPartnerApi.formName), waitTimeout);
        createUserWithPartnerApi.formName.getText().then(function (isNameEmpty) {
            assert.equal(isNameEmpty, '');
        });
    });

    it('Fill User Management form details', () => {
        createUserWithPartnerApi.fillUserForm(userWithPartnerApi);
    });

    it('Save the User details, Click Generate API key and wait for the pop up', () => {
        createUserWithPartnerApi.saveUser.click();
        createUserWithPartnerApi.generateApiKeybutton.click();
        // To access the browser alert box pop up that comes
        browser.switchTo().alert().accept();
        browser.wait(until.visibilityOf(createUserWithPartnerApi.popUp), waitTimeout);
    });

});
