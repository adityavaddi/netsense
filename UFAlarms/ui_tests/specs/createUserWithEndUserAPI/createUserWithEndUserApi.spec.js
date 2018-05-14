'use strict';

import UserWithEndUserApiPage from './createUserWithEndUserApi.page';
import { userWithEndUserApi as userWithEndUserApi } from '../../variables';
import SiteSelectionPage from '../sitePanel/site_selection.page';


describe("New User Management UX panel ", ()=>{

    var createUserWithEndUserAPI = new UserWithEndUserApiPage();
    var selectSite = new SiteSelectionPage();


    it("Should Click the create New user icon", ()=>{
        browser.wait(until.visibilityOf(createUserWithEndUserAPI.addNewActiveUser), waitTimeout);
        createUserWithEndUserAPI.addNewActiveUser.click();
    });

    it('Expect User Management Name Field to be Empty', () => {
        browser.wait(until.visibilityOf(createUserWithEndUserAPI.formName), waitTimeout);
        createUserWithEndUserAPI.formName.getText().then(function (isNameEmpty) {
            assert.equal(isNameEmpty, '');
        });
    });

    it('Fill User Management form details', () => {
        createUserWithEndUserAPI.fillUserForm(userWithEndUserApi);
    });

    it('Save the User details, Click Generate API key and wait for the pop up', () => {
        createUserWithEndUserAPI.saveUser.click();
        createUserWithEndUserAPI.generateApiKeybutton.click();
        // To access the browser alert box pop up that comes
        browser.switchTo().alert().accept();
        browser.wait(until.visibilityOf(createUserWithEndUserAPI.popUp), waitTimeout);
    });

});
