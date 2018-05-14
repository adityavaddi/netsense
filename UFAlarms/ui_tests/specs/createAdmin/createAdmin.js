'use strict';
import createAdminPom from './createAdminPom';
import { endUserAdminVariable as endUserAdminVariable } from '../../variables';

describe('Should Create End User Admin', () => {
    var createAdmin = new createAdminPom();

    it('Should Pull the Customer page up and click the site ', () => {
        browser.wait(until.visibilityOf(createAdmin.customerGrid), waitTimeout);
        createAdmin.routeToSites();
        createAdmin.routeToNodes();
        createAdmin.routeToUserManagement();
    });

    // Click on add user button
    it('Should Pull up a new form to add end user admin', () => {
        createAdmin.addUserBtn.click();
        // verify if form is empty
        createAdmin.form_name.getText().then(function (isNameEmpty) {
            assert.equal(isNameEmpty, '');
        });
    });

    // Fill in the form
    it('Should Fill new End User form', () => {
        createAdmin.fillForm(endUserAdminVariable);
    });

    // Hit save and check for pop-up message
    it('Should Hit save and check for pop-up message', () => {
        createAdmin.form_saveUser_btn.click()
        browser.wait(until.visibilityOf(createAdmin.popUp), waitTimeout);
        createAdmin.isEndAdminCreated();
    });

    // Verify if added user is reflecting in users list
    it('Should Search grid for added user', () => {
        createAdmin.searchAddedAdmin(endUserAdminVariable);
    });

    // //Validate duplicate
    it('Should Pull up a new form to add end user admin again', () => {
        browser.wait(until.visibilityOf(createAdmin.userTable), waitTimeout);
        createAdmin.addUserBtn.click();
        // verify if form is empty
        createAdmin.form_name.getText().then(function (isNameEmpty) {
            assert.equal(isNameEmpty, '');
            createAdmin.fillForm(endUserAdminVariable);
            browser.wait(until.invisibilityOf(createAdmin.popUp), waitTimeout);
            createAdmin.form_saveUser_btn.click()
            createAdmin.isDuplicatePresent();
        });
    });
});
