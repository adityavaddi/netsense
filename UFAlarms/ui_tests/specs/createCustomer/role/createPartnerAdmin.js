'use strict';
import Page from '../page';
import { partner } from '../mockData';
import { partnerAdminVariable } from '../../../variables';
import SiteSelectionPage from '../../sitePanel/site_selection.page';
import UserManagementPage from '../../userManagement/user_management.page';

describe('As Sensity Admin Create a Partner Admin', () => {

  var createUser = new UserManagementPage();
  var selectSite = new SiteSelectionPage();

  it("Should Click the create New user icon", ()=>{
    browser.wait(until.visibilityOf(createUser.addNewActiveUser), waitTimeout);
    createUser.addNewActiveUser.click();
  });

  it('Expect User Management Name Fiel d to be Empty', () => {
    browser.wait(until.visibilityOf(createUser.formName), waitTimeout);
    createUser.formName.getText().then(function (isNameEmpty) {
      assert.equal(isNameEmpty, '');
    });
  });

  it('Fill User Management form details to create Partner Admin', () => {
    createUser.fillUserForm(partnerAdminVariable);
  });

  it('Save the Users details and wait for the pop up', () => {
    createUser.saveUser.click();
    browser.wait(until.visibilityOf(createUser.popUp), waitTimeout);
  });

});
