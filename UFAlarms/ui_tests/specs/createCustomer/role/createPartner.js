'use strict';
import Page from '../page';
import { partner } from '../mockData';

describe('As Sensity Admin Create a Partner', () => {

  var createCustomerPage = new Page();

  it('Pull up a new form to add partner', () => {
    browser.wait(until.visibilityOf(createCustomerPage.addCustomerBtn), waitTimeout);
    createCustomerPage.addCustomerBtn.click();
    createCustomerPage.form_name.getText().then(function (isNameEmpty) {
      assert.equal(isNameEmpty, '');
    });
  });

  it('Fill new partner form', () => {
    createCustomerPage.fillForm_create_partner(partner);
  });

  it('Hit save and check for pop-up message', () => {
    createCustomerPage.form_saveCustomer_btn.click();
    browser.wait(until.visibilityOf(createCustomerPage.popUp), waitTimeout);
  });

  it('validate popup text', () => {
    createCustomerPage.isPartnerCreated();
  });

  it('Search grid for added partner and click the sites icon to route to the sites page ', () => {
    createCustomerPage.searchAddedPartner(partner);
     // @TODO: as qa_ui_automation does not have code from sbt multi branch for the id's added
     // @TODO: to execute the next test case to create a partner admin
     // createCustomerPage.userManagementLink.click();
  });

});
