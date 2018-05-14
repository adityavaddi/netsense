'use strict';
import Page from '../page';
import { variable as input } from '../../../variables';
import { customer } from './../mockData';

describe('As Sensity User Create a Customer', () => {

  var createCustomerPage = new Page();

  it("Check the user is on Site Panel or on Customer page", () => {
    browser.wait(until.visibilityOf(createCustomerPage.navbar), waitTimeout).then(function() {      
      // It is visible
      browser.getCurrentUrl().then(function (url) {
        if (url.includes("/app/sitepanel")) {
          browser.get(input.baseUrl + "/app/customerpanel");
          browser.sleep(1000);
        }
      });
    }, function () {
       // It is not visible
       console.log("Login failed. Exiting process and not running other test cases");
       process.exit();
    });   
  });

  it('Pull up a new from to add customer', () => {
    browser.wait(until.visibilityOf(createCustomerPage.addCustomerBtn), waitTimeout);
    createCustomerPage.addCustomerBtn.click();
    createCustomerPage.form_name.getText().then(function (isNameEmpty) {
      assert.equal(isNameEmpty, '');
    });

  });

  it('Fill new customer form', () => {
    createCustomerPage.fillForm(customer);
  });

  it('Hit save and check for pop-up message', () => {
    createCustomerPage.form_saveCustomer_btn.click();
    browser.wait(until.visibilityOf(createCustomerPage.popUp), waitTimeout);
  });

  it('validate popup text for Created Customer', () => {
    createCustomerPage.isCustomerCreated();
  });

  it('Search grid for added customer and verify', () => {
    createCustomerPage.searchAddedCustomer(customer);
  });

  it("Route to the sites page to navigate to User Management", () => {
    // $('.slick-row:first-child .customer-sites-icon').click();
    $('.slick-row .slick-cell.l6.r6').click();
  });

});