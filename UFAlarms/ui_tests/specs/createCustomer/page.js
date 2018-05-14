'use strict';
import { msgs } from './messages';

const Page = function () {

  // elements
  this.addCustomerBtn = $("#addCustomer");
  this.form_name = $("#name");
  this.form_street1 = $("#street1");
  this.form_street2 = $("#street2");
  this.form_city = $("#city");
  this.form_state = $("#state");
  this.form_postalCode = $("#postal_code");
  this.form_country = $("#country");
  this.form_contact = $("#contact");
  this.form_contactName = $("#contact_name");
  this.form_contactPhone = $("#contact_phone");
  this.form_contactEmail = $("#contact_email");
  this.form_saveCustomer_btn = $("#saveCustomer");
  this.searchByType = element(by.xpath("//*[@id='Customer-grid']/div[3]/div[1]/div[2]/input"));
  this.selectedFirstRow = element(by.xpath("//*[@id='Customer-grid']/div[5]/div/div/div[2]"));
  this.popUp = $('.noty_text');  
  this.popUpClose = $('.noty_close');
  this.searchByName = element(by.xpath("//*[@id='Customer-grid']/div[3]/div[1]/div[2]/input"));

  this.userManagementLink = $('#userManagementLink');

  this.navbar = $("#navbar");

  // elements for Sensity Admin
  this.form_isPartner = $("#type");
  this.UserManagement_AddUsericon = $('#addUser');
  this.form_email = $("#email");
  this.form_title = $("#title");
  this.form_phone = $("#phone");
  this.form_saveUser = $(".btn .btn-success");

  // functions
  this.fillForm = function (data) {
    this.form_name.sendKeys(data.name);
    this.form_street1.sendKeys(data.street1);
    this.form_street2.sendKeys(data.street2);
    this.form_city.sendKeys(data.city);
    this.form_state.sendKeys(data.state);
    this.form_postalCode.sendKeys(data.postal_code);
    this.form_country.sendKeys(data.country);
    this.form_contactName.sendKeys(data.contact_name);
    this.form_contactPhone.sendKeys(data.contact_phone);
    this.form_contactEmail.sendKeys(data.contact_email);
  };

  this.fillForm_create_partner = function (data) {
    this.form_name.sendKeys(data.name);

    this.form_street1.sendKeys(data.street1);
    this.form_street2.sendKeys(data.street2);
    this.form_city.sendKeys(data.city);
    this.form_state.sendKeys(data.state);
    this.form_postalCode.sendKeys(data.postal_code);
    this.form_country.sendKeys(data.country);
    this.form_contact.sendKeys(data.contact);
    this.form_contactName.sendKeys(data.contact_name);

    var bar = $('[type="checkbox"]');
    bar.click();
  };

  this.goToAccountsPage = function (newLink) {
    $('.subnav.active .subnavlist li:first-child a').click()
  };

  this.isCustomerCreated = function () {
    browser.wait(until.visibilityOf(this.popUp), waitTimeout);
    this.popUp.getText().then(function (text) {
      expect(text).to.equal(msgs.customer_sucess);
    });
    this.closeNoty();
  };

  this.isDuplicateCustomerPresent = function () {
    this.popUp.getText().then(function (text) {
      expect(text).to.equal(msgs.customer_warning);
    });
  };

  this.isPartnerCreated = function () {
    this.popUp.getText().then(function (text) {
      expect(text).to.equal(msgs.partner_sucess);
    });
  };

  this.isDuplicatePartnerPresent = function () {
    this.popUp.getText().then(function (text) {
      expect(text).to.equal(msgs.partner_warning);
    });
  };

  this.searchAddedCustomer = function (data) {
    browser.wait(until.visibilityOf(this.searchByName), waitTimeout);
    this.searchByName.sendKeys(data.name);

    this.selectedFirstRow.getText().then((text) => {
      expect(text).to.equal(data.name);
    });
  };

  this.searchAddedPartner = function (data) {
    browser.wait(until.visibilityOf(this.searchByName), waitTimeout);
    this.searchByName.sendKeys(data.name);
    // this.searchByType.sendKeys(data.type);
    this.selectedFirstRow.getText().then((text) => {
      expect(text).to.equal(data.name);
    });

    // As qa_ui_automation does not have code deployed from sbt multi branch commenting it for now
    // $('.slick-row:first-child .customer-sites-icon').click();
  };

  // Close Noty
  this.closeNoty = function () {
    const that = this;
    that.popUp.isDisplayed().then(function (isVisible) {
      if (isVisible) {      
        browser.actions().mouseMove(that.popUp).perform();
        that.popUpClose.click();
      }
    });
  }
};

module.exports = Page;
