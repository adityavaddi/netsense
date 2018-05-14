'use strict';
import SidebarPage from '../sidebar/sidebar.page';
import { mockCustomer as mockCustomer } from '../../variables';

const createAdminPom = function () {

  var sidebarPage = new SidebarPage();

  // elements
  this.addUserBtn = $("#addUser");
  this.form_name = $("#name");
  this.form_email = $("#email");
  this.form_title = $("#title");
  this.form_phone = $("#phone");
  this.form_roles = $("#roles");
  this.form_saveUser_btn = $("#saveUser");
  this.searchByName = $('.slick-headerrow-column:first-child input');
  this.selectedFirstRow = $('.slick-row:first-child > .slick-cell:first-child');
  this.popUp = $(".noty_text");

  //Common
  this.customerGrid = $('#Customer-grid');
  this.userTable = $('#user-table-container');
  this.siteTilesContainer = $('#site-tiles-container');
  this.mockCustomerAdmin = "Cisco End Customer1234 Verizon"
  // functions
  this.fillForm = function (data) {
    this.form_name.sendKeys(data.name);
    this.form_email.sendKeys(data.email);
    this.form_title.sendKeys(data.title);
    this.form_phone.sendKeys(data.phone);
    this.form_roles.sendKeys(data.roles);
  };

  this.routeToSites = function () {
    browser.wait(until.visibilityOf(this.customerGrid), waitTimeout);
    $('.slick-headerrow-columns > .slick-headerrow-column:first-child input').sendKeys(this.mockCustomerAdmin);
    $('.slick-row:first-child').click();
    sidebarPage.goToSites();
  };

  this.routeToNodes = function () {
    browser.wait(until.visibilityOf(this.siteTilesContainer), waitTimeout);
    $('.site-tile >.site-tile-buttons  .site-tile-button:nth-child(3)').click();
  };

  this.routeToUserManagement = function () {
    sidebarPage.goToUserManagement();
  };

  this.isEndAdminCreated = function () {
    this.popUp.getText().then(function (text) {
      expect(text).include("added");
    });
  };

  this.isDuplicatePresent = function () {
    this.popUp.getText().then(function (text) {
      expect(text).include("already exists");
    });
  };

  this.searchAddedAdmin = function (data) {
    browser.wait(until.visibilityOf(this.searchByName), waitTimeout);
    this.searchByName.sendKeys(data.name);
    this.selectedFirstRow.getText().then((text) => {
      expect(text).to.equal(data.name);
    });
  }
};

module.exports = createAdminPom;