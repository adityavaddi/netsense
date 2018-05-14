'use strict';

// import { mockCustomer as mockCustomer } from '../../variables';

var CommissionPage = function () {
  // elements
  this.groupsGrid = $('#Group-grid');
  this.tableContainer = $('#node-table-container');
  this.checkBox = $('[type="checkbox"]');
  this.commissionBtn = $('#commision-btn-test');
  this.siteToBeSelected = $('#selectSites');
  this.saveBtn = $(".btn-primary-green");
  this.popUp = $('.noty_text');

  this.multiSelect = function () {
    browser.wait(until.visibilityOf(this.tableContainer), 15000);
    $('.slick-row').click();
    $('.slick-cell').click();
  };

  this.commissionIt = function () {
    browser.wait(until.visibilityOf(this.tableContainer), waitTimeout);
    this.commissionBtn.click();
  };

  this.selectSite = function () {
    this.siteToBeSelected.sendKeys("Amin site");
    this.saveBtn.click();
    // browser.sleep(5000);
  };
};

module.exports = CommissionPage;