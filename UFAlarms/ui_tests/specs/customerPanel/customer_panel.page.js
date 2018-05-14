'use strict';

import { mockCustomerParkingGroup as mockCustomerPg } from '../../variables';
import { mockCustomerUserManagement as mockCustomerUm } from '../../variables';
import { mockCustomerConfig as mockCustomerConfig } from '../../variables';
import { mockCustomerPd as mockCustomerPd } from '../../variables';
import { trafficConfigData as trafficConfigData } from '../../variables';
import { mockCustomer as mockCustomer } from '../../variables';
import { mockPartner as mockPartner } from '../../variables';
// import { input as mockCustomer } from '../createCustomer/mockData';


var Page = function () {
  // elements
  this.searchByName = element(by.xpath("//*[@id='Customer-grid']/div[3]/div[1]/div[2]/input"));

  this.searchCustomer = function (data) {
    browser.wait(until.visibilityOf(this.searchByName), waitTimeout);
    // Send in the name of the customer needed to be found
    this.searchByName.sendKeys(data);
  };

  this.routeToSites = function () {
    $('.grid-canvas .slick-row .slick-cell.l6.r6 ').click();
  };


};

module.exports = Page;
