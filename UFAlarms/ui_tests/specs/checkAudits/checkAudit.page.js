'use strict';

// import SidebarPage from '../sidebar/sidebar.page';
import {mockCustomer as mockCustomer} from '../../variables';

var CheckAudit = function () {
  // elements
  this.notyContainer = $('#noty_topRight_layout_container');

  this.auditGrid = $('#Audit-grid');
  this.userManagementLink = $('#userManagementLink');

  this.searchGivenAudit = function (data) {
    browser.wait(until.visibilityOf(this.auditGrid), waitTimeout);
  }
};

module.exports = CheckAudit;
