'use strict';

// import SidebarPage from '../sidebar/sidebar.page';
import { mockCustomer as mockCustomer } from '../../variables';

const createGroupPom = function () {

  // var sidebarPage = new SidebarPage();

  // elements
  this.addGroupBtn = $("#add-group");
  this.form_name = $("#name");
  this.form_description = $("#description");
  this.form_selectLighting_btn = $("#select-lighting");
  this.form_xschedule_btn = $("#xschedule");
  this.form_dhprofile_btn = $("#etdhprofile");
  this.form_pdprofile_btn = $("#pdprofile");
  this.deleteGroupBtn = $("#delete-group");
  this.form_saveGroup_btn = $("#save-group-button");
  this.popUp = $(".noty_text");
  this.groupsGrid = $('#Group-grid');
  this.showHideDetailsButton = $('#showDetailsGroups');
  this.gropsDetailPanel = $('#group-detail-panel');

  //Common
  this.customerGrid = $('#Customer-grid');
  this.siteTilesContainer = $('#site-tiles-container');
  this.mockGroupCustomer = "Cisco End Customer1234 Verizon";

  // functions
  this.fillForm = function (data) {
    this.form_name.sendKeys(data.name);
    this.form_description.sendKeys(data.description);
    this.form_xschedule_btn.sendKeys(data.xschedule);
    this.form_dhprofile_btn.sendKeys(data.dhprofile);
    this.form_pdprofile_btn.sendKeys(data.pdprofile);
  };

  this.clearSearchArea = function(){
    this.searchByName.clear();
  };

  this.routeToCustomer = function () {
    browser.wait(until.visibilityOf(this.siteTilesContainer), waitTimeout);
    sidebarPage.goToCustomer();
    $('.slick-headerrow-columns > .slick-headerrow-column:first-child input').sendKeys(this.mockGroupCustomer);
    $('.slick-row:first-child').click();
    sidebarPage.goToSites();
  };


  this.isGroupCreated = function () {
    this.popUp.getText().then(function (text) {
      expect(text).to.equal("added");
    });
  };

  this.searchAddedGroup = function (data) {
    browser.wait(until.visibilityOf(this.groupsGrid), waitTimeout);
    $('.slick-headerrow-column:nth-child(2) input').sendKeys(data.name);
    $('.slick-row:first-child > .slick-cell:nth-child(2)').getText().then((data) => {
      expect(data).to.equal(data);
    });
  };

  this.selectedGroup = function () {
    browser.wait(until.visibilityOf(this.groupsGrid), waitTimeout);
    $('.slick-row:first-child > .slick-cell:nth-child(1)').click();
  };
};

module.exports = createGroupPom;
