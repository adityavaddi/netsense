'use strict';
import SiteSelectionPage from './site_selection.page';


describe('As Sensity User routing to desired panel', () => {

  var siteSelectionPage = new SiteSelectionPage();

  // Click for more options
  it('Click More To See For Routes', () => {
    siteSelectionPage.getMoreOptions();
  });

  // // Only for testing Purposes
  //
  // it('Click Schedules link', () => {
  //   siteSelectionPage.clickSchedulesLink()
  // });
  //
  // it('Click Groups link', () => {
  //   siteSelectionPage.clickGroups();
  // });
  //
  // it('Click Nodes link', () => {
  //   siteSelectionPage.clickNodesLink();
  // });
  //
  // it("Click Audits Link", ()=>{
  //     siteSelectionPage.clickAudits();
  // });
  //
  // it("Click Reporting Link", ()=>{
  //   siteSelectionPage.clickReporting();
  // });
  //
  // it("Click Notification Link", ()=>{
  //   siteSelectionPage.clickNotification();
  // });
  //
  // it("Click Fixture Link", ()=>{
  //   siteSelectionPage.clickFixture();
  // });
  //
  // it("Click DayLight Harvesting Link", ()=>{
  //   siteSelectionPage.clickDayLightHarvesting();
  // });
  //
  // it("Click Proximity Dimming Link", ()=>{
  //   siteSelectionPage.clickProximityDimmingLink();
  // });
  //
  // it("Click Firmware Link", ()=>{
  //   siteSelectionPage.clickFirmware();
  // });
  //
  // it("Click Commissioning Link", ()=>{
  //   siteSelectionPage.clickCommissioning();
  // });
  //
  // it("Click Configurations Link", ()=>{
  //   siteSelectionPage.clickConfig();
  // });
  //
  // it("Click Parking Groups Link", ()=>{
  //   siteSelectionPage.clickParkingGroups();
  // });
  //
  // it("Click Parking Zones Link", ()=>{
  //   siteSelectionPage.clickParkingZones();
  // });
  //
  // it("Click Traffic Config Link", ()=>{
  //   siteSelectionPage.clickTrafficConfig();
  // });
  //
  // it("Click UserManagement Link", ()=>{
  //   siteSelectionPage.clickUserManagement();
  // });

});
