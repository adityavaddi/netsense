import SidebarPage from '../sidebar/sidebar.page';

const groupsPage = function() {
  this.groupGrid = $('#Group-grid');
  this.searchByName = element(by.xpath('//*[@id="Group-grid"]/div[3]/div[1]/div[2]/input')),
  this.selectedFirstRow = element(by.xpath('//*[@id="Group-grid"]/div[5]/div/div/div[3]')),
  this.schedDropdown = $('#xschedule'),
  this.PDprofileDropDown = $('#pdprofile'),
  this.saveGroupBtn = $('#save-group-button'),

  this.showHideDetailsButton = $('#showDetailsGroups');
  this.gropsDetailPanel = $('#group-detail-panel h2');


  // Configuaration to select a customer and site and go to groups -start
  this.customerGrid = $('#Customer-grid');
  this.siteTilesContainer = $('#site-tiles-container');

  //functions
  this.selectSched = function(scheduleName) {
    element(by.cssContainingText('option', scheduleName)).click();
  }

  this.clearSearchArea = function(){
    this.searchByName.clear();
  };

  this.selectPDProfile = function(PDProfileName) {
    element(by.cssContainingText('option', PDProfileName)).click();
  }

  this.selectGroup = function(groupName) {
    this.searchByName.sendKeys(groupName);
    $('#Group-grid .grid-canvas > .slick-row:first').click();
    this.showHideDetailsButton.click();
    browser.wait(until.visibilityOf(this.gropsDetailPanel), waitTimeout);
  }

  this.getSched = function() {
    return this.schedDropdown.$('option:checked').getText()
  }

  this.getPDProfile = function() {
    return this.PDprofileDropDown.$('option:checked').getText()
  }

  this.clickSaveGroup = function() {
    browser.actions().mouseMove(this.saveGroupBtn).perform();
    this.saveGroupBtn.click();
  }
};


// Export New Instance of the Page for each Create and Check Apply to Node
module.exports = new groupsPage();
