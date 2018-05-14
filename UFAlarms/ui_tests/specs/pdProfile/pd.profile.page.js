const PdProfilePage = function() {

  //  elements
  this.proximityGrid = $('#Proximity-grid');
  this.addSiteBtn = $("#add-Site");
  this.addAddPdProfileBtn = $('#add-profile'),
  this.profileName = $('#name'),
  this.profileDesc = $('#description'),
  this.minLevel = $('#minLevel'),
  this.maxLevel = $('#maxLevel'),
  this.beginTime = $('#beginTime'),
  this.endTime = $('#endTime'),
  this.detectionDuration = $('#detection_duration'),
  this.saveProfile = $('#submit'),
  this.searchByName = element(by.xpath('//*[@id="Proximity-grid"]/div[3]/div[1]/div[1]/input')),
  this.selectedFirstRow = element(by.xpath('//*[@id="Proximity-grid"]/div[5]/div/div[1]/div[1]')),

  this.notyContainer = $('#noty_topRight_layout_container');

  // Configuaration to select a customer and site and go to groups -start
   this.mockSiteCustomer = "Sensity Systems";
  this.customerGrid = $('#Customer-grid');
  this.siteTilesContainer = $('#site-tiles-container');

   //functions
  this.fillForm = function(pdVariable){
    this.profileName.sendKeys(pdVariable.name);
    this.profileDesc.sendKeys(pdVariable.desc);
    this.minLevel.sendKeys(pdVariable.minLevel);
    this.maxLevel.sendKeys(pdVariable.maxLevel);
    this.beginTime.sendKeys(pdVariable.beginTime);
    this.endTime.sendKeys(pdVariable.endTime);
    this.detectionDuration.sendKeys(pdVariable.detectionDuration)
  };

  this.getName = function() {
    return this.profileName.getAttribute('value')
  }

  this.getDesc = function() {
    return this.profileDesc.getAttribute('value')
  }

  this.selectPdProfile = function(PdProfileName) {
    this.searchByName.sendKeys(PdProfileName);
  }
};

module.exports = new PdProfilePage();
