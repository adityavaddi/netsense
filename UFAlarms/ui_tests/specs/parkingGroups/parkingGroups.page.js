'use strict';

var ParkingGroupsPage = function () {
    // elements
    this.panelGrid = $('#parkingGroup-list-panel');
    this.addBtn = $('.pulse');
    this.formName = $('#name');
    this.description = $('#description');
    this.saveIt = $('#btn-save');
    this.popUp = $('.noty_text');
    this.searchByName = element(by.xpath("//*[@id='Parkinggroup-grid']/div[3]/div[1]/div[1]/input"));
    //*[@id="Parkinggroup-grid"]/div[3]/div[1]/div[1]/input

    this.fillUserForm = function (data) {
        this.formName.sendKeys(data.name);
        this.description.sendKeys(data.description);
    };

    this.searchParkingGroup = function () {
        browser.wait(until.visibilityOf(this.searchByName), waitTimeout);
        this.searchByName.sendKeys(data.name);
    }
};

module.exports = ParkingGroupsPage;