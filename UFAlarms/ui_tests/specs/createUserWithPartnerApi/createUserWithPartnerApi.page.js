'use strict';

import { userWithPartnerApi as userWithPartnerApi } from '../../variables';

var UserWithPartnerApiPage = function () {
    // elements

    this.addNewActiveUser = $('#addUser');
    this.formName = $('#name');
    this.email = $('#email');
    this.title= $('#title');
    this.phone = $('#phone');
    this.selectRolesDropDown = $('#roles');
    this.saveUser = $('#saveUser');
    this.userGrid = $('#Usermanagement-grid');
    this.customerGrid = $('#Customer-grid');
    this.popUp = $('.noty_text');
    this.closeIcon = $('#userManagementoverlayClose');
    this.userManagementLink = $('#userManagementLink');

    this.generateApiKeybutton = $('#generateApiKeybutton');


    this.chooseRole = function(){
        this.selectDropdown.sendKeys("partner_api");
    };

    this.fillUserForm = function (data) {
        this.formName.sendKeys(data.name);
        this.email.sendKeys(data.email);
        this.title.sendKeys(data.title);
        this.phone.sendKeys(data.phone);
        this.selectRolesDropDown.sendKeys(data.role);
    };

    this.searchAddedUser = function (data) {
        browser.wait(until.visibilityOf(this.userGrid), waitTimeout);
        $('.slick-headerrow-column:first-child input').sendKeys(data.name);
        $('.slick-row:first-child > .slick-cell:first-child').getText().then((data) => {
            expect(data).to.equal(data);
        });
    }

    this.isUserCreated = function () {
        browser.wait(until.visibilityOf(this.userGrid), waitTimeout);
        this.popUp.getText().then(function (text) {
            expect(text).to.equal("added");
        });
    };

    this.isUserKeyGenerated = function () {
        browser.wait(until.visibilityOf(this.userGrid), waitTimeout);
        this.popUp.getText().then(function (text) {
            expect(text).include("API key is ");
        });
    };

    this.checkPopUpText = function(){
        this.popUp.getText().then(function (text) {
            expect(text).include("added");

        });
    }


};

module.exports = UserWithPartnerApiPage;
