'use strict';

// import SidebarPage from '../sidebar/sidebar.page';
import { mockCustomer as mockCustomer } from '../../variables';

const createSitePom = function () {

    // var sidebarPage = new SidebarPage();
    // elements
    this.addSiteBtn = $("#add-Site");
    this.form_name = $("#name");
    this.form_street1 = $("#street1");
    this.form_street2 = $("#street2");
    this.form_contactName = $("#contact_name");
    this.form_contactPhone = $("#contact_phone");
    this.form_contactEmail = $("#contact_email");
    this.form_city = $("#city");
    this.form_state = $("#state");
    this.form_postalCode = $("#postal_code");
    this.form_country = $("#country");
    this.form_latitude = $("#latitude");
    this.form_longitude = $("#longitude");
    this.form_saveSite_btn = $("#saveSite");
    this.form_cancelSite_btn = $("#cancelSite");
    this.form_validate_btn = $("#ns-validate-site");
    this.popUp = $('.noty_text');
    this.popUpClose = $('.noty_message .noty_close');

    this.notyContainer = $('#noty_topRight_layout_container');
    //Common
    this.customerGrid = $('#Customer-grid');
    this.siteTilesContainer = $('#site-tiles-container');
    this.mockSiteCustomer = "Sensity Systems";

    // functions
    this.fillForm = function (data) {
        this.form_name.sendKeys(data.name);
        this.form_street1.sendKeys(data.street1);
        this.form_street2.sendKeys(data.street2);
        this.form_contactName.sendKeys(data.contact_name);
        this.form_contactPhone.sendKeys(data.contact_phone);
        this.form_contactEmail.sendKeys(data.contact_email);
        this.form_city.sendKeys(data.city);
        this.form_state.sendKeys(data.state);
        this.form_postalCode.sendKeys(data.postal_code);
        this.form_country.sendKeys(data.country);
        this.form_latitude.sendKeys(data.latitude);
        this.form_longitude.sendKeys(data.longitude);
        this.form_validate_btn.click();             
        browser.wait(until.visibilityOf(this.popUp), waitTimeout);   
        this.closeNoty();
    };

    this.routeToCustomer = function () {
        browser.wait(until.visibilityOf(this.customerGrid), waitTimeout);
        $('.slick-headerrow-columns > .slick-headerrow-column:first-child input').sendKeys(this.mockSiteCustomer);
        $('.slick-row:nth-child(1) > .slick-cell:nth-child(6)').click();
    };

    this.isSiteCreated = function () {
        this.popUp.getText().then(function (text) {
            expect(text).to.equal("added");
        });
        this.closeNoty();
    };

    this.isAddressValidated = function () {        
        this.popUp.getText().then(function (text) {
            expect(text).include("Found for");
        });
        this.closeNoty();
    };

    this.isAddressNotValidated = function () {
        this.popUp.getText().then(function (text) {
            expect(text).include("No match found for");
        });
    };

    // Close Noty
    this.closeNoty = function () {
        const that = this;
        that.popUp.isDisplayed().then(function (isVisible) {
            if (isVisible) {
                browser.actions().mouseMove(that.popUp).perform();                
                that.popUpClose.click();
            }
        });
    }
};

module.exports = createSitePom;
