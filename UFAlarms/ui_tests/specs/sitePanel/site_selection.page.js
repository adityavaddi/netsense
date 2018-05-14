'use strict';

import { mockCustomer as mockCustomer } from '../../variables';

var SiteSelectionPage = function () {
    // elements
    this.siteTiles = $('#site-tiles-container');
    this.siteSingle = $('.netsense__site__single');
    this.singleTile = $('.site-tile');
    this.allElements
    this.popUp = $('.noty_text');
    this.popUpClose = $('.noty_close');


    this.getMoreOptions = function () {
        browser.wait(until.visibilityOf(this.siteTiles), waitTimeout);
        browser.wait(until.visibilityOf(this.singleTile), waitTimeout);
        browser.actions().mouseMove(element(by.css('.ns-navigate'))).perform();
    };

    // Common Utility function to access  different Links to be clicked
    this.commonUtilityClick = function (itemIndex) {
        this.allElements = element.all(by.css('.ns-navigationclass li'));
        this.link = this.allElements.get(itemIndex);
        browser.wait(until.visibilityOf(this.link), waitTimeout);
        this.link.click();
    };

    this.clickSchedulesLink = function () {
        this.commonUtilityClick('2')
    };

    this.clickGroupsLink = function () {
        this.commonUtilityClick('1')
    };

    this.clickNodesLink = function () {
        this.commonUtilityClick('0')
    };

    this.clickAudits = function () {
        this.commonUtilityClick('6')
    };

    this.clickReporting = function () {
        this.commonUtilityClick('16')
    };

    this.clickNotifications = function () {
        this.commonUtilityClick('13')
    };

    this.clickFixtures = function () {
        this.commonUtilityClick('12')
    };

    this.clickDayLightHarvesting = function () {
        this.commonUtilityClick('9')
    };

    this.clickProximityDimmingLink = function () {
        this.commonUtilityClick('14')
    };

    this.clickFirmware = function () {
        this.commonUtilityClick('11')
    };

    this.clickCommissioning = function () {
        this.commonUtilityClick('7')
    };

    this.clickConfig = function () {
        this.commonUtilityClick('8')
    };

    this.clickParkingGroups = function () {
        this.commonUtilityClick('3')
    };

    this.clickParkingZones = function () {
        this.commonUtilityClick('4')
    };

    this.clickTrafficConfig = function () {
        this.commonUtilityClick('5')
    };

    this.clickUserManagement = function () {
        this.commonUtilityClick('18')
    };

    // Close Noty
    this.closeNoty = function () {
        const that = this;
        that.popUp.isDisplayed().then(function (isVisible) {
            if (isVisible) {
                console.log('noty message visible');
                browser.actions().mouseMove(that.popUp).perform();
                that.popUpClose.click();
            }
        });
    }
    // // Special Cases:
    //
    // // Only for End User Admin - to login and choose a site that has nodes to verify the details
    // this.clickNodesLinkSecond = function () {
    //     // qastage 3
    //     //$('#site-tiles-container > div[data-siteid="ffa6f7d0-5b60-11e7-a191-07bbbcab6a12"] >.netsense__site--description .netsense__form--list .netsense__form--list--item:nth-child(3)').click();
    //
    //     // qa stage 3 - abu dhabi site
    //     $('#site-tiles-container > div[data-siteid="c8e750e0-09db-11e7-863d-dde2efee023e"] >.netsense__site--description .netsense__form--list .netsense__form--list--item:nth-child(3)').click();
    //
    // };



};

module.exports = SiteSelectionPage;
