'use strict';
import SidebarPage from '../sidebar/sidebar.page';
import { mockCustomer as mockCustomer } from '../../variables';

var Page = function () {

    // elements
    this.addNotificationBtn = $("#addNotification");

    // Form specific
    this.form_name = $('#name');
    this.form_description = $("#description");
    this.alarmType = $('form .form-group:nth-child(3) > .col-sm-6 > .ms-parent > button');
    this.alarmTypeSelect = $('.ms-select-all');

    this.severity = $('form .form-group:nth-child(4) > .col-sm-6 > .ms-parent > button');
    this.dropDown = $('form .form-group:nth-child(4) > .col-sm-6 > .ms-parent > .ms-drop');
    this.severityOptionSelect = $('form .form-group:nth-child(4) > .col-sm-6 > .ms-parent > .ms-drop > ul li:nth-child(4) > label > input');

    this.windowDays = $('form .form-group:nth-child(5) > .col-sm-6 > div > .ms-parent > button');
    this.daysOptionSelect = $('form .form-group:nth-child(5) > .col-sm-6 > div > .ms-parent > .ms-drop > ul li:nth-child(4) > label > input');

    this.fromTime = $('#windowtimemin');
    this.toTime = $('#windowtimemax');

    this.holdOff = $('#hold_off');
    this.interval = $('#resend_interval');
    this.message = $('#msg');

    this.email = $('form .form-group:nth-child(10) > .col-sm-6 > .ms-parent > button');
    this.dropDown = $('form .form-group:nth-child(10) > .col-sm-6 > .ms-parent > .ms-drop');
    this.emailSelect = $('form .form-group:nth-child(10) > .col-sm-6 > .ms-parent > .ms-drop > ul li:nth-child(2) > label > input');

    this.additionalMail = $('#additionalEmails');

    this.phone = $('form .form-group:nth-child(12) > .col-sm-6 > .ms-parent > button');
    this.dropDown = $('form .form-group:nth-child(12) > .col-sm-6 > .ms-parent > .ms-drop');
    this.phoneSelect = $('form .form-group:nth-child(12) > .col-sm-6 > .ms-parent > .ms-drop > ul li:nth-child(2) > label > input');

    this.saveBtn = $("#saveNotification");
    this.popUp = $('.noty_text');
    this.closeModal = $("#notificationOverlayClose");


    // functions
    this.fillForm = function (data) {
        this.form_name.sendKeys(data.name);
        this.form_description.sendKeys(data.description);
        this.alarmType.click();
        this.alarmTypeSelect.click();
        this.alarmType.click();

        this.severity.click();
        this.severityOptionSelect.click();
        this.severity.click();


        this.windowDays.click();
        this.daysOptionSelect.click();
        this.windowDays.click();

        this.fromTime.click();
        this.fromTime.sendKeys(data.fromTime);
        browser.actions().sendKeys(protractor.Key.ENTER).perform();

        this.toTime.click();
        this.toTime.sendKeys(data.toTime);
        browser.actions().sendKeys(protractor.Key.ENTER).perform();

        this.holdOff.sendKeys(data.holdOffValue);
        this.interval.sendKeys(data.interval);
        this.message.sendKeys(data.message);

        this.additionalMail.sendKeys(data.additionalMail);

    };
};

module.exports = Page;
