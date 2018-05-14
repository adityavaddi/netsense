'use strict';

var ReportingPage = function () {
  this.showUpdates = $('#showUpdates');
  this.closeModal = $('#closeModal');
  this.sensorDisplay = $('.sensor-display');
  this.sensorDisplayTime = $('#sensorDisplayTime');

  this.fillForm = function (data) {

    browser.wait(until.visibilityOf($('.datetimepicker-from')), waitTimeout);
    $('#sensor-y1').sendKeys('Driver Level');
    $('#sensor-y2').sendKeys('Voltage');
    $('#nodeList').sendKeys('New Name 2');
  };
};

module.exports = ReportingPage;