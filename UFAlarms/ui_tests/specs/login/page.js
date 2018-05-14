'use strict';
const Page = function () {
  // elements
  this.username = element(by.id('emailaddress'));
  this.password = $("#password");
  this.submitButton = element(by.xpath("//button[@type='submit']"));
  this.elementRole = $$('avatar-col')
  this.updatePasswordoverlayClose = $("#updatePasswordoverlayClose");
  this.navbar = $("#navbar");
  // functions
  this.login = function (a, b) {
    this.username.sendKeys(a);
    this.password.sendKeys(b);
    this.submitButton.click();
    var that = this;
    this.updatePasswordoverlayClose.isDisplayed().then(function (result) {
      if (result) {
        that.updatePasswordoverlayClose.click();
        that.submitButton.click();
      }
    }, function (err) {
      console.log('No Password Reset Dialog');
    });   

    // Try click on the submit button when login failed the first time 
    browser.wait(until.visibilityOf(this.navbar), waitTimeout).then(function () {
      // It is visible     
    }, function () {
      // It is not visible
      that.submitButton.click();
    });
  }
};
module.exports = Page;