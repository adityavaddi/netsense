'use strict';

var Page = function () {
  // elements
  this.logoutBtn = $("#logoutButton");

  this.popUp = $('.noty_text');
  this.popUpMessage = $('.noty_message');
  this.popUpClose = $('.noty_close');
  this.rightNav = $('#rightNav');
  this.hoverOver = $('.accountNav');
  this.link = $('#logoutLink');

  this.notyContainer = $('#noty_topRight_layout_container');

  // functions
  this.logout = function () {
    browser.wait(until.visibilityOf(this.rightNav), waitTimeout);
    this.hoverOver.click(); 
    this.link.click(); 
    browser.sleep(5000);  
  }
};

module.exports = Page;