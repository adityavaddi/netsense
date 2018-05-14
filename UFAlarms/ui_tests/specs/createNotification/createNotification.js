'use strict';
import Page from './page';
import { input } from './mockData';
import SiteSelectionPage from '../sitePanel/site_selection.page';


describe('Create Notification', () => {
  var createNotificationPage = new Page();
  var selectSite = new SiteSelectionPage();

  it('Should Pull up a new form to add Notification', () => {    
    browser.wait(until.visibilityOf(createNotificationPage.addNotificationBtn), waitTimeout).then(function () {
      // It is visible
    }, function () {      
      // It is not visible
      browser.refresh();      
      browser.sleep(6000);
    });    
    browser.wait(until.elementToBeClickable(createNotificationPage.addNotificationBtn), waitTimeout).then(function(){
      createNotificationPage.addNotificationBtn.click();
    });
  });

  it('Should Fill new notification form', () => {
    createNotificationPage.fillForm(input);
  })

  it('Should Save the Notification and wait for the pop up', () => {
    createNotificationPage.saveBtn.click();
    browser.wait(until.visibilityOf(createNotificationPage.popUp), waitTimeout);
  });

  it('Route back to sites page', () => {
    browser.navigate().back();
  });

  it("Click Groups Link", () => {
    selectSite.getMoreOptions();
    selectSite.clickGroupsLink();
  })
});
