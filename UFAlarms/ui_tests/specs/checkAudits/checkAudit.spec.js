'use strict';
import CheckAudit from './checkAudit.page';
import { input } from './mockData';
import SiteSelectionPage from '../sitePanel/site_selection.page';


describe('Check the Audit', () => {
  var checkAuditPageObject = new CheckAudit();
  var selectSite = new SiteSelectionPage();


  // Verify if Audit is reflecting in Audits list
  it('Make sure the Grid Is visible', () => {
       browser.wait(until.visibilityOf(checkAuditPageObject.auditGrid), waitTimeout);
  })

    it('Route back to sites page', () => {
        browser.navigate().back();
    })

    it("Click User Management Link", () => {
        selectSite.getMoreOptions();
        browser.wait(until.stalenessOf(checkAuditPageObject.notyContainer), waitTimeout);
        selectSite.clickUserManagement();
    })
});
