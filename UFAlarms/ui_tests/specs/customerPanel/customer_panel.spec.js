'use strict';
import Page from './customer_panel.page';
import { singleCustomerTestExecution as singleCustomerTestExecution } from '../../variables';
import SiteSelectionPage from '../sitePanel/site_selection.page';


describe('Select a site through site tile', () => {

  var customerPanelPage = new Page();
  // var SiteSelectionPage = new SiteSelectionPage();
  var selectSite = new SiteSelectionPage();

  // Search for the customer
  it('Search grid for added customer', () => {
    customerPanelPage.searchCustomer(singleCustomerTestExecution);
  });

  // Click on the site icon for the selected customer
  it('Click the site icon of the selected customer', () => {
    customerPanelPage.routeToSites();
  });

  it("Route to the respective page from Sites Panel page", ()=>{
    selectSite.getMoreOptions();
    selectSite.clickNodesLink();
  });


});

