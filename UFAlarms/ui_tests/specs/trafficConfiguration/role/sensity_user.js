import Page from '../page';
import { trafficConfigData } from '../../../variables';
import SiteSelectionPage from '../../sitePanel/site_selection.page';

describe('Traffic Configuration Page', () => {
    var trafficConfigPage = new Page();
    var selectSite = new SiteSelectionPage();

    it("Route to Traffic Configration page", () => {
        selectSite.getMoreOptions();
        selectSite.clickTrafficConfig();
    });

    it('Verify page title', () => {
        browser.wait(until.visibilityOf(trafficConfigPage.trafficConfigTitle), waitTimeout);
        trafficConfigPage.trafficConfigTitle.getText().then((text) => {
            expect(text).to.equal('Traffic Configuration');
        })
    });
});
