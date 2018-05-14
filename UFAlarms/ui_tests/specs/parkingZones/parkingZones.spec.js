'use strict';

import ParkingZonesPage from './parkingZones.page';
import SiteSelectionPage from '../sitePanel/site_selection.page';

describe("Parking Zones UX panel ", () => {

    var selectSite = new SiteSelectionPage();
    var parkingZonesPage = new ParkingZonesPage();

    it("Click ParkingZones Link", () => {
        selectSite.getMoreOptions();
        selectSite.clickParkingZones();
    });

    it('Should wait for the panel and elements to display', () => {
        browser.wait(function () {
            return element(by.css("#parkingZone-list-panel")).isPresent()
        });
    });

    it("Click ParkingZones Link", () => {
        parkingZonesPage.onlyClick.click();
    });
});
