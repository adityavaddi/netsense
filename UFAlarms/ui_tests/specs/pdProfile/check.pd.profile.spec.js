'use strict';
import { pdVariable } from '../../variables';
import PdProfilePage from './pd.profile.page.js';
import SiteSelectionPage from '../sitePanel/site_selection.page.js';

describe('Checking Proximiy Dimming Profile just created . . .', () => {
    var selectSite = new SiteSelectionPage();

    it("expand More and route to Pd Profile Link", ()=>{
        selectSite.getMoreOptions();
        selectSite.clickProximityDimmingLink();
    })

     it('Should search for a ' + pdVariable.name + ' Profile', () => {
        browser.wait(until.visibilityOf(PdProfilePage.proximityGrid), waitTimeout);
        PdProfilePage.selectPdProfile(pdVariable.name)
    })

    it('Proximity dimming profile name should be equal to ' + pdVariable.name, () => {
        PdProfilePage.getName().then(function(value) {
            expect(value).to.equal(pdVariable.name)
        })
    })

    it('Proximity dimming profile description should be equal to ' + pdVariable.desc, () => {
        PdProfilePage.getDesc().then(function(value) {
            expect(value).to.equal(pdVariable.desc)
        })
    })
})