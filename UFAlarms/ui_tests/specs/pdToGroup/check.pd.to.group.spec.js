'use strict';
import { groupVariable } from '../../variables';
import groupsPage from './pd.to.group.page.js'
import SiteSelectionPage from '../sitePanel/site_selection.page.js';

describe('Check Proximity Dimming Profile . . .', () => {

    var selectSite = new SiteSelectionPage();

    // it("expand More and route to Group Profile Link", () => {
    //     selectSite.getMoreOptions();
    //     selectSite.clickGroupsLink();
    // });

    it('Should Search for Group ' + groupVariable.groupName, () => {
        browser.wait(until.visibilityOf(groupsPage.groupGrid), waitTimeout);
        groupsPage.selectGroup(groupVariable.groupName)
    })

     it('Proximity dimming profile should be ' + groupVariable.PDProfileName, () => {
        groupsPage.getPDProfile().then(function(value) {
            expect(value).to.equal(groupVariable.PDProfileName)
        })
    })

})