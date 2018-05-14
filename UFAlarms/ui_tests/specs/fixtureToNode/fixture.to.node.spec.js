'use strict';
import FixtureToNodePage from './fixture.to.node.page';
// import CustomerPage from '../customer/customer.page';
import SiteSelectionPage from '../sitePanel/site_selection.page';
import {configVariable as configVariable} from '../../variables';

describe('Assign the Fixture to the Node Suite', () => {

    var fixtureToNodePage = new FixtureToNodePage();
    var selectSite = new SiteSelectionPage();

    it("Expand the Fixture Panel", ()=>{
         // $('#fixtureLabel').click();
        $('#collapseTwonested .panel.panel-default:nth-child(3) #fixtureLabel').click();
    });

    it("Check the current Fixture Label", ()=>{
        fixtureToNodePage.currentFixtureText.getText().then(function () {
            assert.notEqual('Not Set');
        });
    });

    it("Switch the Current Fixture to a new Fixture from the dropdown", ()=>{
        fixtureToNodePage.chooseAnotherOption();
        fixtureToNodePage.setFixtureButton.click();
    });

    it('Route back to sites page', () => {
        browser.navigate().back();
    })

    it("Click Audits Link", () => {
        selectSite.getMoreOptions();
        selectSite.clickAudits();
    })

});
