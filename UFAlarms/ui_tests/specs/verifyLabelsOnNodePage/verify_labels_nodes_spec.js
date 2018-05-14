'use strict';
import LabelsOnNodesPage from './verify_labels_nodes_page';
import SiteSelectionPage from '../sitePanel/site_selection.page';

describe('Check if the Labels are present in the Node panel ', () => {

    var labelsOnNodesPage = new LabelsOnNodesPage();
    var selectSite = new SiteSelectionPage();

    // it("Wait for Sites page to come up and Click Nodes Link", ()=>{
    //     browser.wait(until.visibilityOf(labelsOnNodesPage.siteTilesContainer), 9000);
    //     selectSite.clickNodesLinkSecond();
    // });
    //
    // it("Select the First node in the Node Grid", ()=>{
    //     browser.wait(until.visibilityOf(labelsOnNodesPage.nodeGrid), waitTimeout);
    //     $('.slick-row:first-child').click();
    // });
    //
    it("Wait for the pullout to be visible and Check if the Basic Identifiers Label is visible", ()=>{
        // browser.wait(until.visibilityOf(labelsOnNodesPage.viewScheduleButton), waitTimeout);
        labelsOnNodesPage.checkBasicIdentifierLabel();
    });

    it("Check if the Config Label is visible", ()=>{
        labelsOnNodesPage.checkConfigLabel()
    });

    it("Check if the Reports Label is visible", ()=>{
        labelsOnNodesPage.checkReportsLabel()
    });

    it("Check if the Alerts and Alarms Label is visible", ()=>{
        labelsOnNodesPage.checkAlertsAndAlaramsLabel();
    });
});