'use strict';

import AssignScheduleToNodePage from './assignScheduleToNode.page';
import SiteSelectionPage from '../sitePanel/site_selection.page';


describe("Assign Schedule To Node and check the node if assigned to new Schedule", ()=>{

    var assignScheduleToNodePage = new AssignScheduleToNodePage();
    var selectSite = new SiteSelectionPage();

    it("Should search for a group in the list and click it to expand the detail section", ()=>{
        browser.wait(until.visibilityOf(assignScheduleToNodePage.groupsGrid), waitTimeout);
        assignScheduleToNodePage.searchGroupInGrid();
    });

    it("Wait for the Left Detail Panel to Open up", ()=>{
        browser.wait(until.visibilityOf(assignScheduleToNodePage.formName), waitTimeout);
        assignScheduleToNodePage.findAndGetNodeName();
    });

    it("Should Change the Schedule and Save the Changes ", ()=>{
        assignScheduleToNodePage.changeScheduleAndSave();
    });

    it("Should wait for Pop and check if the Schedule is updated", ()=>{
        // Group "sample-group-name" updated.
        browser.wait(until.visibilityOf(assignScheduleToNodePage.popUp), waitTimeout);
        // assignScheduleToNodePage.popUp.getText().then(function (text) {
        //     expect(text).include("updated");
        // });
    });

    // it("Should Route back to Sites page", ()=>{
    //     browser.navigate().back();
    // });
    //
    // it("Now Route back to Nodes Page ", ()=>{
    //     selectSite.getMoreOptions();
    //     selectSite.clickNodesLink();
    // });
    //
    // it("Should search for a Node in the list and click it to expand the detail section", ()=>{
    //     browser.sleep(12000);
    //     browser.wait(until.visibilityOf(assignScheduleToNodePage.nodesGrid), waitTimeout);
    //     assignScheduleToNodePage.searchNodeInGrid();
    //
    // });
    //
    // it("Wait for the Detail Panel to be open and click the View Schedule button", ()=>{
    //     browser.wait(until.visibilityOf(assignScheduleToNodePage.viewScheduleButton), waitTimeout);
    //     assignScheduleToNodePage.viewScheduleButton.click();
    //     browser.wait(until.visibilityOf(assignScheduleToNodePage.scheduleNameLabel),waitTimeout);
    // });
    //
    // it("Should wait for the schedule to be visible match the Schedule name that was passed", ()=>{
    //     assignScheduleToNodePage.scheduleNameLabel.getText().then(function(text){
    //         expect(text).to.equal(assignScheduleToNodePage.scheduleName);
    //     });
    //
    // });

    it('Route back to sites page', () => {
        browser.navigate().back();
    })

    it("Click Reporting Link", () => {
        selectSite.getMoreOptions();
        selectSite.clickReporting();
    })

});
