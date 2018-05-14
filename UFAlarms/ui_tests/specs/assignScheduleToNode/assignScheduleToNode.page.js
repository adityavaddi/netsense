'use strict';

import { userWithPartnerApi as userWithPartnerApi } from '../../variables';
import { nodename as nodename } from '../../variables';
import { nodeVariable1 as nodeVariable1 } from '../../variables';

var AssignScheduleToNodePage = function () {
    // elements
    this.groupsGrid = $('#Group-grid');
    this.groupNameInput = $('.slick-headerrow > .slick-headerrow-columns > .slick-headerrow-column.l1 input')
    this.firstRow = $('.slick-row:first-child');
    this.formName = $('#name');
    this.nodeList = $('#nodeList');
    this.scheduleDropDown = $('#xschedule');
    this.saveGroupsButton = $('#save-group-button');
    this.nodesGrid = $('#Node-grid');
    this.NodeName = "";
    this.popUp = $('.noty_text');
    this.accordion = $('#accordion');
    this.viewScheduleButton = $('#viewSchedule');
    this.schedulePopUp = $('.sched-display');
    this.configLabel = $('#configlabel');
    this.lightningControlLabel = $('#lightingControlLabel');
    this.scheduleNameLabel = $('#sched-heading');
    this.gropsDetailPanel = $('#group-detail-panel');

    this.groupName = "new group";
    this.scheduleName = "Site Lighting Group";
    this.showHideDetailsButtonGroups = $('#showDetailsGroups');
    this.showHideDetailsButtonNodes = $('#showHideDetails');



    this.searchGroupInGrid = function(){

        // @TODO: when new groud created then that same group name can be uncommnted and the groups created can be passed - until then passing the default group name
        // $('.slick-headerrow > .slick-headerrow-columns > .slick-headerrow-column.l1 input').sendKeys(nodeVariable1);
        $('.slick-headerrow > .slick-headerrow-columns > .slick-headerrow-column.l1 input').sendKeys(this.scheduleName);
        this.firstRow.click();
        this.showHideDetailsButtonGroups.click();
        browser.wait(until.visibilityOf(this.gropsDetailPanel), waitTimeout);
    };

    this.findAndGetNodeName = function(){
        this.NodeName = $('#nodeList > div:first-child').getText();
    };

    this.changeScheduleAndSave = function(){
        this.scheduleDropDown.sendKeys(this.scheduleName);
        this.saveGroupsButton.click();
    };

    this.searchNodeInGrid = function(){
        $('.slick-headerrow-columns> .slick-headerrow-column:nth-child(4) input').sendKeys(nodename);
        $('.slick-row > .slick-cell.l0.r0').click();
        browser.wait(until.visibilityOf(this.showHideDetailsButtonNodes), waitTimeout);
        this.showHideDetailsButtonNodes.click();
    };


};

module.exports = AssignScheduleToNodePage;
