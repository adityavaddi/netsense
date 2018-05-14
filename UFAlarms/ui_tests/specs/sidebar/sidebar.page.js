'use strict';

const SidebarPage = function () {
    // elements
    this.toggleButton = $('#step-direction');

    this.form_name = $("#name");
    this.configGrid = $('#Config-grid');

    this.customersIcon = $('.sidebar-nav > li:nth-child(1) a .rubix-icon');
    this.sitesIcon = $('.sidebar-nav > li:nth-child(2) a .rubix-icon');
    this.schedulesIcon = $('.sidebar-nav > li:nth-child(3) a .rubix-icon');
    this.groupsIcon = $('.sidebar-nav > li:nth-child(4) a .rubix-icon');
    this.nodesIcon = $('.sidebar-nav > li:nth-child(5) a .rubix-icon');
    this.auditsIcon = $('.sidebar-nav > li:nth-child(6) a .rubix-icon');
    this.reportingIcon = $('.sidebar-nav > li:nth-child(7) a .rubix-icon');
    this.energyIcon = $('.sidebar-nav > li:nth-child(8) a .rubix-icon');
    this.daylightHarvestingIcon = $('.sidebar-nav > li:nth-child(9) a .rubix-icon');
    this.proximityDimmingIcon = $('.sidebar-nav > li:nth-child(10) a .rubix-icon');
    this.notificationsIcon = $('.sidebar-nav > li:nth-child(11) a .rubix-icon');
    this.fixturesIcon = $('.sidebar-nav > li:nth-child(12) a .rubix-icon');
    this.firmwareIcon = $('.sidebar-nav > li:nth-child(13) a .rubix-icon');
    this.commissioningIcon = $('.sidebar-nav > li:nth-child(14) a .rubix-icon');
    this.configIcon = $('.sidebar-nav > li:nth-child(15) a .rubix-icon');
    // this.userManagementIcon = $('.sidebar-nav > li:nth-child(16) a .rubix-icon');
    this.userManagementIcon = $('.sidebar-nav a .icon-fontello-users-1');
    this.goToCustomer = function () {
        this.customersIcon.click();
    };

    this.goToSites = function () {
        this.sitesIcon.click();
    };

    this.goToConfig = function () {
        this.configIcon.click();
    };

    this.goToFixtures = function () {
        this.fixturesIcon.click();
    };

    this.goToAudits = function () {
        this.auditsIcon.click();
    };

    this.goToNotifications = function () {
        this.notificationsIcon.click();
    };

    this.goToUserManagement = function () {
        this.userManagementIcon.click();
    };

    this.goToGroups = function () {
        this.groupsIcon.click();
    };

    this.goToProximityPanel = function(){
        this.proximityDimmingIcon.click();
    };

};

export { SidebarPage };
