'use strict';
import NodeDetailsUnderConfig from './node_details_under_config_page';
import SiteSelectionPage from '../sitePanel/site_selection.page';

describe('Check if the Labels are present in the Node panel under Config Section ', () => {

    var nodeDetailsUnderConfig = new NodeDetailsUnderConfig();
    var selectSite = new SiteSelectionPage();


    it("Find the Panel Group and expand the config panel", ()=>{
        nodeDetailsUnderConfig.expandConfig();
    });

    it("Check if the Groups and Profiles Label is visible", ()=>{
        nodeDetailsUnderConfig.checkGroupsAndProfilesLabel()
        //browser.sleep(2000);
    });

    it("Check if the Fixture Label is visible", ()=>{
        nodeDetailsUnderConfig.checkFixtureLabel();
        //browser.sleep(2000);
    });

    it("Check if the Location Label is visible", ()=>{
        nodeDetailsUnderConfig.checkLocationLabel();
    });

    it("Check if the Hardware Label is visible", ()=>{
        nodeDetailsUnderConfig.checkHardwareLabel();
    });

    it("Check if the Lightning Control Label is visible", ()=>{
        nodeDetailsUnderConfig.checkLightningControlLabel();
    });

    it("Check if the Firmware Label is visible", ()=>{
        nodeDetailsUnderConfig.checkFirmwareLabel();
    });

    it("Check if the Configuration Management Label is visible", ()=>{
        nodeDetailsUnderConfig.checkConfigurationManagementLabel();
    });

    it("Check if the Reset Label is visible", ()=>{
        nodeDetailsUnderConfig.checkResetnLabel();
    });

});
