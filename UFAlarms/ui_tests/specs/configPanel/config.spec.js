'use strict';
import ConfigPage from './config.page';
import { configVariable as configVariable } from '../../variables';
import SiteSelectionPage from '../sitePanel/site_selection.page';

describe('Config Creation Screen', () => {
    var configPage = new ConfigPage();
    var selectSite = new SiteSelectionPage();

    it("Click Config Link", () => {
        selectSite.getMoreOptions();
        selectSite.clickConfig();
        browser.wait(until.stalenessOf(configPage.notyContainer), waitTimeout);
    });

    it('Click the Add Config Button to add a New Config', () => {
        browser.wait(until.visibilityOf(configPage.addNewConfigButton), waitTimeout);
        configPage.addNewConfigButton.click();
    });

    it('Click the Apply Config button to add further details', () => {
        browser.wait(until.visibilityOf(configPage.selectDropdown), waitTimeout);
        configPage.chooseNewModel();
        configPage.applyConfigButton.click();
    });

    it('Fill config form details', () => {
        browser.wait(until.visibilityOf(configPage.formName), waitTimeout);
        configPage.fillConfigForm(configVariable);
    });

    it('Save the config details and wait for the pop up', () => {
        configPage.saveConfigButton.click();
        browser.wait(until.visibilityOf(configPage.popUp), waitTimeout);       
    });

    it('Search grid for added Config', () => {
        configPage.searchAddedConfig(configPage.formName.getText());
    });

    it('Route back to sites page', () => {
        browser.navigate().back();
    })

    it("Route back to Proximity Dimming Page ", () => {
        selectSite.getMoreOptions();
        browser.wait(until.stalenessOf(configPage.notyContainer), waitTimeout);
        selectSite.clickProximityDimmingLink();
    });

});
