'use strict';
import FixturePage from './fixture.page';
import { fixtureVariable as fixtureVariable } from '../../variables';
import SiteSelectionPage from '../sitePanel/site_selection.page';

describe('New Fixture Panel Screen', () => {
    var fixturePage = new FixturePage();
    var selectSite = new SiteSelectionPage();

    it('Click the Add Fixture Button to add a New Fixture', () => {        
        browser.wait(until.visibilityOf(fixturePage.addNewFixtureButton), waitTimeout).then(function () {
            // It is visible              
        }, function () {
            // It is not visible
           browser.refresh();
           browser.sleep(3000);                    
        });          
        browser.wait(until.elementToBeClickable(fixturePage.addNewFixtureButton), waitTimeout).then(function(){
            fixturePage.addNewFixtureButton.click(); 
        });
    });

    it('Expect Fixture Name to be Empty', () => {
        browser.wait(until.visibilityOf(fixturePage.formName), waitTimeout);
        fixturePage.formName.getText().then(function (isNameEmpty) {
            assert.equal(isNameEmpty, '');
        });
    });

    it('Fill Fixture form details', () => {
        fixturePage.fillFixtureForm(fixtureVariable);
    });

    it('Save the Fixture details and wait for the pop up', (done) => {
        fixturePage.saveFixtureButton.click();
        browser.wait(until.textToBePresentInElement(fixturePage.notyContainer, 'Fixture'), waitTimeout).then(function () {
            // It is visible  
            done();
        });      
    });

    it('Route back to sites page', () => {
        browser.navigate().back();
    });

    it("Route back to Groups Page ", () => {
        selectSite.getMoreOptions();
        browser.wait(until.stalenessOf(fixturePage.notyContainer), waitTimeout);
        selectSite.clickGroupsLink();
    });
});