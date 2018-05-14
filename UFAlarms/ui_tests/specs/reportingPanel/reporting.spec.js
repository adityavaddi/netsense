'use strict';
import ReportingPage from './reporting.page';
import { input } from './mockData';
import SiteSelectionPage from '../sitePanel/site_selection.page';

describe('New Reporting Panel Screen', () => {
    var reportingPage = new ReportingPage();
    var selectSite = new SiteSelectionPage();

    it('Should Select sensors and nodes', () => {
        reportingPage.fillForm(input);
    });

    it('Should wait for the looker panel in iframe', () => {
        browser.switchTo().frame(element(by.tagName('iframe')).getWebElement()).then(function () {
            browser.switchTo().defaultContent();
        });
    });

    it('Should be able to see real-time updates', () => {
        reportingPage.showUpdates.click();
    });

    it('Should close the modal and clear', () => {
        browser.wait(until.visibilityOf(reportingPage.sensorDisplayTime), waitTimeout).then(function () {
            // It is visible
            reportingPage.closeModal.click();
        });
    });

    it('Route back to sites page', () => {
        browser.navigate().back();
    });
});