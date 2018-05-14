'use strict';
import CommissionPage from './commission_panel.page';
import { commissionVariable as commissionVariable } from '../../variables';

describe('New Commissioning Panel Screen', () => {
    var commissionPage = new CommissionPage();

    it('Should Select Nodes And Commission It', () => {
        commissionPage.multiSelect();
    });

    it('Should Click Button To Commission', () => {
        commissionPage.commissionIt();
    });

    it('Should Select An Site', () => {
        commissionPage.selectSite();
        // commissionPage.selectSite(1);
    });

    it('Should wait for the pop up', () => {
        browser.wait(until.visibilityOf(commissionPage.popUp), waitTimeout);
    });
});
