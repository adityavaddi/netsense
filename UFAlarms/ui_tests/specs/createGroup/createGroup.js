'use strict';
import createGroupPom from './createGroupPom';
import { groups as groups } from '../../variables';
import SiteSelectionPage from '../sitePanel/site_selection.page.js';

describe('Create Group Under A Site', () => {
    var createGroup = new createGroupPom();
    var selectSite = new SiteSelectionPage();

    // Click on add Group button
    it('Should Pull up a new form to add a Group', () => {
        browser.wait(until.visibilityOf(createGroup.groupsGrid), waitTimeout);
        createGroup.addGroupBtn.click();
        browser.wait(until.visibilityOf(createGroup.gropsDetailPanel), waitTimeout);

        createGroup.form_name.getText().then(function (isNameEmpty) {
            assert.equal(isNameEmpty, '');
        });
    });

    it('Should Fill new Group form', () => {
        createGroup.fillForm(groups);
    });

    // // Hit save and check for pop-up message
    it('Should Hit save and check for pop-up message', () => {
        createGroup.form_saveGroup_btn.click()
        browser.wait(until.visibilityOf(createGroup.popUp), waitTimeout);
    });

});
