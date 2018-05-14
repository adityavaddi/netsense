'use strict';

import {mockCustomer as mockCustomer} from '../../variables';
import {fixtureVariable as fixtureVariable} from '../../variables';

const FixtureToNodePage = function () {
    // elements
    this.formName = $('#name');
    this.customerGrid = $('#Customer-grid');
    this.nodeGrid = $('#Node-grid');
    this.siteTilesContainer = $('#site-tiles-container');
    this.popUp = $('.noty_text');
    this.accordion = $('#accordion');
    this.changeFixtureDropdown = $('#changefixture');
    this.setFixtureButton = $('#setFixture');
    this.currentFixtureText = $('#currentFixture');
    this.panelGroup = $('.panel-group');
    this.popUp = $('.noty_text');
    this.fixturePanel = $('#fixtureLabel');

    this.expandConfig = function(){
        browser.wait(until.visibilityOf(this.panelGroup), waitTimeout);
      $('.panel-group > .panel-default:nth-child(2)').click();
        // browser.wait(until.visibilityOf(this.popUp), waitTimeout);
        // $('.noty_bar').click();
        // $('.noty_close').click();
    };

    this.expandFixtureInsideConfig = function(){
        browser.wait(until.visibilityOf('#fixtureLabel'), waitTimeout);
        $('#fixtureLabel').click();
    };

    this.chooseAnotherOption = function(){
         browser.wait(until.visibilityOf(this.changeFixtureDropdown), waitTimeout);
        this.changeFixtureDropdown.sendKeys(fixtureVariable.name);
    };

};

module.exports = FixtureToNodePage;
