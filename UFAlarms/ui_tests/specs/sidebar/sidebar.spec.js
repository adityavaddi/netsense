'use strict';
import SidebarPage from './sidebar.page';

describe('Sidebar Click Icons test Suite', () => {

    var sidebarPage = new SidebarPage();

    it('Pull the Customer page up and click the site ', ()=>{
        browser.sleep(5000);
    });

    it('Click the icon', ()=>{
        $('.sidebar-nav > li:nth-child(2) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(3) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(4) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(5) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(6) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(7) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(8) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(9) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(10) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(11) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(12) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(13) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(14) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(15) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(1) a .rubix-icon').click();
        $('.sidebar-nav > li:nth-child(16) a .rubix-icon').click();
        browser.sleep(10000);
    });

});
