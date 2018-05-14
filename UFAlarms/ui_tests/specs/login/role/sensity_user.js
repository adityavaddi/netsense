import Page from '../page';
import { variable as input } from '../../../variables';
import { loginData } from '../mockData';

describe('Login as Sensity User', () => {
    var loginPage = new Page();

    before(() => {
        browser.get(input.baseUrl);
        console.log('Running test cases in', input.baseUrl);
    });

    it("Check element visibilty and reload login page if element not visible", function(done) {
        this.timeout(60000); // 60 sec - To retry page load
        browser.wait(until.visibilityOf(loginPage.username), waitTimeout).then(function () {
            // It is visible 
            done();
        }, function () {
            // It is not visible
            browser.refresh();
            browser.wait(until.visibilityOf(loginPage.username), waitTimeout).then(function () {
                done();
            }, function () {
                done();
            });                    
        });
    });

    it('User login as Sensity User', () => {        
        loginPage.login(loginData.username_sensity_user, loginData.password_sensity_user);
    });
});
