import Page from '../page';
import { variable as input } from '../../../variables';
import { loginData } from '../mockData';

describe('Login as Partner Admin', () => {
    var loginPage = new Page();

    beforeEach(() => {
        browser.get(input.baseUrl);
    });

    it('User login as Partner Admin', () => {
        browser.wait(until.visibilityOf(loginPage.username), waitTimeout);
        loginPage.login(loginData.username_partner_admin, loginData.password_partner_admin);
    });
});
