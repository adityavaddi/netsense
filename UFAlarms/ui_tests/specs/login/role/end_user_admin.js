import Page from '../page';
import { variable as input } from '../../../variables';
import { loginData } from '../mockData';

describe('Login as End User Admin', () => {
    var loginPage = new Page();

    beforeEach(() => {
        browser.get(input.baseUrl);
    });

    it('User login as End User Admin', () => {
        browser.wait(until.visibilityOf(loginPage.username), waitTimeout);
        loginPage.login(loginData.username_end_user_admin, loginData.password_end_user_admin);
    });
});
