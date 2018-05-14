'use strict';
import * as input  from '../variables';
import * as API from './login';

describe('login with API', function () {
    it('Sensity user Login API', function () {
        var api = API;
        beforeAll(function () {
            api.login();
        });
    });
});