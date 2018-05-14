'use strict';

const adminSuite = [
    // Login as sensity admin
    'specs/login/role/sensity_admin.js',

    // Create a partner
    './specs/createCustomer/role/createPartner.js',

    // Create a partner admin -- dependency to login once email is received to the user account provided
    // NOTE: This test case is ony the User Creation and not resetting the login for the first time and changing the password
    // './specs/createCustomer/role/createPartnerAdmin.js',

    // Logout
    'specs/logout/logout.js'
]
module.exports = adminSuite;
