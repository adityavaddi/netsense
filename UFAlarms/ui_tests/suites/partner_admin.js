'use strict'


const partnerAdminSuite = [

    // Login as Partner admin
    'specs/login/role/partner_admin.js',

    // Create customer
    'specs/createCustomer/role/createCustomer.js', //partnerAdminSuite
    // will need the same customer created to be used for the creating an end user admin user for the next spec  -

    // Create End user admin for the customer created under the partner
    // NOTE: This test case covers only Creating of the user not resetting the password
    './specs/userManagement/partner.admin.spec.js',

    // Now route back to the accounts page and back to the User Management to create User with Partner API
    'specs/createUserWithPartnerApi/createUserWithPartnerApi.spec.js',

    // Logout
    'specs/logout/logout.js'
]
module.exports = partnerAdminSuite;
