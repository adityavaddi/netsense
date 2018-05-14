'use strict';

const sensity_user = [

    /* Login as sensity user*/
    './specs/login/role/sensity_user.js',

    // create customer as sensity user
    './specs/createCustomer/role/createCustomer.js',

    // Specific Spec to run for each panel
    './specs/createSite/createSite.spec.js', // API calls to create a Node
    './specs/configPanel/config.spec.js',
    './specs/pdProfile/create.pd.profile.spec.js',
    './specs/fixturePanel/fixture.spec.js',

    './specs/createGroup/createGroup.js',
    './specs/pdToGroup/apply.pd.to.group.spec.js',

    './specs/nodeNameChange/node_name_change_spec.js',
    './specs/verifyLabelsOnNodePage/verify_labels_nodes_spec.js',
    './specs/nodeDetailsUnderConfig/node_details_under_config_spec.js',
    './specs/fixtureToNode/fixture.to.node.spec.js',
    './specs/checkAudits/checkAudit.spec.js',
    // Need to Create a user in the user management tab to have the notification work as expected
    // As a notification needs an email/phone and if not that test case fails
    './specs/userManagement/end_user.spec.js',
    './specs/createNotification/createNotification.js',
    './specs/assignScheduleToNode/assignScheduleToNode.spec.js',
    './specs/reportingPanel/reporting.spec.js',
    //'./specs/dayLightHarvesting/daylightHarvesting.spec.js',

    // Logout
    './specs/logout/logout.js'
];
module.exports = sensity_user;


//  Other test cases that dont have data right now...
// Parking Groups test Cases (Runs only in qascale2)
// './specs/parkingGroups/parkingGroups.spec.js',

// Parking Zones test Cases (Runs only in qascale2)
// './specs/parkingZones/parkingZones.spec.js',

// Traffic Configuration
// './specs/trafficConfiguration/role/sensity_user.js',
