'use strict';

const endUserAdminSuite = [
    // Login as End user admin
    'specs/login/role/end_user_admin.js',

    // the user once logged in is taken to the sites page so no routing needed from customer to site page
    // the first config test case already takes care of it
    './specs/configPanel/config.spec.js',
    './specs/pdProfile/create.pd.profile.spec.js',
    './specs/fixturePanel/fixture.spec.js',
    './specs/createGroup/createGroup.js',
    './specs/pdToGroup/apply.pd.to.group.spec.js',
    'specs/nodeNameChange/node_name_change_spec.js',
    'specs/verifyLabelsOnNodePage/verify_labels_nodes_spec.js',
    'specs/nodeDetailsUnderConfig/node_details_under_config_spec.js',
    './specs/fixtureToNode/fixture.to.node.spec.js',
    './specs/checkAudits/checkAudit.js',
    './specs/createNotification/createNotification.js',
    'specs/assignScheduleToNode/assignScheduleToNode.spec.js',
    'specs/reportingPanel/reporting.spec.js',
    'specs/createUserWithEndUserAPI/createUserWithEndUserApi.spec.js',
    'specs/dayLightHarvesting/daylightHarvesting.spec.js',

    // Logout
    'specs/logout/logout.js'
]
module.exports = endUserAdminSuite;
