/**
 * Created by dejan on 12/12/15.
 * @example $ node ./privileges.js
 */
var PrivilegesV3_0_0 = {
    "PartnerModel" : [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createPartner"],
            "role": ["sensity_admin"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deletePartner"],
            "role": ["sensity_admin"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updatePartner"],
            "role": ["sensity_admin", "sensity_user", "partner_admin"]
        }, {
            "privilege": "CAN_SUSPEND",
            "operation": ["suspendPartner"],
            "role": ["sensity_user", "partner_admin"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getPartner", "getAllPartners"],
            "role": ["sensity_admin", "sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api"]
        },
    ],

    "OrgModel":[
        {
            "privilege": "CAN_CREATE",
            "operation": ["createOrg"],
            "role": ["sensity_admin", "sensity_user", "partner_admin", "partner_api"]
        }, {
            "privilege": "CAN_CHANGE",
            "operation": ["updateOrg"],
            "role": ["sensity_admin", "sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user"/*, "partner_api"*/, "end_user_admin","parking_owner_admin"]
        }, {
            "privilege": "CAN_SUSPEND",
            "operation": ["suspendOrg", "activateOrg"],
            "role": ["sensity_admin", "sensity_user", "partner_admin", "partner_api"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteOrg"],
            "role": ["sensity_admin", "sensity_user", "partner_admin", "partner_deployment_user", "partner_api"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getOrg", "getAllOrgs"],
            "role": ["sensity_admin", "sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_read_only", "end_user_api","parking_owner_admin", "policy_authority", "parking_manager", "end_user_lighting_user", "end_user_sensor_user"]
        },
    ],

    "SiteModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createSite"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_api"]
        }, {
            "privilege": "CAN_CHANGE",
            "operation": ["updateSite"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_api", "end_user_lighting_user", "end_user_sensor_user", "end_user_api","parking_owner_admin"]
        }, {
            "privilege": "CAN_SUSPEND",
            "operation": ["suspendSite", "activateSite", "deleteSite"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_api"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getSite", "getAllSites", "getAllSitesForOrg", "getAllSuspendedSitesForOrg"],
            "role": ["sensity_admin", "sensity_user", "sensity_read_only", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer", "parking_owner_admin", "policy_authority", "parking_manager"]
        },
    ],

    "UserModel":[
        {
            "privilege": "CAN_CREATE_ORG_USER",
            "operation": ["createUser"],
            "role": ["sensity_admin", "sensity_user", "partner_admin", "partner_api", "end_user_admin","parking_owner_admin"]
            //"role": ["sensity_admin", "sensity_user", "partner_admin", "partner_deployment_user", "partner_api", "end_user_admin"]
        }, {
            "privilege": "CAN_CHANGE_ORG_USER",
            "operation": ["updateUser"],
            "role": ["sensity_admin", "sensity_user", "partner_admin", "partner_api", "end_user_admin","parking_owner_admin"]
            //"role": ["sensity_admin", "sensity_user", "partner_admin", "partner_deployment_user", "partner_api", "end_user_admin"]
        }, {
            "privilege": "CAN_SUSPEND_ORG_USER",
            "operation": ["suspendUser", "activateUser"],
            "role": ["sensity_admin", "sensity_user", "partner_admin", "partner_api", "end_user_admin","parking_owner_admin"]
            //"role": ["sensity_admin", "sensity_user", "partner_admin", "partner_deployment_user", "partner_api", "end_user_admin"]
        }, {
            "privilege": "CAN_READ_ORG_USER",
            "operation": ["getUser"],
            "role": ["sensity_admin", "sensity_user", "sensity_read_only", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api","parking_owner_admin", "policy_authority", "parking_manager", "installer"]
            //"role": ["sensity_admin", "sensity_user", "sensity_read_only", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api"]
        }, {
            "privilege": "CAN_GENERATE_API_KEY",
            "operation": ["generateAPIKey"],
            "role": ["sensity_admin", "sensity_user", "partner_admin", "installer"]
        },
    ],


    "GroupModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createGroup"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_api", "parking_owner_admin", "end_user_networking_user"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateGroup", "addNodeToGroup", "removeNodeFromGroup"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_api","parking_owner_admin", "partner_deployment_user", "end_user_networking_user"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteGroup"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_api","parking_owner_admin", "end_user_networking_user"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getGroup", "getAllGroups"],
            "role": ["sensity_user", "sensity_read_only", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer","parking_owner_admin", "parking_manager", "partner_deployment_user", "policy_authority"]
        },
    ],

    "LicenseModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createLicense"],
            "role": ["sensity_user", "partner_admin", "end_user_admin"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getLicense"],
            "role": ["sensity_read_only", "sensity_user", "partner_admin", "end_user_admin"]
        }, {
            "privilege": "CAN_READ_USAGE",
            "operation": ["getLicenseUsage"],
            "role": ["sensity_user", "partner_admin", "end_user_admin"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteLicense"],
            "role": ["sensity_user", "partner_admin", "end_user_admin"]
        },
    ],

    "ScheduleModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createLightingSchedule"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateLightingSchedule"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteLightingSchedule"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getLightingSchedule", "getAllLightingSchedules"],
            "role": ["sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_api", "installer", "end_user_read_only", "partner_networking_user", "parking_manager"]
        }, {
            "privilege": "CAN_APPLY",
            "operation": ["applyScheduleToGroup", "applyScheduleToSite", "applyScheduleToNode"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api", "partner_read_only", "end_user_read_only"]
        },
    ],

    "NodeModel": [
        {
            "privilege": "CAN_ASSIGN_TO_LS_GROUP",
            "operation": ["assignLightingScheduleToGroup"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api"]
        }, {
            "privilege": "CAN_ASSIGN_LM_DETECTION_TO_GROUP",
            "operation": ["assignLightAndMotionDetectionToGroup"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api"]
        }, {
            "privilege": "CAN_MANAGE_LIGHTS",
            "operation": ["lighting-control-group", "lighting-control-site", "lighting-control"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api", "installer"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getNode", "getAllNodes", "getAllNodesForSite", "getNodeConnectionStatus", "getSiteNodesConnectionStatus"],
            "role": ["sensity_admin", "sensity_user", "sensity_read_only", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_api", "partner_read_only", "end_user_admin", "end_user_lighting_user", "end_user_read_only", "end_user_sensor_user", "end_user_api", "installer","parking_owner_admin", "policy_authority", "parking_manager", "end_user_networking_user" , "partner_deployment_user", "partner_networking_user"]
        }, {
            "privilege": "CAN_CREATE",
            "operation": ["createNode", "createEmptyNode"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "partner_api", "installer"]
        }, {
            "privilege": "CAN_CHANGE",
            "operation": ["updateNode"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "installer", "end_user_api"]
        }, {
            "privilege": "CAN_DEACTIVATE",
            "operation": ["deleteNode"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api", "installer"]
        }, {
            "privilege": "CAN_OPERATE",
            "operation": ["deleteNode"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api", "installer"]
        }, {
            "privilege": "CAN_ASSIGN_TO_ORGS",
            "operation": ["assignNodes"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api", "installer"]
        }, {
            "privilege": "CAN_ASSIGN_TO_SITES",
            "operation": ["assignNodes"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_api", "end_user_admin", "end_user_api", "installer", "sensity_admin"]
        }, {
            "privilege": "CAN_ASSIGN_TO_GROUPS",
            "operation": ["assignNodes"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api", "installer","parking_owner_admin", "policy_authority", "parking_manager"]
        }, {
            "privilege": "CAN_UPGRADE_FIRMWARE",
            "operation": ["upgradeFirmware"],
            "role": ["partner_admin", "partner_deployment_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api"]
        }, {
            "privilege": "CAN_ASSIGN_FIXTURE",
            "operation": ["assignFixture"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api", "installer", "partner_lighting_user", "end_user_sensor_user"]
        }, {
            "privilege": "CAN_SEARCH",
            "operation": ["searchForNodes"],
            "role": ["sensity_admin", "sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_read_only", "end_user_api","parking_owner_admin", "policy_authority", "parking_manager", "end_user_networking_user"]
        }, {
            "privilege": "CAN_RETRIEVE_LOG_FILE",
            "operation": ["retrieveLogFile"],
            "role": ["sensity_admin", "sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_read_only", "end_user_api", "installer"]
        }, {
            "privilege": "CAN_RETRIEVE_ALARM",
            "operation": ["retrieveAlarm"],
            "role": ["sensity_admin", "sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_read_only", "end_user_api", "installer", "end_user_networking_user"]
        },
    ],

    "ConfigModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["addConfig"],
            "role": ["partner_admin", "partner_api", "sensity_user"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateConfig"],
            "role": ["sensity_user", "partner_admin", "partner_api"]
        }, {
            "privilege": "CAN_DEACTIVATE",
            "operation": ["deleteConfig"],
            "role": ["sensity_user", "partner_admin", "partner_api"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getConfig", "getAllConfigs", "getDefaultConfigs", "updateVPNInfo", "connectToVPN", "disconnectFromVPN"],
            "role": ["sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_read_only", "partner_api", "end_user_networking_user","partner_networking_user", "parking_manager", "policy_authority", "sensity_admin"]
        }, {
            "privilege": "CAN_APPLY",
            "operation": ["applyConfigToSite", "applyConfigToNodes", "applyConfigToGroup"],
            "role": ["sensity_user", "partner_admin", "partner_api", "end_user_admin"]
        }
    ],

    "FixtureModel": [
        {
            "privilege": "CAN_READ",
            "operation": ["getFixture", "getAllFixtures"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api", "installer", "partner_deployment_user", "sensity_read_only", "partner_read_only", "end_user_read_only"]
        }, {
            "privilege": "CAN_CREATE",
            "operation": ["createFixture"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api", "installer"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateFixture"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api", "installer", "partner_deployment_user"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteFixture"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_api", "installer"]
        },
    ],

    // "ReportModel": [
    //     {
    //         "privilege": "CAN_CREATE",
    //         "operation": ["createReport"],
    //         "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_read_only", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_api"]
    //     }, {
    //         "privilege": "CAN_UPDATE",
    //         "operation": ["updateReport"],
    //         "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_read_only", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_api"]
    //     }, {
    //         "privilege": "CAN_DELETE",
    //         "operation": ["deleteReport"],
    //         "role": ["sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_read_only", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_api"]
    //     }, {
    //         "privilege": "CAN_READ",
    //         "operation": ["getReport", "getAllReports"],
    //         "role": ["sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_read_only", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_read_only", "end_user_api", "installer"]
    //     },
    // ],

    "AuditModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createAudit"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api","parking_owner_admin", "policy_authority", "parking_manager"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateAudit"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteAudit"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getAudit", "getAllAudits"],
            "role": ["sensity_admin", "sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_read_only", "end_user_api", "installer","parking_owner_admin", "end_user_networking_user", "installer", "partner_sensor_user", "end_user_sensor_user", "partner_networking_user"]
        },
    ],

    "FirmwareModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createFirmware"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_api"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateFirmware"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_api"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteFirmware"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_api"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getFirmware", "getAllFirmwares"],
            "role": ["sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_read_only", "end_user_admin", "end_user_lighting_user", "end_user_read_only", "end_user_api","parking_owner_admin", "installer", "partner_deployment_user", "partner_read_only", "end_user_read_only", "partner_lighting_user", "partner_networking_user", "parking_manager", "policy_authority"]
        },
    ],

    "NotificationModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createNotification"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "partner_api", "end_user_lighting_user", "end_user_api","parking_owner_admin", "end_user_networking_user", "installer", "end_user_sensor_user", "partner_lighting_user", "parking_manager"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateNotification"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "partner_api", "end_user_lighting_user", "end_user_api","parking_owner_admin", "end_user_networking_user", "installer", "end_user_sensor_user", "partner_lighting_user", "parking_manager"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteNotification"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "partner_api", "end_user_lighting_user", "end_user_api","parking_owner_admin", "end_user_networking_user", "installer", "end_user_sensor_user", "partner_lighting_user", "parking_manager"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getNotification", "getAllNotificationsForSite", "getNotificationTypes"],
            "role": ["sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_read_only", "end_user_api","parking_owner_admin", "end_user_networking_user", "installer", "partner_sensor_user", "end_user_sensor_user", "partner_networking_user", "partner_lighting_user", "parking_manager"]
        }, {
            "privilege": "CAN_ACTIVATE",
            "operation": ["activateNotification"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "partner_api", "end_user_lighting_user", "end_user_api","parking_owner_admin", "end_user_networking_user", "installer", "end_user_sensor_user", "partner_lighting_user", "parking_manager"]
        }, {
            "privilege": "CAN_DEACTIVATE",
            "operation": ["deactivateNotification"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "partner_api", "end_user_lighting_user", "end_user_api","parking_owner_admin", "end_user_networking_user", "installer", "end_user_sensor_user", "partner_lighting_user", "parking_manager"]
        },
    ],

    "ParkingZoneModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createParkingZone"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "partner_api", "end_user_api","parking_owner_admin", "policy_authority"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateParkingZone"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "partner_api", "end_user_api","parking_owner_admin", "policy_authority"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteParkingZone"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "partner_api", "end_user_api","parking_owner_admin", "policy_authority"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getParkingZone", "getAllParkingZones"],
            "role": ["sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_read_only", "end_user_api","parking_owner_admin", "policy_authority", "parking_manager", "sensity_read_only"]
        },
    ],

    "TrafficObjectModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createTrafficObject"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "partner_api","end_user_api"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateTrafficObject"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "partner_api", "end_user_api"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteTrafficObject"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "partner_api", "end_user_api"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getTrafficObject", "getAllTrafficObjects"],
            "role": ["sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_read_only", "end_user_api"]
        },
    ],

    "AlertModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createAlert"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api","parking_owner_admin", "end_user_sensor_user", "partner_lighting_user"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateAlert"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api","parking_owner_admin", "end_user_sensor_user", "partner_lighting_user"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteAlert"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api","parking_owner_admin", "end_user_sensor_user", "partner_lighting_user"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getAlert", "getAllAlerts"],
            "role": ["sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_read_only", "end_user_api","parking_owner_admin", "end_user_networking_user", "installer", "partner_sensor_user", "end_user_sensor_user", "partner_networking_user", "partner_lighting_user"]
        },
    ],

    "OverlayModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createOverlay"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateOverlay"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteOverlay"],
            "role": ["sensity_user", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getOverlay", "getAllOverlays"],
            "role": ["sensity_read_only", "sensity_user", "partner_admin", "partner_deployment_user", "partner_read_only", "end_user_admin", "end_user_lighting_user", "end_user_read_only", "end_user_api"]
        },
    ],

    "ParkingSpotModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createMetadataForParkingSpot"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_api", "parking_owner_admin", "policy_authority","parking_manager"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateMetadataForParkingSpot"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_api", "parking_owner_admin", "policy_authority","parking_manager"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteMetadataForParkingSpot"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_api", "parking_owner_admin", "policy_authority","parking_manager"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getMetadataForParkingSpot", "getAllMetadataForParkingSpot"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_api", "parking_owner_admin", "policy_authority","parking_manager", "sensity_read_only"]
        },
    ],

    "ParkingPolicyModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createParkingPolicy", "searchParkingPolicy"],
            "role": ["sensity_user","partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api", "parking_owner_admin", "policy_authority"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateParkingPolicy", "policyTagsAssociation"],
            "role": ["sensity_user","partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api", "parking_owner_admin", "policy_authority"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteParkingPolicy", "policyTagsDisassociation"],
            "role": ["sensity_user","partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api", "parking_owner_admin", "policy_authority"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getParkingPolicy", "getAllParkingPolicy", "getAllVersionsOfParkingPolicy", "getParkingPolicyVersion"],
            "role": ["sensity_user","partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api", "parking_owner_admin", "policy_authority", "parking_manager", "sensity_read_only"]
        },
    ],


    "PolicyCategoryModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createPolicyCategory"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api", "parking_owner_admin", "policy_authority"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updatePolicyCategory"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api", "parking_owner_admin", "policy_authority"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deletePolicyCategory"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api", "parking_owner_admin", "policy_authority"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getPolicyCategory", "getAllPolicyCategory"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api", "parking_owner_admin", "policy_authority", "parking_manager", "sensity_read_only"]
        },
    ],


    "ParkingGroupModel": [
        {
            "privilege": "CAN_UPDATE",
            "operation": ["policyAssociation"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_api", "parking_owner_admin", "parking_manager", "policy_authority"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["policyDisassociation"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_api", "parking_owner_admin", "policy_authority", "parking_manager"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["associatedParkingGroups"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_api", "parking_owner_admin", "policy_authority", "parking_manager", "sensity_read_only"]
        },
    ],




    "SummaryReportModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createSummaryReport"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api", "parking_owner_admin", "policy_authority"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateSummaryReport"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api", "parking_owner_admin", "policy_authority"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteSummaryReport"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api", "parking_owner_admin", "policy_authority"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getSummaryReport", "getAllSummaryReport"],
            "role": ["sensity_user","sensity_admin", "partner_admin", "partner_deployment_user", "end_user_admin", "end_user_lighting_user", "end_user_api", "parking_owner_admin", "policy_authority", "parking_manager"]
        },
    ],



    "BusinessAlertModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createBusinessAlert"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateBusinessAlert"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteBusinessAlert", "dismissBusinessAlert"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getBusinessAlert", "getAllBusinessAlerts", "filterBusinessAlert"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api", "sensity_read_only"]
        },
    ],



    "TriggerModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createTrigger"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateTrigger"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteTrigger"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getTrigger", "getAllTriggers", "filterTrigger"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api","sensity_read_only"]
        },
    ],

    "WhatIfAnalysisModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createWhatIfJob"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["abortWhatIfJob", "updateWhatIfJob"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteTrigger"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getAllWhatIfJobs", "getWhatIfJob", "searchWhatIfJob"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api", "sensity_read_only"]
        },
    ],

    "WhatIfPolicyModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createWhatIfPolicy"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateWhatIfPolicy", "whatIfPolicyTagsAssociation"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteWhatIfPolicy", "whatIfPolicyTagsDisassociation"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getAllWhatIfPolicies", "getWhatIfPolicy"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api", "sensity_read_only"]
        },
    ],

    "WhatIfTagModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createWhatIfTag"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateWhatIfTag"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteWhatIfTag"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getAllWhatIfTags", "getWhatIfTag"],
            "role": ["parking_owner_admin", "end_user_admin", "policy_authority", "parking_manager", "end_user_api", "partner_admin", "sensity_user", "sensity_admin", "partner_api", "sensity_read_only"]
        },
    ],



    "GpsModel": [
            {
                "privilege": "CAN_READ",
                "operation": ["getGpsByNodeId", "getGpsForOrgIdAndSiteId"],
                "role": ["sensity_admin", "sensity_user", "sensity_read_only", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_api", "partner_read_only", "end_user_admin", "end_user_lighting_user", "end_user_read_only", "end_user_sensor_user", "end_user_api", "installer","parking_owner_admin", "policy_authority", "parking_manager", "end_user_networking_user" , "partner_deployment_user", "partner_networking_user"]
            },
    ],



    "UFAlarmModel": [
        {
            "privilege": "CAN_CREATE",
            "operation": ["createUFAlarm", "createBulkUFAlarms"],
            "role": ["sensity_user","sensity_admin"]
        }, {
            "privilege": "CAN_UPDATE",
            "operation": ["updateUFAlarm", "resetUFAlarm"],
            "role": ["sensity_user","sensity_admin"]
        }, {
            "privilege": "CAN_DELETE",
            "operation": ["deleteUFAlarm"],
            "role": ["sensity_user","sensity_admin"]
        }, {
            "privilege": "CAN_READ",
            "operation": ["getUFAlarms", "getAllUFAlarms"],
            "role": ["sensity_user","sensity_admin"]
        },
    ],



}

var hierarchy = {
    "parking_owner_admin": [
        {
            "privilege": "CAN_CREATE",
            "role": ["parking_owner_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer"]
        },
        {
            "privilege": "CAN_DELETE",
            "role": ["parking_owner_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer"]
        },
        {
            "privilege": "CAN_CHANGE",
            "role": ["parking_owner_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer"]
        }
    ],
    "policy_authority": [
        {
            "privilege": "CAN_CREATE",
            "role": ["policy_authority", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer"]
        },
        {
            "privilege": "CAN_DELETE",
            "role": ["policy_authority", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer"]
        },
        {
            "privilege": "CAN_CHANGE",
            "role": ["policy_authority", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer"]
        }
    ],
    "sensity_admin": [
        {
            "privilege": "CAN_CREATE",
            "role": ["sensity_admin", "sensity_user", "sensity_read_only", "partner_admin", "parking_owner_admin", "parking_manager", "policy_authority"]
        },
        {
            "privilege": "CAN_DELETE",
            "role": ["sensity_admin", "sensity_user", "sensity_read_only", "partner_admin", "parking_owner_admin", "parking_manager", "policy_authority"]
        },
        {
            "privilege": "CAN_UPDATE",
            "role": ["sensity_admin", "sensity_user", "sensity_read_only", "partner_admin", "parking_owner_admin", "parking_manager", "policy_authority"]
        },
        {
            "privilege": "CAN_CHANGE",
            "role": ["parking_owner_admin", "policy_authority", "parking_manager"]
        }
    ],
    "sensity_user": [
        {
            "privilege": "CAN_CREATE",
            "role": ["sensity_user", "sensity_read_only", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer", "parking_owner_admin", "parking_manager", "policy_authority"]
        },
        {
            "privilege": "CAN_DELETE",
            "role": ["sensity_user", "sensity_read_only", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer", "parking_owner_admin", "parking_manager", "policy_authority"]
        },
        {
            "privilege": "CAN_CHANGE",
            "role": ["sensity_user", "sensity_read_only", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer", "parking_owner_admin", "parking_manager", "policy_authority"]
        }
    ],
    "partner_admin": [
        {
            "privilege": "CAN_CREATE",
            "role": ["sensity_read_only", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "parking_owner_admin", "parking_manager", "policy_authority"]
        },
        {
            "privilege": "CAN_DELETE",
            "role": ["sensity_read_only", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "parking_owner_admin", "parking_manager", "policy_authority"]
        },
        {
            "privilege": "CAN_CHANGE",
            "role": ["sensity_read_only", "partner_admin", "partner_deployment_user", "partner_lighting_user", "partner_sensor_user", "partner_networking_user", "partner_read_only", "partner_api", "end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "parking_owner_admin", "parking_manager", "policy_authority"]
        }
    ],
    "end_user_admin": [
        {
            "privilege": "CAN_CREATE",
            "role": ["end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer", "parking_owner_admin", "parking_manager", "policy_authority"]
        },
        {
            "privilege": "CAN_DELETE",
            "role": ["end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer", "parking_owner_admin", "parking_manager", "policy_authority"]
        },
        {
            "privilege": "CAN_CHANGE",
            "role": ["end_user_admin", "end_user_lighting_user", "end_user_sensor_user", "end_user_networking_user", "end_user_read_only", "end_user_api", "installer", "parking_owner_admin", "parking_manager", "policy_authority"]
        }
    ],
};

    var PrivilegesV3_0_0x = [ ];

var models = {

    "OrgModel": "om",
    "SiteModel": "sm",
    "NodeModel": "nm",
    "UserModel": "um",
    "OverlayModel": "olm",
    "FirmwareModel": "fwm",
    "AuditModel": "am",
    "FixtureModel": "fm",
    "LightingPolicyModel": "lpm",
    "LevelModel": "lm",
    "ConfigModel": "cm",
    "NotificationModel": "nfm",
    "ParkingZoneModel": "pz",
    "AlertModel": "alm",
    "GroupModel": "gm",
    "ScheduleModel": "scm",
    "TrafficObjectModel": "tom",
    "ParkingSpotModel": "psm",
    "ParkingPolicyModel": "ppm",
    "PolicyCategoryModel": "pcm",
    "ParkingGroupModel": "pgm",
    "SummaryReportModel": "srm",
    "BusinessAlertModel": "bam",
    "TriggerModel": "tm",
    "WhatIfAnalysisModel": "wam",
    "WhatIfPolicyModel": "wpm",
    "WhatIfTagModel": "wtm",
    "UFAlarmModel": "ufam",
    // TODO --- Not sure about these ---
    "PartnerModel": "pm",
    "ReportModel": "rm",
    "LicenseModel": "lcm",
    "GpsModel": "gps",

};

var matches = {
    'OrgModel': '(om:OrgModel:Model {omid: "omid", name: "OrgModel"})',
    'SiteModel': '(sm:SiteModel:Model {smid: "smid", name: "SiteModel"})',
    'NodeModel': '(nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})',
    'ConfigModel': '(cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})',
    'UserModel': '(um:UserModel:Model {umid: "umid", name: "UserModel"})',
    'OverlayModel': '(olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})',
    'ParkingSpotModel': '(psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})',
    'ParkingPolicyModel': '(ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})',
    'PolicyCategoryModel': '(pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})',
    'ParkingGroupModel': '(pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})',
    'SummaryReportModel': '(srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})',
    'FirmwareModel': '(fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})',
    'AuditModel': '(am:AuditModel:Model {amid: "amid", name: "AuditModel"})',
    'FixtureModel': '(fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})',
    'NotificationModel': '(nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})',
    'ParkingZoneModel': '(pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})',
    'AlertModel': '(alm:AlertModel:Model {almid: "almid", name: "AlertModel"})',
    'GroupModel': '(gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})',
    'ScheduleModel': '(scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})',
    'PartnerModel': '(pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})',
    'ReportModel': '(rm:ReportModel:Model {rmid: "rmid", name: "ReportModel"})',
    'LicenseModel': '(lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})',
    'TrafficObjectModel': '(tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})',
    'BusinessAlertModel': '(bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})',
    'TriggerModel': '(tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})',
    'GpsModel': '(gps:GpsModel:Model {gpsid: "gpsid", name: "GpsModel"})',
    'WhatIfAnalysisModel': '(wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})',
    'WhatIfPolicyModel': '(wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})',
    'WhatIfTagModel': '(wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})',
    'UFAlarmModel': '(ufam:UFAlarmModel:Model {ufamid: "ufamid", name: "UFAlarmModel"})',
    'sensity_admin': '(sensity_admin:Role:Admin {rolename: "sensity_admin"})',
    'sensity_read_only': '(sensity_read_only:Role:Admin {rolename: "sensity_read_only"})',
    'sensity_user': '(sensity_user:Role:Admin {rolename: "sensity_user"})',
    'partner_admin': '(partner_admin:Role {rolename: "partner_admin"})',
    'partner_deployment_user': '(partner_deployment_user:Role {rolename: "partner_deployment_user"})',
    'partner_lighting_user': '(partner_lighting_user:Role {rolename: "partner_lighting_user"})',
    'partner_sensor_user': '(partner_sensor_user:Role {rolename: "partner_sensor_user"})',
    'partner_networking_user': '(partner_networking_user:Role {rolename: "partner_networking_user"})',
    'partner_read_only': '(partner_read_only:Role {rolename: "partner_read_only"})',
    'partner_api': '(partner_api:Role {rolename: "partner_api"})',
    'end_user_admin': '(end_user_admin:Role {rolename: "end_user_admin"})',
    'end_user_lighting_user': '(end_user_lighting_user:Role {rolename: "end_user_lighting_user"})',
    'end_user_sensor_user': '(end_user_sensor_user:Role {rolename: "end_user_sensor_user"})',
    'end_user_networking_user': '(end_user_networking_user:Role {rolename: "end_user_networking_user"})',
    'end_user_read_only': '(end_user_read_only:Role {rolename: "end_user_read_only"})',
    'end_user_api': '(end_user_api:Role {rolename: "end_user_api"})',
    'parking_owner_admin': '(parking_owner_admin:Role {rolename: "parking_owner_admin"})',
    'parking_manager': '(parking_manager:Role {rolename: "parking_manager"})',
    'policy_authority': '(policy_authority:Role {rolename: "policy_authority"})',
    'installer': '(installer:Role {rolename: "installer"})'
};

var init_models = `

//------------- THIS IS GENERATED FILE - DO NOT EDIT  --------------------//
// Generate this file from Farallones/tools/matrixgen/privileges.js
//
// $ cd Farallones/tools/matrixgen/
//
// edit privileges.js with ACL changes
//
// $ node privileges.js
//
// This will produce the file
// Farallones/dbinit/patches/AllPrivileges-dbpatch.cypher
//----------------------------------------------------------------------- //

CREATE CONSTRAINT ON (org:Org) ASSERT org.orgid IS UNIQUE;
CREATE CONSTRAINT ON (site:Site) ASSERT site.siteid IS UNIQUE;
CREATE CONSTRAINT ON (node:Node) ASSERT node.nodeid IS UNIQUE;
CREATE CONSTRAINT ON (f:Fixture) ASSERT f.fixtureid IS UNIQUE;
CREATE CONSTRAINT ON (c:Config) ASSERT c.configid IS UNIQUE;
CREATE CONSTRAINT ON (g:Group) ASSERT g.groupid is UNIQUE;
CREATE CONSTRAINT ON (fw:Firmware) ASSERT fw.firmwareid is UNIQUE;
CREATE CONSTRAINT ON (sch:Schedule) ASSERT sch.scheduleid is UNIQUE;
CREATE CONSTRAINT ON (u:User) ASSERT u.userid IS UNIQUE;
CREATE CONSTRAINT ON (u:User) ASSERT u.email IS UNIQUE;
CREATE CONSTRAINT ON (r:Role) ASSERT r.rolename IS UNIQUE;
CREATE CONSTRAINT ON (pd:PDProfile) ASSERT pd.pdprofileid IS UNIQUE;
CREATE CONSTRAINT ON (dh:DHProfile) ASSERT dh.dhprofileid IS UNIQUE;
CREATE CONSTRAINT ON (etdh:ETDHProfile) ASSERT etdh.etdhprofileid IS UNIQUE;
CREATE INDEX ON :Alert(nodeid);
CREATE INDEX ON :Alert(name);
CREATE INDEX ON :Alert(type);
CREATE CONSTRAINT ON (a:Alert) ASSERT a.alertid IS UNIQUE;
CREATE CONSTRAINT ON (sm:SiteModel) ASSERT sm.smid IS UNIQUE;
CREATE CONSTRAINT ON (nm:NodeModel) ASSERT nm.nmid IS UNIQUE;
CREATE CONSTRAINT ON (cm:ConfigModel) ASSERT cm.cmid IS UNIQUE;
CREATE CONSTRAINT ON (um:UserModel) ASSERT um.umid IS UNIQUE;
CREATE CONSTRAINT ON (om:OrgModel) ASSERT om.omid IS UNIQUE;
CREATE CONSTRAINT ON (olm:OverlayModel) ASSERT olm.olmid IS UNIQUE;
CREATE CONSTRAINT ON (fwm:FirmwareModel) ASSERT fwm.fwmid IS UNIQUE;
CREATE CONSTRAINT ON (am:AuditModel) ASSERT am.amid IS UNIQUE;
CREATE CONSTRAINT ON (fm:FixtureModel) ASSERT fm.fmid IS UNIQUE;
CREATE CONSTRAINT ON (n:Notification) ASSERT n.notificationid IS UNIQUE;
CREATE CONSTRAINT ON (nfm:NotificationModel) ASSERT nfm.nfmid IS UNIQUE;
CREATE CONSTRAINT ON (pz:ParkingZoneModel) ASSERT pz.parkingzoneid IS UNIQUE;
CREATE CONSTRAINT ON (tom:TrafficObjectModel) ASSERT tom.tomid is UNIQUE;
CREATE CONSTRAINT ON (alm:AlertModel) ASSERT alm.almid IS UNIQUE;
CREATE CONSTRAINT ON (gm:GroupModel) ASSERT gm.gmid IS UNIQUE;
CREATE CONSTRAINT ON (scm:ScheduleModel) ASSERT scm.scmid is UNIQUE;
CREATE CONSTRAINT ON (pm:PartnerModel) ASSERT pm.pmid is UNIQUE;
CREATE CONSTRAINT ON (rm:ReportModel) ASSERT rm.rmid is UNIQUE;
CREATE CONSTRAINT ON (lcm:LicenseModel) ASSERT lcm.lcmid is UNIQUE;
CREATE CONSTRAINT ON (psm:ParkingSpotModel) ASSERT psm.psmid IS UNIQUE;
CREATE CONSTRAINT ON (ppm:ParkingPolicyModel) ASSERT ppm.ppmid IS UNIQUE;
CREATE CONSTRAINT ON (pcm:PolicyCategoryModel) ASSERT pcm.pcmid IS UNIQUE;
CREATE CONSTRAINT ON (pgm:ParkingGroupModel) ASSERT pgm.pgmid IS UNIQUE;
CREATE CONSTRAINT ON (srm:SummaryReportModel) ASSERT srm.srmid IS UNIQUE;
CREATE CONSTRAINT ON (bam:BusinessAlertModel) ASSERT bam.bamid IS UNIQUE;
CREATE CONSTRAINT ON (tm:TriggerModel) ASSERT tm.tmid IS UNIQUE;
CREATE CONSTRAINT ON (gps:GpsModel) ASSERT gps.gpsid IS UNIQUE;
CREATE CONSTRAINT ON (wam:WhatIfAnalysisModel) ASSERT wam.wamid IS UNIQUE;
CREATE CONSTRAINT ON (wpm:WhatIfPolicyModel) ASSERT wpm.wpmid IS UNIQUE;
CREATE CONSTRAINT ON (wtm:WhatIfTagModel) ASSERT wtm.wtmid IS UNIQUE;


CREATE CONSTRAINT ON (ufam:UFAlarmModel) ASSERT ufam.ufamid is UNIQUE;



MERGE (om:OrgModel:Model {omid: "omid", name: "OrgModel"})
  ON CREATE SET om.created = timestamp();
MERGE (sm:SiteModel:Model {smid: "smid", name: "SiteModel"})
  ON CREATE SET sm.created = timestamp();
MERGE (nm:NodeModel:Model {nmid: "nmid", name: "NodeModel"})
  ON CREATE SET nm.created = timestamp();
MERGE (cm:ConfigModel:Model {cmid: "cmid", name: "ConfigModel"})
  ON CREATE SET cm.created = timestamp();
MERGE (um:UserModel:Model {umid: "umid", name: "UserModel"})
  ON CREATE SET um.created = timestamp();
MERGE (olm:OverlayModel:Model {olmid: "olmid", name: "OverlayModel"})
  ON CREATE SET olm.created = timestamp();
MERGE (fwm:FirmwareModel:Model {fwmid: "fwmid", name: "FirmwareModel"})
  ON CREATE SET fwm.created = timestamp();
MERGE (am:AuditModel:Model {amid: "amid", name: "AuditModel"})
  ON CREATE SET am.created = timestamp();
MERGE (fm:FixtureModel:Model {fmid: "fmid", name: "FixtureModel"})
  ON CREATE SET fm.created = timestamp();
MERGE (nfm:NotificationModel:Model {nfmid: "nfmid", name: "NotificationModel"})
  ON CREATE SET nfm.created = timestamp();
MERGE (pz:ParkingZoneModel:Model {parkingzoneid: "parkingzoneid", name: "ParkingZoneModel"})
  ON CREATE SET pz.created = timestamp();
MERGE (alm:AlertModel:Model {almid: "almid", name: "AlertModel"})
  ON CREATE SET alm.created = timestamp();
MERGE (gm:GroupModel:Model {gmid: "gmid", name: "GroupModel"})
  ON CREATE SET gm.created = timestamp();
MERGE (scm:ScheduleModel:Model {scmid: "scmid", name: "ScheduleModel"})
  ON CREATE SET scm.created = timestamp();
MERGE (pm:PartnerModel:Model {pmid: "pmid", name: "PartnerModel"})
  ON CREATE SET pm.created = timestamp();
MERGE (rm:ReportModel:Model {rmid: "rmid", name: "ReportModel"})
  ON CREATE SET rm.created = timestamp();
MERGE (lcm:LicenseModel:Model {lcmid: "lcmid", name: "LicenseModel"})
  ON CREATE SET lcm.created = timestamp();
MERGE (tom:TrafficObjectModel:Model {tomid: "tomid", name: "TrafficObjectModel"})
  ON CREATE SET tom.created = timestamp();
MERGE (psm:ParkingSpotModel:Model {psmid: "psmid", name: "ParkingSpotModel"})
  ON CREATE SET psm.created = timestamp();
MERGE (ppm:ParkingPolicyModel:Model {ppmid: "ppmid", name: "ParkingPolicyModel"})
  ON CREATE SET ppm.created = timestamp();
MERGE (pcm:PolicyCategoryModel:Model {pcmid: "pcmid", name: "PolicyCategoryModel"})
  ON CREATE SET pcm.created = timestamp();
MERGE (pgm:ParkingGroupModel:Model {pgmid: "pgmid", name: "ParkingGroupModel"})
  ON CREATE SET pgm.created = timestamp();
MERGE (srm:SummaryReportModel:Model {srmid: "srmid", name: "SummaryReportModel"})
  ON CREATE SET srm.created = timestamp();
MERGE (bam:BusinessAlertModel:Model {bamid: "bamid", name: "BusinessAlertModel"})
  ON CREATE SET bam.created = timestamp();
MERGE (tm:TriggerModel:Model {tmid: "tmid", name: "TriggerModel"})
  ON CREATE SET tm.created = timestamp();
MERGE(gps:GpsModel:Model {gpsid: "gpsid",name: "GpsModel"})
  ON CREATE SET gps.created = timestamp();
  MERGE (wam:WhatIfAnalysisModel:Model {wamid: "wamid", name: "WhatIfAnalysisModel"})
    ON CREATE SET wam.created = timestamp();
  MERGE (wpm:WhatIfPolicyModel:Model {wpmid: "wpmid", name: "WhatIfPolicyModel"})
    ON CREATE SET wpm.created = timestamp();
  MERGE (wtm:WhatIfTagModel:Model {wtmid: "wtmid", name: "WhatIfTagModel"})
    ON CREATE SET wtm.created = timestamp();
MERGE (ufam:UFAlarmModel:Model {ufamid: "ufamid", name: "UFAlarmModel"})
  ON CREATE SET ufam.created = timestamp();




MERGE (sensity_admin:Role:Admin {rolename: "sensity_admin"})
  ON CREATE SET sensity_admin.created = timestamp();
MERGE (sensity_read_only:Role:Admin {rolename: "sensity_read_only"})
  ON CREATE SET sensity_read_only.created = timestamp();
MERGE (sensity_user:Role:Admin {rolename: "sensity_user"})
  ON CREATE SET sensity_user.created = timestamp();

MERGE (partner_admin:Role {rolename: "partner_admin"})
  ON CREATE SET partner_admin.created = timestamp();
MERGE (partner_deployment_user:Role {rolename: "partner_deployment_user"})
  ON CREATE SET partner_deployment_user.created = timestamp();
MERGE (partner_lighting_user:Role {rolename: "partner_lighting_user"})
  ON CREATE SET partner_lighting_user.created = timestamp();
MERGE (partner_sensor_user:Role {rolename: "partner_sensor_user"})
  ON CREATE SET partner_sensor_user.created = timestamp();
MERGE (partner_networking_user:Role {rolename: "partner_networking_user"})
  ON CREATE SET partner_networking_user.created = timestamp();
MERGE (partner_read_only:Role {rolename: "partner_read_only"})
  ON CREATE SET partner_read_only.created = timestamp();
MERGE (partner_api:Role {rolename: "partner_api"})
  ON CREATE SET partner_api.created = timestamp();

MERGE (end_user_admin:Role {rolename: "end_user_admin"})
  ON CREATE SET end_user_admin.created = timestamp();
MERGE (end_user_lighting_user:Role {rolename: "end_user_lighting_user"})
  ON CREATE SET end_user_lighting_user.created = timestamp();
MERGE (end_user_sensor_user:Role {rolename: "end_user_sensor_user"})
  ON CREATE SET end_user_sensor_user.created = timestamp();
MERGE (end_user_networking_user:Role {rolename: "end_user_networking_user"})
  ON CREATE SET end_user_networking_user.created = timestamp();
MERGE (end_user_read_only:Role {rolename: "end_user_read_only"})
  ON CREATE SET end_user_read_only.created = timestamp();
MERGE (end_user_api:Role {rolename: "end_user_api"})
  ON CREATE SET end_user_api.created = timestamp();

MERGE (installer:Role {rolename: "installer"})
  ON CREATE SET installer.created = timestamp();

MERGE (parking_owner_admin:Role {rolename: "parking_owner_admin"})
  ON CREATE SET parking_owner_admin.created = timestamp();
MERGE (parking_manager:Role {rolename: "parking_manager"})
  ON CREATE SET parking_manager.created = timestamp();
MERGE (policy_authority:Role {rolename: "policy_authority"})
  ON CREATE SET policy_authority.created = timestamp();

MATCH (sensity_admin_user:User:Active {userid: "114ad560-a046-11e5-a57f-ef24ae600576"}),
      (sensity_admin:Role:Admin {rolename: "sensity_admin"}),
      (sensity:Org:Active {orgid: "efe5bdb3-baac-5d8e-6cae57771c13"})
MERGE (sensity_admin_user)-[:IS]->(sensity_admin)
MERGE (sensity_admin)-[:HAS]->(sensity_admin_user)
MERGE (sensity_admin_user)-[:IS_USER_OF]->(sensity)
MERGE (sensity)-[:HAS_USER]->(sensity_admin_user);

MATCH (sensity_user_user:User:Active {userid: "507ef3cc-cd2d-46d8-ae6d-7ccf430c1110"}),
      (sensity_user:Role:Admin {rolename: "sensity_user"}),
      (sensity:Org:Active {orgid: "efe5bdb3-baac-5d8e-6cae57771c13"})
MERGE (sensity_user_user)-[:IS]->(sensity_user)
MERGE (sensity_user)-[:HAS]->(sensity_user_user)
MERGE (sensity_user_user)-[:IS_USER_OF]->(sensity)
MERGE (sensity)-[:HAS_USER]->(sensity_user_user);

MATCH (sensity_read_only_user:User:Active {userid: "47e3e2f2-b93b-4557-b668-271d43d028ac"}),
      (sensity_read_only:Role:Admin {rolename: "sensity_read_only"}),
      (sensity:Org:Active {orgid: "efe5bdb3-baac-5d8e-6cae57771c13"})
MERGE (sensity_read_only_user)-[:IS]->(sensity_read_only)
MERGE (sensity_read_only)-[:HAS]->(sensity_read_only_user)
MERGE (sensity_read_only_user)-[:IS_USER_OF]->(sensity)
MERGE (sensity)-[:HAS_USER]->(sensity_read_only_user);

`;

var fix_user_roles = `

MATCH (sensity_admin:Role:Admin {rolename: "sensity_admin"})
MATCH (sensity_user:Role {rolename: "sensity_user"})
MATCH (sensity_read_only:Role {rolename: "sensity_read_only"})
MATCH (partner_admin:Role {rolename: "partner_admin"})
MATCH (partner_deployment_user:Role {rolename: "partner_deployment_user"})
MATCH (partner_lighting_user:Role {rolename: "partner_lighting_user"})
MATCH (partner_sensor_user:Role {rolename: "partner_sensor_user"})
MATCH (partner_networking_user:Role {rolename: "partner_networking_user"})
MATCH (partner_read_only:Role {rolename: "partner_read_only"})
MATCH (partner_api:Role {rolename: "partner_api"})
MATCH (end_user_admin:Role {rolename : "end_user_admin"})
MATCH (end_user_lighting_user:Role {rolename: "end_user_lighting_user"})
MATCH (end_user_sensor_user:Role {rolename: "end_user_sensor_user"})
MATCH (end_user_networking_user:Role {rolename: "end_user_networking_user"})
MATCH (end_user_read_only:Role {rolename: "end_user_read_only"})
MATCH (end_user_api:Role {rolename: "end_user_api"})
MATCH (installer:Role {rolename: "installer"})
MATCH (parking_owner_admin:Role {rolename: "parking_owner_admin"})
MATCH (parking_manager:Role {rolename: "parking_manager"})
MATCH (policy_authority:Role {rolename: "policy_authority"})

`;

var cypher_buffer = "";
var fs = require('fs');
for (var model in PrivilegesV3_0_0) {
    for (var i = 0; i < PrivilegesV3_0_0[model].length; i++) {
        for (var j = 0; j < PrivilegesV3_0_0[model][i].role.length; j++) {
            //console.log("Processing", PrivilegesV3_0_0[model])
            if (models[model])
                cypher_buffer += ("CREATE UNIQUE (" + PrivilegesV3_0_0[model][i].role[j] + ")-[:" + PrivilegesV3_0_0[model][i].privilege + "]->(" + models[model] + ")\n");
            else
                console.error("Error: missing model ", model, "in", models);
        }
    }
}
fs.writeFile("./PrivilegesV3_0_0.cypher", cypher_buffer + ";\n", function(err) {
    if (err) {
        return console.log(err);
    }

});

cypher_buffer = "";
csv_buffer = "#model,role,privilege\n";
for (var model in PrivilegesV3_0_0) {
    for (var i = 0; i < PrivilegesV3_0_0[model].length; i++) {
        for (var j = 0; j < PrivilegesV3_0_0[model][i].role.length; j++) {
            //console.log("Processing", PrivilegesV3_0_0[model])
            if (models[model]) {
                cypher_buffer += ("MATCH " + matches[PrivilegesV3_0_0[model][i].role[j]] + ", " + matches[model] + "\n");
                cypher_buffer += ("MERGE (" + PrivilegesV3_0_0[model][i].role[j] + ")-[:" + PrivilegesV3_0_0[model][i].privilege + "]->(" + models[model] + ");\n");
                csv_buffer += (model + "," + PrivilegesV3_0_0[model][i].role[j] + "," + PrivilegesV3_0_0[model][i].privilege + "\n");
            } else
                console.error("Error: missing model ", model, "in", models);
        }
    }
}

hierarchy_buffer = "#adminrole,privilege,role\n";
for (var role in hierarchy) {
    for (var i = 0; i < hierarchy[role].length; i++) {
        for (var j = 0; j < hierarchy[role][i].role.length; j++) {
            hierarchy_buffer += (role + "," + hierarchy[role][i].privilege + "," + hierarchy[role][i].role[j] + "\n");
            fix_user_roles += ("MERGE (" + role + ")-[:" + hierarchy[role][i].privilege + "]->(" + hierarchy[role][i].role[j] + ")\n");
        }
        fix_user_roles += "\n";
    }
    fix_user_roles += "\n";
}


fs.writeFile("./../../dbinit/patches/AllPrivileges-dbpatch.cypher", init_models + "MATCH (:Role)-[r]->(:Model) DELETE r;\n" + cypher_buffer + "\n" + fix_user_roles + ";\n", function(err) {
    if (err) {
        return console.log(err);
    }

});
fs.writeFile("./../../dbinit/csv/acl.csv", csv_buffer, function(err) {
    if (err) {
        return console.log(err);
    }
});
fs.writeFile("./../../dbinit/csv/adminroles.csv", hierarchy_buffer, function(err) {
    if (err) {
        return console.log(err);
    }
});

var operation_privileges = {};
var model_privileges = {};
for (var model in PrivilegesV3_0_0) {
    if (!operation_privileges[model])
        operation_privileges[model] = {};
    if (!model_privileges[model])
        model_privileges[model] = [];
    for (var i = 0; i < PrivilegesV3_0_0[model].length; i++) {
        model_privileges[model].push(PrivilegesV3_0_0[model][i].privilege);
        for (var j = 0; j < PrivilegesV3_0_0[model][i].operation.length; j++) {
            var operation = PrivilegesV3_0_0[model][i].operation[j];
            if (!operation_privileges[model][operation])
                operation_privileges[model][operation] = [];
            operation_privileges[model][operation].push(PrivilegesV3_0_0[model][i].privilege);
            //
        }
    }
}
var amodel_privileges = [];
for (var model in model_privileges) {
    amodel_privileges.push({
        model: model,
        allowed: model_privileges[model]
    });
}
fs.writeFile("./../../api_service/ui_api/auth/PrivilegesV3_0_0.js", "\"use strict\";\nexports.operation_privileges  =\n" + JSON.stringify(operation_privileges, null, '\t') + ";\n", function(err) {
    if (err) {
        return console.log(err);
    }

});
fs.appendFile("./../../api_service/ui_api/auth/PrivilegesV3_0_0.js", "\nexports.model_privileges  =\n" + JSON.stringify(amodel_privileges, null, '\t') + ";\n", function(err) {
    if (err) {
        return console.log(err);
    }

});
console.log("The file was saved!");
