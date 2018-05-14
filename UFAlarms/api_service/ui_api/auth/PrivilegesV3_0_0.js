"use strict";
exports.operation_privileges  =
{
	"PartnerModel": {
		"createPartner": [
			"CAN_CREATE"
		],
		"deletePartner": [
			"CAN_DELETE"
		],
		"updatePartner": [
			"CAN_UPDATE"
		],
		"suspendPartner": [
			"CAN_SUSPEND"
		],
		"getPartner": [
			"CAN_READ"
		],
		"getAllPartners": [
			"CAN_READ"
		]
	},
	"OrgModel": {
		"createOrg": [
			"CAN_CREATE"
		],
		"updateOrg": [
			"CAN_CHANGE"
		],
		"suspendOrg": [
			"CAN_SUSPEND"
		],
		"activateOrg": [
			"CAN_SUSPEND"
		],
		"deleteOrg": [
			"CAN_DELETE"
		],
		"getOrg": [
			"CAN_READ"
		],
		"getAllOrgs": [
			"CAN_READ"
		]
	},
	"SiteModel": {
		"createSite": [
			"CAN_CREATE"
		],
		"updateSite": [
			"CAN_CHANGE"
		],
		"suspendSite": [
			"CAN_SUSPEND"
		],
		"activateSite": [
			"CAN_SUSPEND"
		],
		"deleteSite": [
			"CAN_SUSPEND"
		],
		"getSite": [
			"CAN_READ"
		],
		"getAllSites": [
			"CAN_READ"
		],
		"getAllSitesForOrg": [
			"CAN_READ"
		],
		"getAllSuspendedSitesForOrg": [
			"CAN_READ"
		]
	},
	"UserModel": {
		"createUser": [
			"CAN_CREATE_ORG_USER"
		],
		"updateUser": [
			"CAN_CHANGE_ORG_USER"
		],
		"suspendUser": [
			"CAN_SUSPEND_ORG_USER"
		],
		"activateUser": [
			"CAN_SUSPEND_ORG_USER"
		],
		"getUser": [
			"CAN_READ_ORG_USER"
		],
		"generateAPIKey": [
			"CAN_GENERATE_API_KEY"
		]
	},
	"GroupModel": {
		"createGroup": [
			"CAN_CREATE"
		],
		"updateGroup": [
			"CAN_UPDATE"
		],
		"addNodeToGroup": [
			"CAN_UPDATE"
		],
		"removeNodeFromGroup": [
			"CAN_UPDATE"
		],
		"deleteGroup": [
			"CAN_DELETE"
		],
		"getGroup": [
			"CAN_READ"
		],
		"getAllGroups": [
			"CAN_READ"
		]
	},
	"LicenseModel": {
		"createLicense": [
			"CAN_CREATE"
		],
		"getLicense": [
			"CAN_READ"
		],
		"getLicenseUsage": [
			"CAN_READ_USAGE"
		],
		"deleteLicense": [
			"CAN_DELETE"
		]
	},
	"ScheduleModel": {
		"createLightingSchedule": [
			"CAN_CREATE"
		],
		"updateLightingSchedule": [
			"CAN_UPDATE"
		],
		"deleteLightingSchedule": [
			"CAN_DELETE"
		],
		"getLightingSchedule": [
			"CAN_READ"
		],
		"getAllLightingSchedules": [
			"CAN_READ"
		],
		"applyScheduleToGroup": [
			"CAN_APPLY"
		],
		"applyScheduleToSite": [
			"CAN_APPLY"
		],
		"applyScheduleToNode": [
			"CAN_APPLY"
		]
	},
	"NodeModel": {
		"assignLightingScheduleToGroup": [
			"CAN_ASSIGN_TO_LS_GROUP"
		],
		"assignLightAndMotionDetectionToGroup": [
			"CAN_ASSIGN_LM_DETECTION_TO_GROUP"
		],
		"lighting-control-group": [
			"CAN_MANAGE_LIGHTS"
		],
		"lighting-control-site": [
			"CAN_MANAGE_LIGHTS"
		],
		"lighting-control": [
			"CAN_MANAGE_LIGHTS"
		],
		"getNode": [
			"CAN_READ"
		],
		"getAllNodes": [
			"CAN_READ"
		],
		"getAllNodesForSite": [
			"CAN_READ"
		],
		"getNodeConnectionStatus": [
			"CAN_READ"
		],
		"getSiteNodesConnectionStatus": [
			"CAN_READ"
		],
		"createNode": [
			"CAN_CREATE"
		],
		"createEmptyNode": [
			"CAN_CREATE"
		],
		"updateNode": [
			"CAN_CHANGE"
		],
		"deleteNode": [
			"CAN_DEACTIVATE",
			"CAN_OPERATE"
		],
		"assignNodes": [
			"CAN_ASSIGN_TO_ORGS",
			"CAN_ASSIGN_TO_SITES",
			"CAN_ASSIGN_TO_GROUPS"
		],
		"upgradeFirmware": [
			"CAN_UPGRADE_FIRMWARE"
		],
		"assignFixture": [
			"CAN_ASSIGN_FIXTURE"
		],
		"searchForNodes": [
			"CAN_SEARCH"
		],
		"retrieveLogFile": [
			"CAN_RETRIEVE_LOG_FILE"
		],
		"retrieveAlarm": [
			"CAN_RETRIEVE_ALARM"
		]
	},
	"ConfigModel": {
		"addConfig": [
			"CAN_CREATE"
		],
		"updateConfig": [
			"CAN_UPDATE"
		],
		"deleteConfig": [
			"CAN_DEACTIVATE"
		],
		"getConfig": [
			"CAN_READ"
		],
		"getAllConfigs": [
			"CAN_READ"
		],
		"getDefaultConfigs": [
			"CAN_READ"
		],
		"updateVPNInfo": [
			"CAN_READ"
		],
		"connectToVPN": [
			"CAN_READ"
		],
		"disconnectFromVPN": [
			"CAN_READ"
		],
		"applyConfigToSite": [
			"CAN_APPLY"
		],
		"applyConfigToNodes": [
			"CAN_APPLY"
		],
		"applyConfigToGroup": [
			"CAN_APPLY"
		]
	},
	"FixtureModel": {
		"getFixture": [
			"CAN_READ"
		],
		"getAllFixtures": [
			"CAN_READ"
		],
		"createFixture": [
			"CAN_CREATE"
		],
		"updateFixture": [
			"CAN_UPDATE"
		],
		"deleteFixture": [
			"CAN_DELETE"
		]
	},
	"AuditModel": {
		"createAudit": [
			"CAN_CREATE"
		],
		"updateAudit": [
			"CAN_UPDATE"
		],
		"deleteAudit": [
			"CAN_DELETE"
		],
		"getAudit": [
			"CAN_READ"
		],
		"getAllAudits": [
			"CAN_READ"
		]
	},
	"FirmwareModel": {
		"createFirmware": [
			"CAN_CREATE"
		],
		"updateFirmware": [
			"CAN_UPDATE"
		],
		"deleteFirmware": [
			"CAN_DELETE"
		],
		"getFirmware": [
			"CAN_READ"
		],
		"getAllFirmwares": [
			"CAN_READ"
		]
	},
	"NotificationModel": {
		"createNotification": [
			"CAN_CREATE"
		],
		"updateNotification": [
			"CAN_UPDATE"
		],
		"deleteNotification": [
			"CAN_DELETE"
		],
		"getNotification": [
			"CAN_READ"
		],
		"getAllNotificationsForSite": [
			"CAN_READ"
		],
		"getNotificationTypes": [
			"CAN_READ"
		],
		"activateNotification": [
			"CAN_ACTIVATE"
		],
		"deactivateNotification": [
			"CAN_DEACTIVATE"
		]
	},
	"ParkingZoneModel": {
		"createParkingZone": [
			"CAN_CREATE"
		],
		"updateParkingZone": [
			"CAN_UPDATE"
		],
		"deleteParkingZone": [
			"CAN_DELETE"
		],
		"getParkingZone": [
			"CAN_READ"
		],
		"getAllParkingZones": [
			"CAN_READ"
		]
	},
	"TrafficObjectModel": {
		"createTrafficObject": [
			"CAN_CREATE"
		],
		"updateTrafficObject": [
			"CAN_UPDATE"
		],
		"deleteTrafficObject": [
			"CAN_DELETE"
		],
		"getTrafficObject": [
			"CAN_READ"
		],
		"getAllTrafficObjects": [
			"CAN_READ"
		]
	},
	"AlertModel": {
		"createAlert": [
			"CAN_CREATE"
		],
		"updateAlert": [
			"CAN_UPDATE"
		],
		"deleteAlert": [
			"CAN_DELETE"
		],
		"getAlert": [
			"CAN_READ"
		],
		"getAllAlerts": [
			"CAN_READ"
		]
	},
	"OverlayModel": {
		"createOverlay": [
			"CAN_CREATE"
		],
		"updateOverlay": [
			"CAN_UPDATE"
		],
		"deleteOverlay": [
			"CAN_DELETE"
		],
		"getOverlay": [
			"CAN_READ"
		],
		"getAllOverlays": [
			"CAN_READ"
		]
	},
	"ParkingSpotModel": {
		"createMetadataForParkingSpot": [
			"CAN_CREATE"
		],
		"updateMetadataForParkingSpot": [
			"CAN_UPDATE"
		],
		"deleteMetadataForParkingSpot": [
			"CAN_DELETE"
		],
		"getMetadataForParkingSpot": [
			"CAN_READ"
		],
		"getAllMetadataForParkingSpot": [
			"CAN_READ"
		]
	},
	"ParkingPolicyModel": {
		"createParkingPolicy": [
			"CAN_CREATE"
		],
		"searchParkingPolicy": [
			"CAN_CREATE"
		],
		"updateParkingPolicy": [
			"CAN_UPDATE"
		],
		"policyTagsAssociation": [
			"CAN_UPDATE"
		],
		"deleteParkingPolicy": [
			"CAN_DELETE"
		],
		"policyTagsDisassociation": [
			"CAN_DELETE"
		],
		"getParkingPolicy": [
			"CAN_READ"
		],
		"getAllParkingPolicy": [
			"CAN_READ"
		],
		"getAllVersionsOfParkingPolicy": [
			"CAN_READ"
		],
		"getParkingPolicyVersion": [
			"CAN_READ"
		]
	},
	"PolicyCategoryModel": {
		"createPolicyCategory": [
			"CAN_CREATE"
		],
		"updatePolicyCategory": [
			"CAN_UPDATE"
		],
		"deletePolicyCategory": [
			"CAN_DELETE"
		],
		"getPolicyCategory": [
			"CAN_READ"
		],
		"getAllPolicyCategory": [
			"CAN_READ"
		]
	},
	"ParkingGroupModel": {
		"policyAssociation": [
			"CAN_UPDATE"
		],
		"policyDisassociation": [
			"CAN_DELETE"
		],
		"associatedParkingGroups": [
			"CAN_READ"
		]
	},
	"SummaryReportModel": {
		"createSummaryReport": [
			"CAN_CREATE"
		],
		"updateSummaryReport": [
			"CAN_UPDATE"
		],
		"deleteSummaryReport": [
			"CAN_DELETE"
		],
		"getSummaryReport": [
			"CAN_READ"
		],
		"getAllSummaryReport": [
			"CAN_READ"
		]
	},
	"BusinessAlertModel": {
		"createBusinessAlert": [
			"CAN_CREATE"
		],
		"updateBusinessAlert": [
			"CAN_UPDATE"
		],
		"deleteBusinessAlert": [
			"CAN_DELETE"
		],
		"dismissBusinessAlert": [
			"CAN_DELETE"
		],
		"getBusinessAlert": [
			"CAN_READ"
		],
		"getAllBusinessAlerts": [
			"CAN_READ"
		],
		"filterBusinessAlert": [
			"CAN_READ"
		]
	},
	"TriggerModel": {
		"createTrigger": [
			"CAN_CREATE"
		],
		"updateTrigger": [
			"CAN_UPDATE"
		],
		"deleteTrigger": [
			"CAN_DELETE"
		],
		"getTrigger": [
			"CAN_READ"
		],
		"getAllTriggers": [
			"CAN_READ"
		],
		"filterTrigger": [
			"CAN_READ"
		]
	},
	"WhatIfAnalysisModel": {
		"createWhatIfJob": [
			"CAN_CREATE"
		],
		"abortWhatIfJob": [
			"CAN_UPDATE"
		],
		"updateWhatIfJob": [
			"CAN_UPDATE"
		],
		"deleteTrigger": [
			"CAN_DELETE"
		],
		"getAllWhatIfJobs": [
			"CAN_READ"
		],
		"getWhatIfJob": [
			"CAN_READ"
		],
		"searchWhatIfJob": [
			"CAN_READ"
		]
	},
	"WhatIfPolicyModel": {
		"createWhatIfPolicy": [
			"CAN_CREATE"
		],
		"updateWhatIfPolicy": [
			"CAN_UPDATE"
		],
		"whatIfPolicyTagsAssociation": [
			"CAN_UPDATE"
		],
		"deleteWhatIfPolicy": [
			"CAN_DELETE"
		],
		"whatIfPolicyTagsDisassociation": [
			"CAN_DELETE"
		],
		"getAllWhatIfPolicies": [
			"CAN_READ"
		],
		"getWhatIfPolicy": [
			"CAN_READ"
		]
	},
	"WhatIfTagModel": {
		"createWhatIfTag": [
			"CAN_CREATE"
		],
		"updateWhatIfTag": [
			"CAN_UPDATE"
		],
		"deleteWhatIfTag": [
			"CAN_DELETE"
		],
		"getAllWhatIfTags": [
			"CAN_READ"
		],
		"getWhatIfTag": [
			"CAN_READ"
		]
	},
	"GpsModel": {
		"getGpsByNodeId": [
			"CAN_READ"
		],
		"getGpsForOrgIdAndSiteId": [
			"CAN_READ"
		]
	},
	"UFAlarmModel": {
		"createUFAlarm": [
			"CAN_CREATE"
		],
		"createBulkUFAlarms": [
			"CAN_CREATE"
		],
		"updateUFAlarm": [
			"CAN_UPDATE"
		],
		"resetUFAlarm": [
			"CAN_UPDATE"
		],
		"deleteUFAlarm": [
			"CAN_DELETE"
		],
		"getUFAlarms": [
			"CAN_READ"
		],
		"getAllUFAlarms": [
			"CAN_READ"
		]
	}
};

exports.model_privileges  =
[
	{
		"model": "PartnerModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_DELETE",
			"CAN_UPDATE",
			"CAN_SUSPEND",
			"CAN_READ"
		]
	},
	{
		"model": "OrgModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_CHANGE",
			"CAN_SUSPEND",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "SiteModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_CHANGE",
			"CAN_SUSPEND",
			"CAN_READ"
		]
	},
	{
		"model": "UserModel",
		"allowed": [
			"CAN_CREATE_ORG_USER",
			"CAN_CHANGE_ORG_USER",
			"CAN_SUSPEND_ORG_USER",
			"CAN_READ_ORG_USER",
			"CAN_GENERATE_API_KEY"
		]
	},
	{
		"model": "GroupModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "LicenseModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_READ",
			"CAN_READ_USAGE",
			"CAN_DELETE"
		]
	},
	{
		"model": "ScheduleModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ",
			"CAN_APPLY"
		]
	},
	{
		"model": "NodeModel",
		"allowed": [
			"CAN_ASSIGN_TO_LS_GROUP",
			"CAN_ASSIGN_LM_DETECTION_TO_GROUP",
			"CAN_MANAGE_LIGHTS",
			"CAN_READ",
			"CAN_CREATE",
			"CAN_CHANGE",
			"CAN_DEACTIVATE",
			"CAN_OPERATE",
			"CAN_ASSIGN_TO_ORGS",
			"CAN_ASSIGN_TO_SITES",
			"CAN_ASSIGN_TO_GROUPS",
			"CAN_UPGRADE_FIRMWARE",
			"CAN_ASSIGN_FIXTURE",
			"CAN_SEARCH",
			"CAN_RETRIEVE_LOG_FILE",
			"CAN_RETRIEVE_ALARM"
		]
	},
	{
		"model": "ConfigModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DEACTIVATE",
			"CAN_READ",
			"CAN_APPLY"
		]
	},
	{
		"model": "FixtureModel",
		"allowed": [
			"CAN_READ",
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE"
		]
	},
	{
		"model": "AuditModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "FirmwareModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "NotificationModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ",
			"CAN_ACTIVATE",
			"CAN_DEACTIVATE"
		]
	},
	{
		"model": "ParkingZoneModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "TrafficObjectModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "AlertModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "OverlayModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "ParkingSpotModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "ParkingPolicyModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "PolicyCategoryModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "ParkingGroupModel",
		"allowed": [
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "SummaryReportModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "BusinessAlertModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "TriggerModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "WhatIfAnalysisModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "WhatIfPolicyModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "WhatIfTagModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	},
	{
		"model": "GpsModel",
		"allowed": [
			"CAN_READ"
		]
	},
	{
		"model": "UFAlarmModel",
		"allowed": [
			"CAN_CREATE",
			"CAN_UPDATE",
			"CAN_DELETE",
			"CAN_READ"
		]
	}
];
