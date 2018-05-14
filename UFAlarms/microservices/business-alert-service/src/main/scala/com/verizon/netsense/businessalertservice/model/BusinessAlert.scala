package com.verizon.netsense.businessalertservice.model

case class BusinessAlert(orgId: String,
                         siteId: String,
                         triggerId: Option[String],
                         resourceId: Option[String],
                         businessAlertId: String,
                         triggerName: Option[String],
                         triggerCategory: Option[String],
                         triggerSubcategory: Option[String],
                         resourceName: Option[String],
                         active: Option[Boolean],
                         createdOn: Option[Long],
                         message: Option[String],
                         severity: Option[String],
                         lastClearedAt: Option[Long],
                         lastClearedBy: Option[String],
                         lastUpdated: Option[Long],
                         triggerUserId: Option[String])

case class BusinessAlertHistory(orgId: String,
                                siteId: String,
                                triggerId: Option[String],
                                resourceId: Option[String],
                                businessAlertId: Option[String],
                                triggerName: Option[String],
                                triggerCategory: Option[String],
                                triggerSubcategory: Option[String],
                                resourceName: Option[String],
                                active: Option[Boolean],
                                createdOn: Long,
                                message: Option[String],
                                severity: Option[String],
                                lastClearedAt: Option[Long],
                                lastClearedBy: Option[String],
                                triggerUserId: Option[String])

case class BusinessAlertResponse(orgId: String,
                                 siteId: String,
                                 triggerId: String,
                                 resourceId: String,
                                 businessAlertId: String,
                                 triggerName: String,
                                 triggerCategory: String,
                                 triggerSubcategory: String,
                                 resourceName: String,
                                 active: Boolean,
                                 createdOn: String,
                                 message: String,
                                 severity: String,
                                 lastClearedAt: String,
                                 lastClearedBy: String,
                                 lastUpdated: String,
                                 triggerUserId: String)

/**
 * This model is used for only getBusinessAlertSys API
 * This internal API is used by IS for notification purpose
 * @param orgid IS requires this in lowercase
 * @param siteid IS requires this in lowercase
 */
case class BusinessAlertSysResponse(orgid: String,
                                    siteid: String,
                                    triggerId: String,
                                    resourceId: String,
                                    businessAlertId: String,
                                    triggerName: String,
                                    triggerCategory: String,
                                    triggerSubcategory: String,
                                    resourceName: String,
                                    active: Boolean,
                                    createdOn: String,
                                    message: String,
                                    severity: String,
                                    lastClearedAt: String,
                                    lastClearedBy: String,
                                    lastUpdated: String,
                                    triggerUserId: String)

case class SuccessMessage(success: Boolean)

case class SuccessWithMessage(success: Boolean, message: String)
