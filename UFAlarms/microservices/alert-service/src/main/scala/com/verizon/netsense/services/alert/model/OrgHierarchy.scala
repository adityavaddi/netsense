package com.verizon.netsense.services.alert.model

/**
 * Created by vermri5 on 7/12/17.
 */
case class OrgHierarchy(nodeId: String,
                        nodeName: Option[String] = None,
                        orgId: Option[String],
                        orgName: Option[String] = None,
                        siteId: Option[String],
                        siteName: Option[String] = None,
                        siteAddress: Option[String] = None,
                        bssId: Option[String] = None,
                        nodeHw: Option[String] = None)
