package com.verizon.netsense.services.gps.model

import com.verizon.netsense.entity.Entity

/**
 * Created by vermri5 on 7/12/17.
 */
case class OrgHierarchy(nodeId: String,
                        nodeName: Option[String] = None,
                        orgId: String,
                        orgName: Option[String] = None,
                        siteId: String,
                        siteName: Option[String] = None,
                        siteAddress: Option[String] = None,
                        bssId: Option[String] = None,
                        nodeHw: Option[String] = None) extends Entity
