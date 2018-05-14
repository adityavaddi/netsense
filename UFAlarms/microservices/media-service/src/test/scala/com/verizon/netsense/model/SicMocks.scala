package com.verizon.netsense.model

object SicMocks {

  val nodeProps = NodeProps("N05023456")
  val siteProps = SiteProps("0af7c52c-55cf-4f54-9e7d-870c01f0c3b6")
  val orgProps = OrgProps("cc92b979-52fd-49eb-9377-4b7d8c21d19f")
  val sicProps = ExtProps("0", "L", "20170908060606", "jpg")

  val sicISQueryPayload = SicISQueryPayload("req_id_1", "SicISQueryPayload", "NodeModel", "CAN_READ", "instanceid", "ts", "a3cd0a73-60ee-43bd-9674-f4af1d575db3", nodeProps, siteProps, orgProps, sicProps )

  val sicISQueryEnvelope = SicISQueryEnvelope("msg_id_1", "ms.api.reply", sicISQueryPayload )

}
