package com.vz.nsp.parking.model

import com.vz.nsp.parking.CommonHelper._
import com.vz.nsp.parking.model.ValidationMessages.RequiredFieldMissing

case class ParkingGroupPolicyLink(uid: String,
                                  parkinggroupid: String,
                                  policyid: String,
                                  version: Int,
                                  startTime: Long,
                                  endTime: Long)

case class ParkingGroupPolicyLinkRequest(parkinggroupid: String) {
  require(!isEmpty(parkinggroupid), RequiredFieldMissing.value + "parkinggroupid")
}

case class ActivePolicyResponse(policyid: String, version: Int, startTime: String, endTime: String)

case class ParkingGroupInputList(gpId: String, stTime: Long, edTime: Long, version: Int)

case class ParkingGroupOutputList(parkinggroupid: String, startTime: String, endTime: String, version: Int)
