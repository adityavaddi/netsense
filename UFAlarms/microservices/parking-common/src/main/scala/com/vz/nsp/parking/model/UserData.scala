package com.vz.nsp.parking.model

import com.outworkers.phantom.udt.Udt
import com.vz.nsp.parking.CommonHelper._
import com.vz.nsp.parking.model.ValidationMessages._

//case class JsonDataValueClass(datum1: String, datum2: String)

/**
@Udt
case class UserDataValueRule(name: Option[String],
                             attrib1: Option[String],
                             attrib2: Option[String],
                             attrib3: Option[String] /*,
                      parkingRule: ParkingRule*/) {
  require(attrib1 != null, RequiredFieldMissing.value + "attrib1")
  require(attrib1.toString.length > 0, "attrib1 " + ShouldBeGreaterThanZero.value)
  require(attrib2 != null, RequiredFieldMissing.value + "attrib2")
  require(attrib2.toString.length > 0, "attrib2 " + ShouldBeGreaterThanZero.value)
  require(attrib3 != null, RequiredFieldMissing.value + "attrib3")
  require(attrib3.toString.length > 0, "attrib3 " + ShouldBeGreaterThanZero.value)

}
  */

case class UserData(appid: String,
                     userid: String,
                    userdataid: String,
                    datavalue: String,
                    createdon: Long,
                    lastupdated: Long,
                   isdeleted: Boolean
                   )


case class UserDataRequest(appid: String,
                           userid: String,
                           datavalue: String
                          ) {
  require(!isEmpty(userid), RequiredFieldMissing.value + "userid")
  //require(!isEmpty(datavalue.toString), RequiredFieldMissing.value + "dataValue")


}


