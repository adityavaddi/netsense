package com.verizon.netsense.utils

/**
  * Created by subrsi9 on 9/26/17.
  */
object VoltageMapper {

   /*
   Note: This is applicable for only core-node

   Node sub-type information. Comma separated decimal integers "<m>,<v>" where

   <m> = major subtype
   for all nodes: 0 = Undetermined
   for v3 nodes: 1 = ZMotion sensor pod type
   2 = SiHawk sensor pod type
   for v4+ nodes: 1 = Fully populated with external pod bus and an auxiliary power relay
   2 = De-populated: no external pod bus and no auxiliary power relay
   <v> = voltage subtype
   for all nodes: 0 = Undetermined
   1 = 277 V
   2 = 480 V

   */

  //Major subtype  for v3 nodes
  lazy val majorTypeV3 = Map(
                        "0"->"Undetermined",
                        "1"->"ZMotion",
                        "2"->"SiHawk")

  //Major subtype  for v4 nodes
  lazy val majorTypeV4 = Map(
                         "0"->"Undetermined",
                         "1"->"Fully populated",
                         "2"->"De-populated")
  //Voltage subtype
  lazy val voltageSubType = Map(
                          "0"->"Undetermined",
                          "1"->"277 V",
                          "2"->"480 V")

}

