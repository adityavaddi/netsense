package com.vz.nsp.parking.model

object StatusCodes extends Enumeration {

  type StatusCodes = Value

  val BADREQUEST = Value(404)
  val BADREQUESTINPAYLOAD = Value(400)
  val INTERNALSERVERERROR = Value(500)
  val SUCCESS = Value(200)

}
