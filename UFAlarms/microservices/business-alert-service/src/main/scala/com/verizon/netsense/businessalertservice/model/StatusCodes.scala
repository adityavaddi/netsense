package com.verizon.netsense.businessalertservice.model

object StatusCodes extends Enumeration {

  type StatusCodes = Value

  val NOTFOUND          = Value(404)
  val BADREQUEST         = Value(400)
  val INTERNALSERVERERROR = Value(500)
  val SUCCESS = Value(200)

}