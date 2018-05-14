package com.verizon.netsense.utils

/**
  * Created by maidapr on 1/24/18.
  */
object MarshallableImplicits {

  implicit class Unmarshallable(unMarshallMe: String) {
    def toMap: Map[String,Any] = ObjectMapperUtil.toMap(unMarshallMe)
//    def toMapOf[V]()(implicit m: Manifest[V]): Map[String,V] = ObjectMapperUtil.toMapOf[V](unMarshallMe)
    def fromJson[T](implicit m: Manifest[T]): T =  ObjectMapperUtil.fromJson[T](unMarshallMe)
  }

  implicit class Marshallable[T](marshallMe: T) {
    def toJson: String = ObjectMapperUtil.toJson(marshallMe)
  }
}
