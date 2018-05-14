package com.verizon.netsense.utils

/**
  * Created by refsdbr on 8/18/17.
  */
object DeviceModels {

  sealed abstract class Model(name: String) {

    def compare(that: String): Boolean = this.name.compareToIgnoreCase(that) == 0

    override def toString: String = name
  }

  case object UNODEV2 extends Model("unode-v2")
  case object UNODEV3 extends Model("unode-v3")
  case object UNODEV4 extends Model("unode-v4")
  case object UNODEV5 extends Model("unode-v5")
  case object UNODEV6 extends Model("unode-v6")
  case object UNODEV7 extends Model("unode-v7")
  case object FALCON  extends Model("falcon-q")
  case object MERLIN  extends Model("merlin")
  case object VDK     extends Model("vdkmaster")
  case object CNEXT   extends Model("cnext")

  val models = Seq(UNODEV2, UNODEV3, UNODEV4, UNODEV5, UNODEV6, UNODEV7, FALCON, MERLIN, VDK, CNEXT)
}

class DeviceModel(name: String) extends DeviceModels.Model(name)

object DeviceModel {
  import DeviceModels._

  def apply(name: String): DeviceModels.Model = {
    models.find(_.compare(name)) match {
      case Some(model) => model
      case _ => throw new Exception(s"Model '$name' not found")
    }
  }
}