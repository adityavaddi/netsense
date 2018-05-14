package com.vz.nsp.trigger.model.casel

sealed case class CaselType(value: String)

object CaselType {

  object getTrigger extends CaselType("getTrigger")

  object getAllTriggers extends CaselType("getAllTriggers")

  object postTrigger extends CaselType("createTrigger")

  object deleteTrigger extends CaselType("deleteTrigger")

  object updateTrigger extends CaselType("updateTrigger")

  object filterTrigger extends CaselType("filterTrigger")

  object activeValue extends CaselType("active")

  object inactiveValue extends CaselType("inactive")

  object unassignedValue extends CaselType("unassigned")

  object validationfailed extends CaselType("validationfailed")

}
