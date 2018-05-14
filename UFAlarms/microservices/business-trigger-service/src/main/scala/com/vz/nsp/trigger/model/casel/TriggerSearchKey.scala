package com.vz.nsp.trigger.model.casel


sealed case class TriggerSearchKey(value: String)

object TriggerSearchKey {

  object severity extends TriggerSearchKey("severity")

  object triggerName extends TriggerSearchKey("triggerName")

  object triggerCategory extends TriggerSearchKey("triggerCategory")

}