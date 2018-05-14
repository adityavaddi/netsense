package com.vz.nsp.trigger.dblayer


import com.vz.nsp.trigger.db.{PhantomService, ProductionDatabase}
import com.vz.nsp.trigger.model.db.DBTrigger
import com.vz.nsp.trigger.model.casel.{SuccessMessage, TriggerSearchKey}
import com.vz.nsp.trigger.model.casel.TriggerSearchKey._

/**
  * Created by maleva on 2/13/18.
  */

import com.verizon.netsense.utils.Logging
import com.vz.nsp.trigger.model.casel.SuccessMessage

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by donthgo on 7/4/17.
  */
class DbLayer(phantomService: PhantomService) extends Logging {

  implicit val ec = ExecutionContext.Implicits.global

  def storeTrigger(trigger: DBTrigger): Future[SuccessMessage] =
    phantomService
      .storeTrigger(trigger)
      .map(
        x =>
          if (x.wasApplied()) SuccessMessage(true)
          else SuccessMessage(false)
      )

  def getTriggerByTriggerId(orgid: String, siteid: String, triggerid: String): Future[Option[DBTrigger]] =
    phantomService
      .getTrigger(orgid, siteid, triggerid).map {
      case Some(trigger) => if (trigger.isdeleted) None else Some(trigger)
      case None => None
    }


  def getAllTriggers(orgid: String, siteid: String): Future[List[DBTrigger]] =
    phantomService.getAllTrigger(orgid, siteid).map(triggerList => triggerList.filterNot(_.isdeleted))


  def deleteByTriggerId(triggerid: String, siteid: String, orgid: String): Future[SuccessMessage] =
    phantomService
      .deleteByTriggerId(triggerid, siteid, orgid)
      .map(
        x =>
          if (x.wasApplied()) SuccessMessage(true)
          else SuccessMessage(false)
      )

  def updateByTriggerId(triggerid: String, siteid: String, orgid: String, trigger: DBTrigger): Future[SuccessMessage] =
    phantomService
      .updateByTriggerId(triggerid, siteid, orgid, trigger)
      .map(
        x =>
          if (x.wasApplied()) SuccessMessage(true)
          else SuccessMessage(false)
      )

  def searchTrigger(orgid: String, siteid: String, key: String, value: String): Future[List[DBTrigger]] =
    key match {
      case severity.value =>
        getAllTriggers(orgid, siteid).map(
          ba =>
            ba.filter(trigger => trigger.severity.equalsIgnoreCase(value))
        )
      case triggerName.value =>
        getAllTriggers(orgid, siteid).map(
          ba =>
            ba.filter(trigger => trigger.triggername.equalsIgnoreCase(value))
        )
      case triggerCategory.value =>
        getAllTriggers(orgid, siteid).map(
          ba =>
            ba.filter(trigger => trigger.triggercategory.equalsIgnoreCase(value))
        )
      case _ =>
        log.error(s"Found invalid search key $key so filter doesn't apply")
        getAllTriggers(orgid, siteid)
    }

  def triggerExistWithSameName(orgid: String, siteid: String, nameFromUser: String,
                               nameFromDB: Option[String] = None): Future[Boolean] = {
    nameFromDB match {
      case Some(dbName) =>
        if (nameFromUser.equalsIgnoreCase(dbName)) {
          Future {
            false
          }
        }
        else triggerNameDuplicateCheck(orgid, siteid, nameFromUser)
      case None => triggerNameDuplicateCheck(orgid, siteid, nameFromUser)
    }
  }

  private[this] def triggerNameDuplicateCheck(orgid: String, siteid: String, nameFromUser: String): Future[Boolean] = {
    getAllTriggers(orgid, siteid).map(trigger => {
      val triggers = trigger.filter(_.triggername.equalsIgnoreCase(nameFromUser))
      if (triggers.isEmpty) false
      else true
    })
  }
}

object DbLayer {
  def apply(): DbLayer = new DbLayer(new PhantomService with ProductionDatabase {})
}
