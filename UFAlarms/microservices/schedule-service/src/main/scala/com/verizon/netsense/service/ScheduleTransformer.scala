package com.verizon.netsense.service

import com.verizon.netsense.constants.Constants
import com.verizon.netsense.exceptions.CustomExceptions.{MalformedScheduleDataException, NoNodesFoundToSendScheduleException, NoScheduleFoundException}
import com.verizon.netsense.model._
import com.verizon.netsense.util.{DateTimeUtil, Solartime, TokenUtil}
import com.verizon.netsense.utils.Logging

case class ScheduleTransformer(timeInMillis: Long = System.currentTimeMillis()) extends Logging with TokenUtil {

  def transform(schedule: LightingSchedule): STSModel = {
    log.info(s"ScheduleTransformer.transform LightingSchedule : ${schedule.toJSON}")
    val nodeids = schedule.nodes.getOrElse(throw new NoNodesFoundToSendScheduleException(schedule.toJSON))
    if(nodeids.isEmpty) throw new NoNodesFoundToSendScheduleException(schedule.toJSON)

    //Calculate the timezone on the basis of lat and long of the site asociated with the schedule
    //We expect the lat and lon string to be convertible to double. No check applied here.
    val tz = DateTimeUtil.getTimezone(schedule.latitude.toDouble, schedule.longitude.toDouble)
    log.debug(s"The timezone of the schedule is $tz")
    val dow = DateTimeUtil.getDayOfWeek(tz)
    val timestamp = DateTimeUtil.getTimeStamp(tz)
    if(schedule.events == null) throw new MalformedScheduleDataException(schedule.toJSON)

    var events = schedule.events.filter({
      e =>
        e.date match {
          case Some(date) => {
            val day = DateTimeUtil.getDayOfWeek(tz, date)
            if (day == dow) true else false
          }
          case None => false
        }
    })
    //We get a date that has preference
    if (events.isEmpty) {
      events = schedule.events.filter({
        e =>
          e.days match {
            case Some(daysList) => {
              val ret = if (daysList.contains(dow)) true else false
              ret
            }
            case None => false
          }
      })
    }
    if(events.isEmpty) throw new MalformedScheduleDataException(schedule.toJSON)
    log.debug(s"Total selected actions: ${events.size.toString}")

    var stsNormalSlots = getNormalActions(events.head, events.head.actions, schedule.latitude.toDouble, schedule.longitude.toDouble, tz)
    log.debug(s"Total actions for normal network is ${stsNormalSlots.size.toString}")

    val stsNetworkSlots = getNetwork(schedule.network,schedule.latitude.toDouble, schedule.longitude.toDouble, tz)
    log.debug(s"Total actions for No Network is ${stsNetworkSlots.size.toString}")

    val id = convertToScheduleKeyValuePair(schedule.scheduleid
      .getOrElse(throw new NoScheduleFoundException(schedule.toJSON)), timeInMillis)

    log.debug("using token for schedule " + id)

    val stsModel = STSModel(id, Some(stsNormalSlots), stsNetworkSlots, nodeids)
    log.info(s"The generated stsModel: ${stsModel.toJSON} ")
    stsModel
  }

  private def getNormalActions(selectedEvent: ScheduleEvent,
                               selectedActions : Vector[Action],
                               lat : Double, lon : Double,
                               tz : String) : Vector[SlotAction] = {
    var stsNormalSlots: Vector[SlotAction] = Vector()

    //By default we take photocell mode to be off for normal network
    val isPhotocell = selectedEvent.photocell_enabled.getOrElse(false)
    isPhotocell match {
      case true => {
        log.debug("The action is in photocell mode")
        val highAmbient = selectedEvent.photocell_lowLevel.getOrElse(Constants.DEFAULT_DRIVER_LEVEL_LOW)
        val lowAmbient = selectedEvent.photocell_highLevel.getOrElse(Constants.DEFAULT_DRIVER_LEVEL_HIGH)
        stsNormalSlots = stsNormalSlots :+ SlotAction(highAmbient, Some(lowAmbient), None, Constants.PHOTOCELL)
      }
      case false => {
        log.debug("The action is in normal mode")
        selectedActions.foreach(act => {
          //In the remote case time is not there let it throw NoSuchElementException
          //Device time is UTC, convert that + sunrise sunset and DST calculations
          val time = Solartime.resolveExpressionToUTC(act.time.get, lat, lon, tz)
          stsNormalSlots = stsNormalSlots :+ SlotAction(act.level.get, None, Some(time), Constants.NORMAL)
        })
      }
    }
    stsNormalSlots
  }

  private def getNetwork(network : Option[Network], lat : Double, lon : Double, tz : String) = {
    var stsNetworkSlots: Vector[SlotAction] = Vector()

    val NoNetworkDefaultLow = 0
    val NoNetworkDefaultHigh = 100

    network match {
      /* Photocell Mode: by default no netwrok is in photocell mode, hence use true in getorelse
         If high or low values are not present we fall back to default 0 or 100
         remember Neo4J/UI model is sending
         photocell_highLevel: driver level when it is dark
         photocell_lowLevel: driver level when it is not dark
      */
      case Some(_network) if _network.photocell_enabled.getOrElse(true) => {
        stsNetworkSlots = stsNetworkSlots :+ SlotAction(
          level = _network.photocell_lowLevel.getOrElse(Constants.DEFAULT_DRIVER_LEVEL_LOW),
          lowLevel = Some(_network.photocell_highLevel.getOrElse(Constants.DEFAULT_DRIVER_LEVEL_HIGH)),
          time = None,
          mode = Constants.PHOTOCELL)
      }
      //If high time and low time are present but somehow we do not have level for that we use default values
      case Some(Network(Some(_highTime), highLevel, Some(_lowTime), lowLevel, _, _, _)) => {
        val highTime = Solartime.resolveExpressionToUTC(_highTime, lat, lon, tz)
        val lowTime = Solartime.resolveExpressionToUTC(_lowTime, lat, lon, tz)
        stsNetworkSlots = stsNetworkSlots :+ SlotAction(highLevel.getOrElse(NoNetworkDefaultHigh), None, Some(highTime), Constants.NORMAL)
        stsNetworkSlots = stsNetworkSlots :+ SlotAction(lowLevel.getOrElse(NoNetworkDefaultLow), None, Some(lowTime), Constants.NORMAL)
      }
      //This case is not considered an exception in datadealer
      //Default is always on. So if no high value is present we send a 100
      case Some(Network(Some(_highTime), highLevel, _, _,_,_,_)) => {
        val highTime = Solartime.resolveExpressionToUTC(_highTime, lat, lon, tz)
        stsNetworkSlots = stsNetworkSlots :+ SlotAction(highLevel.getOrElse(NoNetworkDefaultHigh), None, Some(highTime), Constants.NORMAL)
      }
      //As extension of above case we are handling this scenarion for low light also
      case Some(Network(_, _ , Some(_lowTime), lowLevel, _, _, _)) => {
        val lowTime = Solartime.resolveExpressionToUTC(_lowTime, lat, lon, tz)
        stsNetworkSlots = stsNetworkSlots :+ SlotAction(lowLevel.getOrElse(NoNetworkDefaultLow), None, Some(lowTime), Constants.NORMAL)
      }
      case _ => {
        //We fallback to the default that is photocell mode with default values
        stsNetworkSlots = stsNetworkSlots :+ SlotAction(
          level = Constants.DEFAULT_DRIVER_LEVEL_LOW,
          lowLevel = Some(Constants.DEFAULT_DRIVER_LEVEL_HIGH),
          time = None,
          mode = Constants.PHOTOCELL)
      }
    }
    stsNetworkSlots
  }

}