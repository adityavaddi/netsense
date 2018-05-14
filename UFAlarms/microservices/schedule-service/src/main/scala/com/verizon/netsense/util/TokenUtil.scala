package com.verizon.netsense.util

import com.verizon.netsense.constants.Constants.{SCHEDULE_TOKEN_DELIMITER_STR, SCHEDULE_TOKEN_PREFIX_STR, SCHEDULE_TOKEN_VALUE_DELIMITER_STR}

/**
  * Created by maidapr on 4/20/18.
  */
trait TokenUtil {
  /**
    * Regex to extract scheduleId from schedule token field of system info message
    * */
  private val scheduleTokenRegex = """^schedule:(\d{13})@([-+*@!#&a-zA-Z0-9]+)""".r


  /**
    * Regex to extract schedule token from schedule_token
    * */
  private val splitTokenValueRegex = """^schedule:(\d{13}@[-+*@!#&a-zA-Z0-9]+)""".r


  /**
    * Regex to extract scheduleId from schedule_token
    * */
  private val splitScheduleIdFromTokenRegex = """\d{13}@([-+*@!#&a-zA-Z0-9]+)""".r

  /**
    * Regex to extract scheduleId & Time from schedule_token
    * */
  private val splitScheduleIdTimeFromToken = """(\d{13})@([-+*@!#&a-zA-Z0-9]+)""".r

  lazy val extractScheduleIdTime: (Option[String]) => (Option[Long], Option[String]) = {
    case Some(tkn) => splitScheduleIdTimeFromToken.findFirstIn(tkn) match {
      case Some(splitScheduleIdTimeFromToken(t, id)) => (Some(t.toLong), Some(id))
      case _ => (None, None)
    }
    case _ => (None, None)
  }

  def timeMillis: Long = System.currentTimeMillis()

  def tokenPrefix(time: Long = timeMillis): String =
    time.toString + SCHEDULE_TOKEN_VALUE_DELIMITER_STR

  lazy val scheduleTokenKey: String = SCHEDULE_TOKEN_PREFIX_STR + SCHEDULE_TOKEN_DELIMITER_STR

  lazy val convertToScheduleKeyValuePair: (String, Long) => (String) = (scheduleId, time) =>
    scheduleTokenKey + tokenPrefix(time) + scheduleId

  lazy val convertScheduleIdToToken: (String, Long) => (String) = (scheduleId, time) =>
    tokenPrefix(time) + scheduleId

  /**
    * Helper function to extract the scheduleId value from scheduleToken
    * */
  lazy val extractScheduleIdFromToken: (String) => (Option[String]) = (token) => {
    splitScheduleIdFromTokenRegex.findFirstIn(token) match {
      case Some(splitScheduleIdFromTokenRegex(scheduleId)) => Some(scheduleId)
      case _                                               => None
    }
  }

  /**
    * Helper function to extract the schedule token value from scheduleToken
    * */
  lazy val extractScheduleTokenValueFromToken: (String) => Option[String] = (token) => {
    scheduleTokenRegex.findFirstIn(token) match {
      case Some(splitTokenValueRegex(tokenValue)) => Some(tokenValue)
      case _                                      => None
    }
  }

  /**
    * Helper function to extract the schedule token from array of tokens
    * */
  val getScheduleTokenFromTokenArray: (Vector[String]) => Option[String] = tks =>
    tks.flatMap(x => scheduleTokenRegex.findFirstIn(x)).headOption match {
      case Some(tokenString) if tokenString.nonEmpty  => extractScheduleTokenValueFromToken(tokenString)
      case _                                          => None
    }

}
