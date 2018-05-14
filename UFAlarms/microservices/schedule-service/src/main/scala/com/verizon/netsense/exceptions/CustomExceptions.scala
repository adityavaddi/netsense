package com.verizon.netsense.exceptions

import com.datastax.driver.core.exceptions.ReadTimeoutException

/**
  * Created by maidapr on 1/16/18.
  */
object CustomExceptions {

  trait DataNotFoundInGraphDb extends Exception {
    val message: String
    val cause: Throwable
  }

  case class UnableToGetDataFromGraph(override val message: String = "", override val cause: Throwable = None.orNull)
    extends DataNotFoundInGraphDb {
    val underlyingMessage = "Unable to get the node data from graph db "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class NoScheduleFoundGraphDb(override val message: String = "", override val cause: Throwable = None.orNull)
    extends DataNotFoundInGraphDb {
    val underlyingMessage = "No schedule found with scheduleid in the Graph Db "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class NoNodeDataFoundInGraphDb(override val message: String = "", override val cause: Throwable = None.orNull)
    extends DataNotFoundInGraphDb {
    val underlyingMessage = "Node data not found for given node "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class NoScheduleFoundException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends DataNotFoundInGraphDb {
    val underlyingMessage = "Schedule not found for given scheduleid: "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class UnableToGetOrgHierarchyFoundException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends DataNotFoundInGraphDb {
    val underlyingMessage = "Unable to get OrgHierarchy for the node: "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }



  trait DataNotFoundException extends Exception {
    val message: String
    val cause: Throwable
  }

  case class EmptyResultSetException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends DataNotFoundException {
    val underlyingMessage = "No values found for the given request: "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  trait ResultParsingFailureException extends Exception {
    val message: String
    val cause: Throwable
  }

  case class UnableToConvertGraphResultToDTO(override val message: String = "", override val cause: Throwable = None.orNull)
    extends ResultParsingFailureException {
    val underlyingMessage = "Unable to convert the MapAccessor to Domain Object: "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  trait UnableToLogActivityToDb extends Exception {
    val message: String
    val cause: Throwable
  }

  case class UnableToUpdateLightStatus(override val message: String = "", override val cause: Throwable = None.orNull)
    extends UnableToLogActivityToDb {
    val underlyingMessage = "Unable to update the light status "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }


  trait UnableToGetDataFromDbException extends Exception {
    val message: String
    val cause: Throwable
  }

  case class UnableToGetLightStatusFromDbException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends UnableToGetDataFromDbException {
    val underlyingMessage = "Unable to get the result from Cassandra for the light status "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class UnableToGetScheduleDataException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends UnableToGetDataFromDbException {
    val underlyingMessage = "Unable to get the schedule for the schedule "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class MalformedScheduleDataException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends UnableToGetDataFromDbException {
    val underlyingMessage = "The received schedule has no acceptable event for the day "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class NoNodesFoundToSendScheduleException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends DataNotFoundException {
    val underlyingMessage = "No nodes found in schedule to send scheduleEvent "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class NoNodesFoundToSendCommandException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends DataNotFoundException {
    val underlyingMessage = "No nodes found request to send LightingSetAuto "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }
  case class NoNodesFoundToSendForceException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends DataNotFoundException {
    val underlyingMessage = "No nodes found request to send LightingSetAuto "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class NoNodesFoundToSendAutoException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends DataNotFoundException {
    val underlyingMessage = "No nodes found request to send LightingSetAuto "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }


  trait UnableToGetScheduleFromCaselException extends Exception {
    val message: String
    val cause: Throwable
  }

  case class UnableToGetScheduleFromCasel(override val message: String = "", override val cause: Throwable = None.orNull)
    extends UnableToGetScheduleFromCaselException {
    val underlyingMessage = "Unable to cater the apply schedule casel request "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  trait UnableToParseTriggerTimeStampException extends Exception {
    val message: String
    val cause: Throwable
  }

  case class UnableToParseTimeStampException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends UnableToParseTriggerTimeStampException {
    val underlyingMessage = "Unable to parse the given timestamp "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class UndefinedTriggerTimeStampException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends UnableToParseTriggerTimeStampException {
    val underlyingMessage = "TimeStamp corrupt or undefined "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  trait IgnoreThisException extends Exception {
    val message: String
    val cause: Throwable
  }

  case class NoResponseSentToISException(override val message: String = "", override val cause: Throwable = None.orNull)
  extends IgnoreThisException {
    val underlyingMessage = "Event not sent as response"

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  trait TokenValidationException extends Exception {
    val message: String
    val cause: Throwable
  }
  case class InvalidTokenFromNodeException(override val message: String = "", override val cause: Throwable = None.orNull)
  extends TokenValidationException {
    val underlyingMessage = "Unable to resolve the tokens from the node "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class TokenFieldMissingException(override val message: String = "", override val cause: Throwable = None.orNull)
  extends TokenValidationException {
    val underlyingMessage = "Token field missing from node update "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }


}
