package com.verizon.netsense.exceptions


/**
 * Created by maidapr on 5/2/17.
 */
object CustomExceptions {

  trait CorruptedEventException extends Exception {
    val message: String
    val cause: Throwable
  }

  class MessageUnMarshallingException(message: String = "Unable to Unmarshall the elements ",
                                      cause: Throwable = None.orNull)
      extends Exception(message, cause)

  case class EventMissingFieldsException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends CorruptedEventException {
    val underlyingMessage = "Event missing necessary fields "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  trait DeviceModelTimeZoneNotFoundException extends Exception {
    val message: String
    val cause: Throwable
  }

  case class NodeModelNotFoundException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends DeviceModelTimeZoneNotFoundException {
    val underlyingMessage = "Node model not found for given node "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class SiteTimeZoneNotFoundException(override val message: String = "",
                                           override val cause: Throwable = None.orNull)
    extends DeviceModelTimeZoneNotFoundException {
    val underlyingMessage = "Site time_zone not found for the given node "

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


  class ModelSiteTimeZoneNotFoundException(override val message: String = "",
                                           override val cause: Throwable = None.orNull)
    extends DeviceModelTimeZoneNotFoundException {

    val underlyingMessage = "Unable to fetch model/site time_zone for the given node "

    override def getMessage: String = "Unable to fetch model/site time_zone for the given node " + message

    override def getCause: Throwable = cause
  }

  class DeviceModelOutOfScopeException(override val message: String = "Device model Out of scope ",
                                       override val cause: Throwable = None.orNull)
    extends DeviceModelTimeZoneNotFoundException {

    val underlyingMessage = "Node Model not in scope of query "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause

  }

  class UnableToGetHistoricalData(message: String = "", cause: Throwable = None.orNull)
      extends RuntimeException(message, cause) {
    override def getMessage: String = "Failed to get events for " + message
  }

  trait CustomDBException extends Exception {
    val message: String
    val cause: Throwable
  }

  class UnableToGetDataFromDbException(override val message: String = "Unable to get the data from database ",
                                       override val cause: Throwable = None.orNull)
  extends CustomDBException {

    val underlyingMessage = "Unable to get the data from the database "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  trait FixtureHarmlessException extends Exception {
    val message: String
    val cause: Throwable
  }

  case class UnableToGetLtDataFromDB(override val message: String = "", override val cause: Throwable = None.orNull)
    extends FixtureHarmlessException {
    val underlyingMessage = "Unable to get the Lt data for node from db "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class UnableToGetNodeModelFromNeo(override val message: String = "", override val cause: Throwable = None.orNull)
    extends FixtureHarmlessException {
    val underlyingMessage = "Unable to get the node model from neo4j "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class FixtureValueNotFound(override val message: String = "", override val cause: Throwable = None.orNull)
    extends FixtureHarmlessException {
    val underlyingMessage = "Unable to find fixture value"

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  trait FixtureHarmfulException extends Exception {
    val message: String
    val cause: Throwable
  }

  case class UnableToConvertStringToDouble(override val message: String = "", override val cause: Throwable = None.orNull)
    extends FixtureHarmfulException {
    val underlyingMessage = "Unable to convert the String to Int"

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class ExceptionWhenQueryingFixture(override val message: String = "", override val cause: Throwable = None.orNull)
    extends FixtureHarmfulException {
    val underlyingMessage = "Exception occured while querying fixture data from neo4j"

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class DriverLevelOutOfRangeException(override val message: String = "", override val cause: Throwable = None.orNull)
    extends FixtureHarmfulException {
    val underlyingMessage = "Driver level is not in range of 0 to 100"

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }
}
