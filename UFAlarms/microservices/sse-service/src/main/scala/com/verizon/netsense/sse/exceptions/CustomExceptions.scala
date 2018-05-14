package com.verizon.netsense.sse.exceptions

/**
 * Created by maidapr on 5/2/17.
 */
object CustomExceptions {

  trait CustomException extends Exception {
    val message: String
    val cause: Throwable
  }

  class CassandraDBException(override val message: String = "Unable to get the data from database ",
                             override val cause: Throwable = None.orNull)
      extends CustomException {

    val underlyingMessage = "Unable to get the data from the database "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  class OrgDataNotFoundException(override val message: String = "No org and site data found for the node or Unknown",
                                 override val cause: Throwable = None.orNull)
      extends CustomException {

    val underlyingMessage = "Unable to get the org and site data from the database "

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  class EventTypeNotInSSEScopeException(message: String = "", cause: Throwable = None.orNull)
      extends Exception(message, cause) {
    lazy val underLyingMessage = "EventType not in the scope of SSE "
    override val getMessage    = underLyingMessage + message
  }

  case class NoTopicMatchFilter(siteid: String, orgid: String, nodeid: String)
      extends Exception(
        s"No topic match the " +
        s"filter: site: $siteid org: $orgid node: $nodeid "
      )

}
