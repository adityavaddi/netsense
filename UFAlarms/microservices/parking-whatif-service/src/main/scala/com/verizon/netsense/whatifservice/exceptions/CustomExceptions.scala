package com.verizon.netsense.whatifservice.exceptions

/**
 * Created by maleva on 3/30/18.
 */
object CustomExceptions {

  trait WhatifCassandraException extends Exception {
    val message: String
    val cause: Throwable
  }

  case class WhatifCassandraDBException(override val message: String = "", override val cause: Throwable = None.orNull)
      extends WhatifCassandraException {
    val underlyingMessage = "Exception in cassandra call"

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  trait WhatifSparkException extends Exception {
    val message: String
    val cause: Throwable
  }

  case class WhatifSparkCallException(override val message: String = "", override val cause: Throwable = None.orNull)
      extends WhatifSparkException {
    val underlyingMessage = "Exception in spark call"

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class WhatifSparkTimeoutException(override val message: String = "", override val cause: Throwable = None.orNull)
      extends WhatifSparkException {
    val underlyingMessage = "Exception in spark call"

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

  case class InvalidSparkStatus(override val message: String = "", override val cause: Throwable = None.orNull)
      extends WhatifSparkException {
    val underlyingMessage = "Invalid result from Spark"

    override def getMessage: String = underlyingMessage + message

    override def getCause: Throwable = cause
  }

}
