package com.verizon.netsense.services.gps.exceptions

class OrgDataNotFoundException(message: String = "No org data found for the node.", cause: Throwable = None.orNull)
    extends Exception(message, cause)

class CassandraDBException(message: String = "Database Operation Failed.", cause: Throwable = None.orNull)
  extends Exception(message, cause)

class AppRequestException(message: String = "AppRequest is missing.", cause: Throwable = None.orNull)
  extends Exception(message, cause)

class GpsPropsEmptyException(message: String = "Gps properties are missing.", cause: Throwable = None.orNull)
  extends Exception(message, cause)

class InvalidCaselTypeException(message: String = "Casel Type is invlid.", cause: Throwable = None.orNull)
  extends Exception(message, cause)

class OrgPropsEmptyException(message: String = "Org Property Empty Exception",cause: Throwable = None.orNull)
  extends Exception(message,cause)

class SitePropsEmptyException(message: String = "Site Property Empty Exception",cause: Throwable = None.orNull)
  extends Exception(message,cause)

class InvalidRequestException(message: String = "Invalid Request: required fiels are missing in request body",cause: Throwable = None.orNull)
  extends Exception(message,cause)



