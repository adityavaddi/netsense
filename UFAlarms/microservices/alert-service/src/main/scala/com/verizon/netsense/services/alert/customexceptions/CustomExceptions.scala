package com.verizon.netsense.services.alert.customexceptions

class OrgDataNotFoundException(message: String = "No org data found for the node.", cause: Throwable = None.orNull)
    extends Exception(message, cause)

class AlertEnrichmentException(message: String = "Alarm to Alert Enrichment Failed", cause: Throwable = None.orNull)
    extends Exception(message, cause)

class JsonParsingException(message: String = "Unable parse the received JSON", cause: Throwable = None.orNull)
    extends Exception(message, cause)

class RestPackConversionException(message: String = "Unable to convert alarm to RestPack",
                                  cause: Throwable = None.orNull)
    extends Exception(message, cause)

class CassandraDBException(message: String = "Database Operation Failed.", cause: Throwable = None.orNull)
    extends Exception(message, cause)

class AlarmNotCreatedException(message: String = "Dropping the Invalid alarm received.", cause: Throwable = None.orNull)
    extends Exception(message, cause)

class NotificationIdMissingException(message: String = "Required values are missing from Notification Props",
                                     cause: Throwable = None.orNull)
    extends Exception(message, cause)

class NodeIdMissingException(message: String = "Node Id is missing from Alert Props", cause: Throwable = None.orNull)
    extends Exception(message, cause)

class AlertTypeMissingException(message: String = "Alert Type is missing from Alert Props",
                                cause: Throwable = None.orNull)
    extends Exception(message, cause)

class AlertIdMissingException(message: String = "Required values are missing from Alert Props",
                              cause: Throwable = None.orNull)
    extends Exception(message, cause)

class AlertExistsException(message: String = "Required values are missing from Alert Props",
                           cause: Throwable = None.orNull)
    extends Exception(message, cause)

class InvalidRequestPayloadException(message: String = "Invalid Request Payload", cause: Throwable = None.orNull)
    extends Exception(message, cause)

class UfAlarmPropsMissingException(message: String = "Required values are missing from UfAlarm Props",
                                   cause: Throwable = None.orNull)
    extends Exception(message, cause)

class AlarmTypeExistsException(message: String = "Alarm type already exists in the database",
                               cause: Throwable = None.orNull)
    extends Exception(message, cause)

class UserTypeMissingException(message: String = "User Type is missing from the payload",
                               cause: Throwable = None.orNull)
    extends Exception(message, cause)

class DataNotFoundInDbOrCacheException(message: String = "No Data Found found in Database or Cache",
                                       cause: Throwable = None.orNull)
    extends Exception(message, cause)
