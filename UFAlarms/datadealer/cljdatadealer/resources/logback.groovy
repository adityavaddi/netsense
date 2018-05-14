import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.core.ConsoleAppender
import ch.qos.logback.core.rolling.SizeAndTimeBasedFNATP
import ch.qos.logback.classic.filter.ThresholdFilter
import ch.qos.logback.core.rolling.TimeBasedRollingPolicy
import ch.qos.logback.core.rolling.RollingFileAppender
import net.logstash.logback.appender.LogstashTcpSocketAppender

import net.logstash.logback.encoder.LogstashEncoder
import net.logstash.logback.fieldnames.LogstashFieldNames

import static ch.qos.logback.classic.Level.DEBUG
import static ch.qos.logback.classic.Level.WARN
import static ch.qos.logback.classic.Level.ERROR
import static ch.qos.logback.classic.Level.INFO

def hostName=hostname
def loggingLevel = System.getProperty("logging_level") ?: System.getenv("logging_level")
if (!loggingLevel) {
    loggingLevel = "ERROR"
}
//loggingLevel = "WARN"

def levelObj = ERROR
if (loggingLevel == "DEBUG")
    levelObj = DEBUG
else if (loggingLevel == "WARN")
    levelObj = WARN
else if (loggingLevel == "ERROR")
    levelObj = ERROR
else if (loggingLevel == "INFO")
    levelObj = INFO

def elkServer=System.getProperty("elk_service") ?: System.getenv("elk_service")
def appenderList = ["STDOUT"]
if (elkServer) {
    appenderList.add("stash")
}

def useLogFile=System.getProperty("use_log_file") ?: System.getenv("use_log_file")
if (useLogFile) {
    appenderList.add("FILE")
}

appender("STDOUT", ConsoleAppender) {
    filter(ThresholdFilter) {
        level = loggingLevel
    }
    encoder(PatternLayoutEncoder) {
        pattern = "%d{MMM dd HH:mm:ss.SSS} ${hostName} [%thread] %-5level %logger{36} - %msg%n"
    }
}

appender("FILE", RollingFileAppender) {
    append = true
    file = "/var/log/datadealer/datadealer.log"
    filter(ThresholdFilter) {
        level = loggingLevel
    }
    rollingPolicy(TimeBasedRollingPolicy) {
        fileNamePattern = "/var/log/datadealer/datadealer-%d{yyyy-MM-dd_HH}.%i.log.gz"
        maxHistory = 100
        timeBasedFileNamingAndTriggeringPolicy(SizeAndTimeBasedFNATP) {
            maxFileSize = "100MB"
        }
    }
    encoder(PatternLayoutEncoder) {
        pattern = "%d{MMM dd HH:mm:ss.SSS} ${hostName} %mdc %-4relative [%thread] %-5level %logger{35} - %msg%n"
    }
}
/*
{
  timestamp: 'timestamp of the logging event in UTC',
  level: ' DEBUG | INFO | WARN | ERROR | CRITICAL ',
  // where applicable - if invoked via an external user action
  user: userid,
  type: 'SYSTEM | NETWORK | NODE | SERVER | SERVICE | STORAGE | DB | AUTH | ACTIVITY',
  logger: 'in the following format host/pid/component_name',
  location: 'filename-linenumber',
  object: 'Logging regarding what - e.g. DEVICE ID',
  objectType: 'NODE | SCHEDULE | ... ",
  message: 'Easily readable precise unambiguous message. This message should be enough
  in most cases for somebody to figure out what is going on'
  // the following is required when LOG LEVEL is ERROR or CRITICAL optional for other LEVELs
  diagonstics: Exception, Error or Other Diagonstic Objects received by callback chains or exception handler
}
 */


appender("stash", LogstashTcpSocketAppender) {
    remoteHost = "${elkServer}"
    port = 4560
    filter(ThresholdFilter) {
        level = loggingLevel
    }
    encoder(LogstashEncoder) {
        customFields = """{"hostname": "${hostName}"}"""
        includeCallerData = true
        includeMdc = true
        timeZone = "UTC"
        includeContext = true
        fieldNames(LogstashFieldNames) {
            timestamp = "timestamp"
            logger = "logger"
            callerLine = "line"
        }
    }
}

logger("org.eclipse.jetty", ERROR)
logger("org.apache.kafka.clients.consumer.KafkaConsumer", ERROR)
logger("org.apache.kafka.clients.consumer.internals.Fetcher", ERROR)
logger("org.apache.kafka.clients.consumer.internals.ConsumerCoordinator", ERROR)
logger("org.apache.kafka.clients.NetworkClient", ERROR)
logger("org.apache.kafka.clients.consumer.internals.AbstractCoordinator", ERROR)
logger("org.apache.kafka.common.metrics.Metrics", INFO)
logger("org.apache.kafka.clients.Metadata", ERROR)
logger("org.apache.kafka.streams.processor.internals.StreamPartitionAssignor", ERROR)
logger("org.apache.kafka.streams.processor.internals.StreamThread", ERROR)
logger("org.apache.kafka.streams.processor.internals.StreamTask", ERROR)
logger("org.apache.kafka.streams.processor.internals.RecordCollectorImpl", ERROR)
logger("org.apache.kafka.streams.processor.internals.RecordCollector", ERROR)
logger("org.apache.kafka.common.network.Selector", ERROR)

//logger("activity", DEBUG, ["ACTIVITY-FILE"], false)
root(levelObj, appenderList)
