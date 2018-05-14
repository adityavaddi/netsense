import ch.qos.logback.classic.boolex.JaninoEventEvaluator
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.core.ConsoleAppender
import ch.qos.logback.core.FileAppender
import ch.qos.logback.core.boolex.Matcher
import ch.qos.logback.core.filter.EvaluatorFilter
import ch.qos.logback.core.rolling.SizeAndTimeBasedFNATP
import ch.qos.logback.core.rolling.SizeBasedTriggeringPolicy
import ch.qos.logback.core.spi.LifeCycle
import ch.qos.logback.classic.filter.ThresholdFilter
import ch.qos.logback.core.rolling.TimeBasedRollingPolicy
import ch.qos.logback.core.rolling.RollingFileAppender

import net.logstash.logback.encoder.LogstashEncoder
import net.logstash.logback.fieldnames.LogstashFieldNames

import static ch.qos.logback.classic.Level.DEBUG
import static ch.qos.logback.classic.Level.ERROR
import static ch.qos.logback.core.spi.FilterReply.NEUTRAL
import static ch.qos.logback.core.spi.FilterReply.DENY

def byDay = timestamp("yyyyMMdd'T'HHmmss")
/*
appender("STDOUT", ConsoleAppender) {
    encoder(PatternLayoutEncoder) {
        pattern = "%d{MMM dd HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n"
    }
}
*/
appender("FILE", RollingFileAppender) {
    append = true
    file = "/var/log/datadealer/datadealer.log"
    rollingPolicy(TimeBasedRollingPolicy) {
        fileNamePattern = "/var/log/datadealer/datadealer-%d{yyyy-MM-dd_HH}.%i.log.gz"
        maxHistory = 100
        timeBasedFileNamingAndTriggeringPolicy(SizeAndTimeBasedFNATP) {
            maxFileSize = "100MB"
        }
    }
    encoder(PatternLayoutEncoder) {
        pattern = "%d{MMM dd HH:mm:ss.SSS} %mdc %-4relative [%thread] %-5level %logger{35} - %msg%n"
    }
}
/*
appender("ACTIVITY-FILE", FileAppender) {
    file = "logs/activity-${byDay}.log"
    append = true
    encoder(PatternLayoutEncoder) {
        pattern = "%-4relative [%thread] %-5level %logger{35} - %msg %n"
    }
}
*/
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


appender("stash", RollingFileAppender) {
    append = true
    file = "/var/log/datadealer/logstash-datadealer.json"
    rollingPolicy(TimeBasedRollingPolicy) {
        fileNamePattern = "/var/log/datadealer/logstash-datadealer-%d{yyyy-MM-dd_HH}.%i.json.gz"
        maxHistory = 100
        timeBasedFileNamingAndTriggeringPolicy(SizeAndTimeBasedFNATP) {
            maxFileSize = "100MB"
        }
    }
    encoder(LogstashEncoder) {
        includeCallerData = true
        includeMdc = true
        timeZone = "UTC"
        includeContext = true
        LogstashFieldNames aFieldNames = new LogstashFieldNames()
        aFieldNames.timestamp = "timestamp"
        aFieldNames.logger = "logger"
        aFieldNames.callerLine = "line"
        if(aFieldNames instanceof LifeCycle)
            aFieldNames.start()
        fieldNames = aFieldNames
    }
}

logger("org.eclipse.jetty", ERROR)
//logger("activity", DEBUG, ["ACTIVITY-FILE"], false)
root(ERROR, ["FILE", "stash"])
