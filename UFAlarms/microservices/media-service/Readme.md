
# Media Service

Micro service to retrieve real time and/or time-based images from a Video Node. Works with RESTPack-MQ protocol only (i.e., Flapjacks or later), not backward compatible with older versions.

### Configuration

1. Bridge Service config: Set "ToKafka.kafka.topic" to "media"

2. Core Config: Set "mqtt.fromTopic" to "v1/+/out/UNSOL/mediaserver/#"

3. Media Service Config: Set the following for "Kafka":

   media-info-topic = "media"
   request-topic = "ISRequestTopic"
   response-topic = "api.reply.interface"
   sic-request-topic = "node.command"
   sic-response-topic = "media"


### SBT

Compile: ` sbt media-service/compile `

Run: ` sbt media-service/run `


#### Sample IS Request:

`{ "messageid" : "818ddb76-6ee2-4dc5-8030-982f1030424b", "responsetopic" : "ms.api.reply", "request" : { "requestid" : "req_1", "type": "SicISQueryPayload", "model": "NodeModel", "action": "CAN_READ", "instanceid" : "inst_1", "timestamp" : "ts_1", "user": "a3cd0a73-60ee-43bd-9674-f4af1d575db3", "nodeprops" : { "nodeid" : "N02c0014e" }, "siteprops": { "siteid": "0af7c52c-55cf-4f54-9e7d-870c01f0c3b6" }, "orgprops": { "orgid": "cc92b979-52fd-49eb-9377-4b7d8c21d19f" }, "sicProps": { "channel" : "1", "res" : "H", "imgTS" : "20171016101050", "imgFormat" : "jpg" } } }`


