package com.verizon.netsense.model;

import org.apache.kafka.common.utils.Crc32;

import java.util.zip.CRC32;

/**
 * Created by brefsdal on 4/7/17.
 */
public class Event {

    private String name;
    private String topic;
    private String event;
    private String uuid;
    private Long timestamp;
    private String rssi;
    private String imei;
    private String ssid;
    private String deviceid;


    public Event(String name, String topic, String event, String uuid,  Long timestamp, String rssi, String imei, String ssid, String deviceid) {
        this.name = name;
        this.topic = topic;
        this.event = event;
        this.uuid = uuid;
        this.rssi = rssi;
        this.imei = imei;
        this.ssid = ssid;
        this.deviceid = deviceid;
        this.timestamp = timestamp;
    }

    public void setRssi(String rssi) {
        this.rssi = rssi;
    }

    public void setImei(String imei) {
        this.imei = imei;
    }

    public void setSsid(String ssid) {
        this.ssid = ssid;
    }

    public void setDeviceid(String deviceid) {
        this.deviceid = deviceid;
    }

    public String getRssi() {

        return rssi;
    }

    public String getImei() {
        return imei;
    }

    public String getSsid() {
        return ssid;
    }

    public String getDeviceid() {
        return deviceid;
    }
    //    @Override
//    public int hashCode() {
//        Crc32 crc = new Crc32();
//
//        crc.update(name.getBytes(), 0, name.getBytes().length);
//        crc.update(topic.getBytes(), 0, topic.getBytes().length);
//        crc.update(event.getBytes(), 0, event.getBytes().length);
//        crc.update(uuid.getBytes(), 0, uuid.getBytes().length);
//        crc.updateLong(timestamp);
//
//        return (int) crc.getValue();
//    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append(this.getClass().getSimpleName()+"(");
        builder.append(name+",");
        builder.append(topic+",");
        builder.append(event+",");
        builder.append(uuid+",");
        builder.append(timestamp.toString()+",");
        if (rssi != null) builder.append(rssi+","); else builder.append("null,");
        if (imei != null) builder.append(imei+","); else builder.append("null,");
        if (rssi != null) builder.append(rssi+","); else builder.append("null,");
        if (deviceid != null) builder.append(deviceid); else builder.append("null");
        builder.append(")");
        return builder.toString();
    }

    public String getName() {
        return name;
    }

    public String getTopic() {
        return topic;
    }

    public String getEvent() {
        return event;
    }

    public String getUuid() {
        return uuid;
    }

    public Long getTimestamp() {
        return timestamp;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setTopic(String topic) {
        this.topic = topic;
    }

    public void setEvent(String event) {
        this.event = event;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    public void setTimestamp(Long timestamp) {
        this.timestamp = timestamp;
    }
}
