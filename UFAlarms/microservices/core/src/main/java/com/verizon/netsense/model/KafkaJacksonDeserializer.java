package com.verizon.netsense.model;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.kafka.common.errors.SerializationException;
import org.apache.kafka.common.serialization.Deserializer;
import org.msgpack.jackson.dataformat.MessagePackFactory;

import java.util.Map;

/**
 * Created by brefsdal on 4/7/17.
 */
public class KafkaJacksonDeserializer<T> implements Deserializer<T> {

    private ObjectMapper objectMapper = new ObjectMapper(new MessagePackFactory());
    private Class<T> type;

    @Override
    public void configure(Map<String, ?> props, boolean isKey) {

    }

    @Override
    public T deserialize(String ignored, byte[] bytes) {
        if (bytes == null || bytes.length == 0) {
            return null;
        }

        System.out.println("deserializing bytes " + new String(bytes));

        try {
            return objectMapper.readValue(bytes, type);
        } catch (Exception e) {
            throw new SerializationException(e);
        }
    }

    protected Class<T> getType() {
        return type;
    }

    @Override
    public void close() {

    }
}