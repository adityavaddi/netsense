package com.sensity.netsense.zmq;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

/**
 * Created by brefsdal on 10/11/16.
 */
public class AsyncSend {
/*
 *  Lazy Pirate client in Java
 *  http://zguide.zeromq.org/java:lpclient
 *
 */
    final static Logger logger = LoggerFactory.getLogger(AsyncSend.class);

    public static void send(String hostnameAndPort, byte[] message, int retries) {
        ZContext ctx = new ZContext();
        try {
            ZMQ.Socket socket = ctx.createSocket(ZMQ.DEALER);
            if (socket == null) logger.warn(String.format("FAILED TO CREATE SOCKET %s", hostnameAndPort));
            assert (socket != null);
            socket.connect(hostnameAndPort);
            while (retries > 0) {
                boolean success = socket.send(message, 0);
                if (!success) logger.error("ZMQ SEND FAILED...");

                logger.debug("Waiting for reply from device center...");

                int expect_reply = 1;
                while (expect_reply > 0) {
                    ZMQ.PollItem items[] = {new ZMQ.PollItem(socket, ZMQ.Poller.POLLIN)};
                    int rc = ZMQ.poll(items, 200);
                    if (rc == -1)
                        break;

                    if (items[0].isReadable()) {
                        //  We got a reply from the server, must match sequence
                        byte[] reply = socket.recv();
                        if (reply == null) {
                            logger.error("RECEIVED INTERRUPTED REPLY FROM LEGACY");
                            break;      //  Interrupted
                        }
                        if (reply.length > 0) {
                            logger.debug(String.format("Received reply [%s]", new String(reply)));
                            retries = 0;
                            expect_reply = 0;
                        } else
                            logger.error("RECEIVED EMPTY REPLY FROM LEGACY");

                    } else if (--retries == 0) {
                        logger.error(String.format("FAILED TO CONNECT TO LEGACY AFTER %s RETRIES", retries));
                        IFn deref = Clojure.var("clojure.core", "deref");
                        IFn mark = Clojure.var("metrics.meters", "mark!");
                        IFn meter = Clojure.var("dealer.devsvcctrl", "async-connection-failure-meter");
                        mark.invoke(deref.invoke(meter));
                        break;
                    } else {
                        logger.error("RETRYING CONNECTION TO LEGACY");
                        //  Old socket is confused; close it and open a new one
                        ctx.destroySocket(socket);
                        socket = ctx.createSocket(ZMQ.DEALER);
                        socket.connect(hostnameAndPort);
                        //  Send request again, on new socket
                        success = socket.send(message, 0);
                        if (!success) logger.error("ZMQ SEND FAILED...");
                    }
                }
            }
        } catch (Exception e) {
            logger.error("AsyncSend", e);
            throw e;
        } finally {
            ctx.destroy();
        }
    }
}
