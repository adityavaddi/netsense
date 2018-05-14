package com.sensity.netsense.neo4j.embedded;

import org.neo4j.cypher.javacompat.ExecutionEngine;
import org.neo4j.cypher.javacompat.ExecutionResult;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseBuilder;
import org.neo4j.graphdb.factory.HighlyAvailableGraphDatabaseFactory;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Properties;
import java.io.InputStream;

public class Node {
    private static Logger LOG = LoggerFactory.getLogger(Node.class);
    private static String NEO4J_CONFIG_FILEPATH = "conf/neo4j.properties";

    GraphDatabaseService graphDb;

    public static void main(String[] args) throws InterruptedException {

        Node embeddedInstance = new Node();
        embeddedInstance.shutDown();
    }

    public Node() throws InterruptedException {
        Properties properties = getProperties(NEO4J_CONFIG_FILEPATH);
        String dbPath = properties.getProperty("db.path");

        LOG.info("Starting database...");
        LOG.debug("dbPath: {}", dbPath);
        try {
            GraphDatabaseBuilder builder = new HighlyAvailableGraphDatabaseFactory()
                    .newEmbeddedDatabaseBuilder(dbPath);
            LOG.debug("builder created with dbPath");
            builder.loadPropertiesFromFile(NEO4J_CONFIG_FILEPATH);
            LOG.debug("builder loaded with neo4j properties");
            graphDb = builder.newGraphDatabase();
            LOG.debug("graphDb created");

            registerShutdownHook(graphDb);
        }
        catch(Exception e) {
            LOG.debug("graphDb not completed and registered", e);
        }

        int i = 0;
        ExecutionResult result;
        ExecutionEngine engine = new ExecutionEngine(graphDb);
        LOG.debug("about to exercise graphDb");
        while (true) {

            Thread.sleep(1000);
            try (Transaction ignored = graphDb.beginTx()) {
                result = engine.execute("MATCH n RETURN count(n) as count");
                LOG.info(result.dumpToString());
            }
            LOG.info("... " + i++);
        }

    }

    private Properties getProperties(String filepath) {
        Properties properties = new Properties();
        InputStream in = null;
        try
        {
            URL url = new File(filepath).toURI().toURL();
            in = url.openStream();
            properties.load(in);
        }
        catch(IOException e) {
            LOG.error("Unable to load neo4j.properties");
        }
        finally {
            if (in!=null) {
                try{in.close();}catch(IOException e) {LOG.error("Unable to close input stream from properties file");}
            }
        }
        return properties;
    }

    void shutDown() {
        LOG.info("Shutting down database ...");
        graphDb.shutdown();
    }

    private static void registerShutdownHook(final GraphDatabaseService graphDb) {
        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                LOG.info("Shutting down database ...");
                graphDb.shutdown();
            }
        });
    }
}