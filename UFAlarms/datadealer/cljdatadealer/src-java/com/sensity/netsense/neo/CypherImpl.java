package com.sensity.netsense.neo;

import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.concurrent.TimeUnit;

import static org.neo4j.driver.v1.Values.value;

import org.neo4j.driver.v1.*;

import org.neo4j.driver.v1.exceptions.ServiceUnavailableException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.json.JSONObject;

public class CypherImpl {

    final static Logger logger = LoggerFactory.getLogger(CypherImpl.class);

    private Driver driver = null;
    private String db_path = null;
    

    private void getDriver(List<String> hosts, int port, String username, String password) {
        Config config = Config
                .build()
                .withConnectionLivenessCheckTimeout(1, TimeUnit.SECONDS)
                .withConnectionTimeout(5, TimeUnit.SECONDS)
                .withMaxConnectionPoolSize(10)
                .withConnectionLivenessCheckTimeout(295, TimeUnit.SECONDS)
                .toConfig();

        String routingContext = "bolt";
        if (hosts.size() > 1) {
            routingContext += "+routing";
        }
        for (String host : hosts) {
            try {
                String uri = routingContext + "://" + host + ":" + port;
                AuthToken authToken = AuthTokens.basic(username, password);

                this.driver = GraphDatabase.driver(uri, authToken, config);

                this.db_path = host + ":" + port;
                return;
            }
            catch (ServiceUnavailableException ex)
            {
                logger.warn("Unable to connection to Neo4j host " + host + ":" + port);
            }
            catch(Throwable exc)
            {
                logger.error("Neo4j Exception:", exc);
                exc.printStackTrace();
            }
        }
        throw new ServiceUnavailableException("No valid database URI found");
    }

    public CypherImpl(List<String> hosts, int port) {
        try {
            getDriver(hosts, port, "neo4j", "neo4j1");
            Session s = driver.session();
            logger.debug("Neo4j session is open: " + s.isOpen());
            s.close();
        } catch(Throwable exc) {
            logger.error("Neo4j Exception:", exc);
            exc.printStackTrace();
            throw exc;
        }
        ShutdownHook shutdownHook = new ShutdownHook();
        Runtime.getRuntime().addShutdownHook(shutdownHook);
    }

    public String executeCypher(String cypher) {
        return execute(cypher, null);
    }

    public String executeCypher(String cypher, Map nodeprops) {
        logger.debug("nodeprops: " + nodeprops);
        Map<String, Object> params = new HashMap<String, Object>();
        params.put( "props", nodeprops );
        Value vals = value(params);
        logger.debug("Values: " + vals.toString());
        return execute(cypher, vals);
    }

    private String execute(String cypher, Value params) {
        try (Session session = driver.session()) {
            JSONObject jsonObject = new JSONObject();
            //Transaction tx = session.beginTransaction();
            //StatementResult result = (params == null) ? tx.run(cypher) : tx.run(cypher, params);
            StatementResult result = (params == null) ? session.run(cypher) : session.run(cypher, params);
            //tx.success();

            while (result.hasNext()) {
                Record record = result.next();
                Map<String, Object> row = record.asMap();
                for (String key : record.keys()) {
                    jsonObject.accumulate(key, row.get(key));
                }
            }
            logger.debug("JSON Object: " + jsonObject.toString());
            return jsonObject.toString(2);
        } catch(Throwable exc) {
            JSONObject jsonObject = new JSONObject();
            jsonObject.accumulate("exception", exc.getMessage());
            logger.error("Cypher: " + cypher);
            logger.error("Values: " + params);
            logger.error("JSON Object: " + jsonObject);
            logger.error("Neo4j Exception:", exc);
            exc.printStackTrace();
            return jsonObject.toString(2);
        }
    }

    public String getDbPath() {
        return this.db_path;
    }

    private class ShutdownHook extends Thread {
        public void run() {
            logger.info("Shutting down the DB");
            driver.close();
        }
    }
}
