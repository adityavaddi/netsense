package com.sensity.netsense.neo;

import java.io.File;
import java.util.Map;
import java.util.HashMap;

import ch.qos.logback.core.hook.ShutdownHook;

import org.neo4j.graphdb.factory.GraphDatabaseSettings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Result;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseFactory;

import org.json.JSONObject;

/**
 * Created by brefsdal on 7/20/16.
 */
public class CypherEmbeddedImpl {

    final static Logger logger = LoggerFactory.getLogger(CypherEmbeddedImpl.class);
    private GraphDatabaseService db = null;
    private String db_path = null;

    public CypherEmbeddedImpl(String path) {
        File dir = new File(path);
        this.db_path = dir.getAbsolutePath();
        this.db = new GraphDatabaseFactory()
                .newEmbeddedDatabaseBuilder( dir )
                .setConfig(GraphDatabaseSettings.allow_store_upgrade, "true")
                .newGraphDatabase();
        ShutdownHook shutdownHook = new ShutdownHook();
        Runtime.getRuntime().addShutdownHook(shutdownHook);
    }

    private String execute(String cypher, Map<String, Object> params) {
        try (Transaction tx = db.beginTx()) {
            Result result = (params == null) ? db.execute(cypher) : db.execute(cypher, params);
            logger.debug("Cypher: " + cypher);
            tx.success();
            JSONObject jsonObject = new JSONObject();
            while(result.hasNext()) {
                Map<String, Object> row = result.next();
                for ( String key : result.columns() )
                {
                    jsonObject.accumulate(key, row.get(key));
                }
            }
            logger.debug("JSON Object: " + jsonObject.toString());
            return jsonObject.toString(2);
        } catch(Exception exc) {
            JSONObject jsonObject = new JSONObject();
            jsonObject.accumulate("exception", exc.getMessage());
            logger.debug("JSON Object: " + jsonObject);
            return jsonObject.toString(2);
        }
    }

    public String executeCypher(String cypher) {
        return execute(cypher, null);
    }

    public String executeCypher(String cypher, Map nodeprops) {
        logger.debug("nodeprops: " + nodeprops);
        Map<String, Object> params = new HashMap<String, Object>();
        //List<Map<String, Object>> maps = Arrays.asList( nodeprops);
        params.put( "props", nodeprops );
        return execute(cypher, params);
    }

    public String getDbPath() {
        return this.db_path;
    }

    public void stop() {
        if (db != null) {
            db.shutdown();
        }
    }

    private class ShutdownHook extends Thread {
        public void run() {
            logger.info("Shutting down the DB");
            db.shutdown();
        }
    }
}
