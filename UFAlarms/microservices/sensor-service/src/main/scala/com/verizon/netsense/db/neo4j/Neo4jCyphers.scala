package com.verizon.netsense.db.neo4j

/**
  * Created by maidapr on 1/10/18.
  */
trait Neo4jCyphers {

  lazy val getFixtureByNodeId: String = """MATCH (n:Node{nodeid:{props}.nodeid})-[:BELONGS_TO]->(f:Fixture)
                                                  |RETURN {nodeid: n.nodeid,
                                                  |nodeType: n.model,
                                                  |minPower0: f.MinPower0,
                                                  |minPower10: f.MinPower10,
                                                  |minPower50: f.MinPower50,
                                                  |minPower100: f.MinPower100,
                                                  |maxPower0: f.MaxPower0,
                                                  |maxPower10: f.MaxPower10,
                                                  |maxPower50: f.MaxPower50,
                                                  |maxPower100: f.MaxPower100
                                                  |} AS fixture """.stripMargin

  lazy val getNodeSiteTimeZoneCypher: String =
    """MATCH(s:Site {siteid:{props}.siteid})-[:HAS]->
      |(n:Node {nodeid:{props}.nodeid})
      | RETURN  {
      | model: n.model,
      | timeZone: s.time_zone
      | } AS NodeModelTimeZone
    """.stripMargin

}