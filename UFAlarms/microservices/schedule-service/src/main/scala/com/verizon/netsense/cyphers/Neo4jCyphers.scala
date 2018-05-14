package com.verizon.netsense.cyphers

/**
  * Created by maidapr on 1/10/18.
  */
trait Neo4jCyphers {

  lazy val get_schedule =
    """MATCH (sch:Schedule {scheduleid: {props}.scheduleid})-[:HAS]->(ce:CalendarEvents)
      |      OPTIONAL MATCH (ce)-[:HAS]->(a:Action)
      |
      |WITH COLLECT( DISTINCT [1,2]) AS useless, sch, ce, a
      |ORDER BY a.id
      |
      |WITH COLLECT( DISTINCT {
      |  time: a.time,
      |  level: a.level
      |}) AS actions, sch, ce
      |ORDER BY ce.id
      |
      |WITH COLLECT( DISTINCT {
      |    date: ce.date,
      |    days: ce.days,
      |    actions: actions,
      |    photocell_enabled: ce.photocell_enabled,
      |    photocell_highLevel: ce.photocell_highLevel,
      |    photocell_lowLevel: ce.photocell_lowLevel
      |}) AS events, sch
      |
      |OPTIONAL MATCH (sch)-[:LINKED]->(slg:SiteLightingGroup)
      |WITH COLLECT( DISTINCT
      |    CASE WHEN slg.groupid IS NULL
      |        THEN NULL
      |        ELSE {
      |            groupid: slg.groupid,
      |            name: slg.name
      |        }
      |    END)
      |AS sitesLG, events, sch
      |
      |OPTIONAL MATCH (sch)-[:LINKED]->(g:Group:LightingGroup) WHERE NOT g:SiteLightingGroup
      |WITH COLLECT( DISTINCT
      |    CASE WHEN g.groupid IS NULL
      |        THEN NULL
      |        ELSE {
      |            groupid: g.groupid,
      |            name: g.name
      |        }
      |    END)
      |AS groups, sitesLG, events, sch
      |
      |OPTIONAL MATCH (sch)-[:HAS]->(n:Node)
      |WITH COLLECT( DISTINCT
      |	 CASE WHEN n.nodeid IS NULL
      |        THEN NULL
      |        ELSE {
      |        	nodeid: n.nodeid,
      |		      name: n.name
      |        }
      |    END
      |) AS nodes, groups, sitesLG, events, sch
      |
      |OPTIONAL MATCH (sch)-[:HAS]->(nnw:NoNetwork)
      |WITH COLLECT( DISTINCT {
      |    highTime: nnw.highTime,
      |    highLevel: nnw.highLevel,
      |    lowTime: nnw.lowTime,
      |    lowLevel: nnw.lowLevel,
      |    photocell_enabled: nnw.photocell_enabled,
      |    photocell_highLevel: nnw.photocell_highLevel,
      |    photocell_lowLevel: nnw.photocell_lowLevel
      |})[0] as network, nodes, groups, sitesLG, events, sch
      |
      |RETURN DISTINCT {
      |    scheduleid: sch.scheduleid,
      |    name: sch.name,
      |    description: sch.description,
      |    events: events,
      |    network: network,
      |    sites: sitesLG,
      |    groups: groups,
      |    nodes: nodes
      |} AS schedule
      | """.stripMargin


  lazy val apply_schedule_to_node =
    """MATCH (sch:Schedule {scheduleid: {props}.scheduleid})-[:HAS]->(ce:CalendarEvents)
      |      OPTIONAL MATCH (ce)-[:HAS]->(a:Action),
      |      (n:Node {nodeid: {props}.nodeid})
      |OPTIONAL MATCH (sch)-[:HAS]->(nnw:NoNetwork)
      |OPTIONAL MATCH (other:Schedule)-[old:HAS|BELONGS_TO]-(n) WHERE other <> sch
      |DELETE old
      |MERGE (n)-[:BELONGS_TO]->(sch)
      |MERGE (sch)-[:HAS]->(n)
      |WITH sch, ce, a, COLLECT({
      |     nodeid: n.nodeid,
      |     latitude: n.latitude,
      |     longitude: n.longitude
      | }) AS items, nnw
      |ORDER BY a.id
      |WITH COLLECT ( DISTINCT {
      |     time: a.time,
      |     level: a.level
      |     }) AS actions, sch, ce, nnw, items
      |ORDER BY ce.id
      |WITH COLLECT( DISTINCT {
      |     date: ce.date,
      |     days: ce.days,
      |     actions: actions,
      |     photocell_enabled: ce.photocell_enabled,
      |     photocell_highLevel: ce.photocell_highLevel,
      |     photocell_lowLevel: ce.photocell_lowLevel
      |     }) AS events, sch, nnw, items
      |WITH collect( distinct {
      |     highTime: nnw.highTime,
      |     highLevel: nnw.highLevel,
      |     lowTime: nnw.lowTime,
      |     lowLevel: nnw.lowLevel,
      |     photocell_enabled: nnw.photocell_enabled,
      |     photocell_highLevel: nnw.photocell_highLevel,
      |     photocell_lowLevel: nnw.photocell_lowLevel
      |     })[0] as network, events, sch, items
      |WITH {
      |    scheduleid: sch.scheduleid,
      |    name: sch.name,
      |    description: sch.description,
      |    events: events,
      |    network: network
      |} AS schedule, items
      |RETURN DISTINCT schedule, items""".stripMargin

  lazy val send_assigned_schedule_to_node =
    """MATCH (sch:Schedule {scheduleid: {props}.scheduleid})-[:HAS]->(ce:CalendarEvents)
      |      OPTIONAL MATCH (ce)-[:HAS]->(a:Action)
      |      OPTIONAL MATCH (sch)-[:HAS]->(nnw:NoNetwork)
      |WITH COLLECT ( DISTINCT {
      |     time: a.time,
      |     level: a.level
      |     }) AS actions, ce, nnw, sch
      |WITH COLLECT( DISTINCT {
      |     date: ce.date,
      |     days: ce.days,
      |     actions: actions,
      |     photocell_enabled: ce.photocell_enabled,
      |     photocell_highLevel: ce.photocell_highLevel,
      |     photocell_lowLevel: ce.photocell_lowLevel
      |     }) AS events, nnw, sch
      |WITH collect( distinct {
      |     highTime: nnw.highTime,
      |     highLevel: nnw.highLevel,
      |     lowTime: nnw.lowTime,
      |     lowLevel: nnw.lowLevel,
      |     photocell_enabled: nnw.photocell_enabled,
      |     photocell_highLevel: nnw.photocell_highLevel,
      |     photocell_lowLevel: nnw.photocell_lowLevel
      |     })[0] AS network, events, sch
      |RETURN DISTINCT {
      |       scheduleid: sch.scheduleid,
      |       events: events,
      |       network: network
      |       } AS schedule
      |""".stripMargin


  lazy val get_site_and_org_for_node =
    """MATCH (n:Node)
      |WHERE n.nodeid = {props}.nodeid
      |OPTIONAL MATCH (n)-[:BELONGS_TO]->(s:Site)-[:BELONGS_TO]->(o:Org)
      |OPTIONAL MATCH (sch:Schedule)-[:HAS]->(n)
      |OPTIONAL MATCH (c:Config)-[:BELONGS_TO]->(n)
      |RETURN {
      |    nodeid: n.nodeid,
      |    nodebssid: n.bssid,
      |    nodehw: n.model,
      |    softwareVersion: n.softwareVersion,
      |    nodename: n.name,
      |    nodelabels: labels(n),
      |    siteid: s.siteid,
      |    sitename: s.name,
      |    latitude: s.latitude,
      |    longitude: s.longitude,
      |    siteaddress: s.address,
      |    sitelabels: labels(s),
      |    orgid: o.orgid,
      |    orgname: o.name,
      |    scheduleid: sch.scheduleid,
      |    configid: c.configid
      |} AS data """.stripMargin

  lazy val get_lat_long_for_site = """MATCH (s:Site {siteid: {props}.siteid})
                                     |                     RETURN {
                                     |                     latitude: s.latitude,
                                     |                     longitude: s.longitude
                                     |                     } AS ret""".stripMargin


  lazy val create_schedule = """MATCH (s:Site {siteid: {siteid}})
                                      |MERGE (s)-[:HAS]->(sch:Schedule {scheduleid: {scheduleid}, name: {name}, description: {description}})-[:BELONGS_TO]->(s)
                                      |WITH { props } AS props, sch
                                      |FOREACH (event IN props.events |
                                      |    CREATE (sch)-[:HAS]->(ce:CalendarEvents {
                                      |    date: event.date, days: event.days, id: event.id,
                                      |     photocell_enabled: event.photocell_enabled,
                                      |     photocell_highLevel: event.photocell_highLevel,
                                      |     photocell_lowLevel: event.photocell_lowLevel
                                      |    })-[:BELONGS_TO]->(sch)
                                      |    FOREACH (action IN event.actions |
                                      |        CREATE (ce)-[:HAS]->(a:Action {time: action.time, level: action.level, id: action.id})-[:BELONGS_TO]->(ce)
                                      |    )
                                      |)
                                      |CREATE (sch)-[:HAS]->(nnw:NoNetwork)-[:BELONGS_TO]->(sch)
                                      |SET nnw =
                                      |CASE props.network
                                      |WHEN null
                                      |THEN {}
                                      |ELSE props.network
                                      |END
                                      |SET sch.last_delivered = 0
                                      |""".stripMargin

  lazy val delete_schedule = """MATCH (s:Schedule {scheduleid: {props}.scheduleid})
                               |WHERE NOT (s)-[:LINKED]->(:LightingGroup)
                               |OPTIONAL MATCH (s)-[:HAS]->(ce:CalendarEvents)-[:HAS]->(a:Action)
                               |OPTIONAL MATCH (s)-[:HAS]->(nnw:NoNetwork)-[:BELONGS_TO]->(s)
                               |DETACH DELETE s, ce, a, nnw
                               |""".stripMargin

  lazy val update_last_sent_schedue = """MATCH (s:Schedule {scheduleid: {props}.scheduleid})
                                        | SET s.last_delivered = {props}.now RETURN true""".stripMargin


  
  lazy val get_all_sites_schedules_by_timezone = """MATCH (s:Site)-[:HAS]->(sch:Schedule)-[:HAS]->(n:Node)
                                                  |WHERE s.time_zone in {props}.timezones AND n.model in {props}.models
                                                  |WITH {
                                                  |sitename: s.name,
                                                  |siteid: s.siteid,
                                                  |latitude: s.latitude,
                                                  |longitude: s.longitude,
                                                  |scheduleid: sch.scheduleid,
                                                  |nodeids: collect(distinct(n.nodeid))
                                                  |} AS items
                                                  |RETURN collect(distinct(items)) AS items""".stripMargin

}
