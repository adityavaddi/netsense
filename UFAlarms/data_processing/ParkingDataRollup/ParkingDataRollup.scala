package com.sensity.rollup

/**
Created by iozsaracoglu on 11/22/2016.

JIRA Ticket: NSN-2992

  Parking data rollup process on Spark.

  Input data:
    Cassandra tables, parking_spot_historic and parking_zone_status

  Config data:
    Cassandra table, aggregation_control

  Output data:
    Cassandra tables, aggregation_parking_spot, aggregation_parking_zone, aggregation_parking_site

  Aggregated elements: Turnovers, occupy time, occupy percentage

  This process is developed to pre-populate rolled-up parking values for all incoming data in pre-defined time intervals (time buckets). Currently we are
  supporting 15min time intervals as the lowest level aggregation unit; resolution is 15 mins. This is set in "Starter.bucketSize" in the code. If this
  value is changed to a lower value, a re-processing of all historic data will be needed. Higher level time buckets (1hr, day, etc) can be computed based
  on the existing lower level aggregations. This can achieved by running futher "groupBy" operations.

  This process is intended to be running periodically in a cron job or any other job scheduler. Note that, currently only a single instance of this process
  should be running. This is not a scalability concern as Spark performs operations in parallel and scales naturally by its internal partitiong of data.
  But, if for any reason we decide to run multiple instances of this process, this can be achieved by dividing data for "siteid" and maintaining seperate
  high water mark records in "aggregation_control" table.

  */

//import java.util.{Calendar, UUID}
import org.apache.spark.{SparkConf, SparkContext, Logging}
import com.datastax.spark.connector._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.{Row}
import org.apache.spark.sql.cassandra.CassandraSQLContext
import org.joda.time._

import org.apache.spark

object Starter extends App {

  // 15 min bucket size (in microseconds) as the lowest level time window for rollups
  val bucketSize = 1000000*60*15;

  /*
  TODO:
  This process should run in a job scheduler (cronjob, etc.) in regular intervals, like every hour, or more frequently depending on rollup speed
  and processing window size. Processing window is a moving window of time, which moves in the universe's time direction. Therefore, if the process
  is run near a singularity, beware of consequences - too many to state in a comment section.

  Logging needs to be setup to follow existing practices.
  */

  // Get the most recent rollup run time. Rollup will run for data after the most recent rollup run time.
  val dtLast = GetHWM.apply() // yyyyMMddHHmm for easier readability

  // Use hard coded time for testing. this should be the current time in prod
  //val rollupUntil = (new DateTime(2016,11,4,17,5)).withMinuteOfHour(0)
  val rollupUntil = DateTime.now().withMinuteOfHour(0)

  // Processing window: dtLast - rollupUntil
  println("Rollup started for data after " + dtLast.toString("yyyy-MM-dd HH:mm") + " until " + rollupUntil.toString("yyyy-MM-dd HH:mm"))

  // Spot level aggregations for the time bucket size (resolution)
  ProcessSpot.run

  // Zone and site level aggregations
  RollupHigherLevels.run

  /*
  TODO:
  Not needed for now but I will need to write back to HWM table. Need to record current time at the very beginning.
  */
  CassandraConnection.sc.stop()
}

object CassandraConnection {
  println("Connecting to Cassandra...")

  /*
  TODO:
  Spark and Cassandra connection parameters should not be hard-coded. Not sure what we do for deployment. Need to find out how this is done here.
  */
  val SparkMaster = "local[1]"
  val SparkAppName = "parking-rollup"
  val CassandraSeed = "52.40.52.243"
  val KeySpace = "farallones"

  val conf = new SparkConf(true) .set("spark.cassandra.connection.host", CassandraSeed)
    .set("spark.cleaner.ttl", "3600")
    .setMaster(SparkMaster)
    .setAppName(SparkAppName)

  lazy val sc = new SparkContext(conf)
  val sqlContext = new CassandraSQLContext(sc)
  sqlContext.setKeyspace(KeySpace)

  println("Connected.")
}

object GetHWM {
  /*
  High water mark for the rollup runs are persisted in a Cassandra table.
  */

  def apply(): DateTime = {
    val DFnew = CassandraConnection.sqlContext.sql(
      "SELECT val  " +
        "FROM   aggregation_control " +
        "WHERE  type = 'parking' " +
        "and    key = 'last_agg_min' " +
        "limit  1"
    )
    var last_agg_min = "na"
    DFnew.collect().foreach(r=> {
      last_agg_min = r.getString(0)
    })

    // HWM value is yyyyMMddHHmm, which has the smallest time unit of minutes
    val dtLast = new DateTime(
      last_agg_min.substring(0,4).toInt,
      last_agg_min.substring(4,6).toInt,
      last_agg_min.substring(6,8).toInt,
      last_agg_min.substring(8,10).toInt,
      last_agg_min.substring(10).toInt
    )
    return dtLast
  }
}

object GetSites {
  /*
  Generates list of distinct siteids. Note that the Cassandra table below has few records; one record per zone. I use the same DataFrame
  also for assigning parking types ("Demarcated", "Undemarcated" or "na") to parking spots. Parking type is defined for a parkingzoneid
  based on what I see from the tables' data. This appears to be the current design.
  */

  val DFparkingZones = CassandraConnection.sqlContext.sql(
    "SELECT siteid, parkinggroupid, parkingzoneid, type " +
      "FROM   parking_zone_status"
  )
  val DFsites = DFparkingZones.select("siteid").distinct().collect()

  def apply(): Array[String] = {
    val siteList = scala.collection.mutable.ArrayBuffer.empty[String]
    println("Sites:")
    var vSiteid = "na"
    DFsites.foreach( siteRow=> {
      if (siteRow.getString(0) != null) {
        vSiteid = siteRow.getString(0)
        siteList += vSiteid
        //println(vSiteid)
      }
    })
    return siteList.toArray
  }
}

object GetDataForSite {
  /*
  All data for a given siteid in the processing window is collected into DataFrame, later to be aggregated and rolled-up to time bucket level (resolution)
  */

  /*
  TODO:
  Look into if I better get the most recent event record for a parkingspotid from the spot level rollup table for performance reasons. If so, I
  will use most recent boolean value as of the bucket-end time. This is not passed to TimeBucket at the moment.
  Currently, I get this value from the source Cassandra table, which will incur a cost of re-processing event records for the initial time bucket.
  I will need to better understand performance of Spark processing on a cluster and make incremental improvements as needed.
  */

  def apply(site: String, from: String, until: String): Array[Row]= {
    val DFnew = CassandraConnection.sqlContext.sql(
      "SELECT parkingspotid, since, occupancy, parkingzoneid " +
        "FROM   parking_spot_historic " +
        "WHERE  siteid = '" + site + "' " +
        "and    since >= " + from + " " +
        "and    since < " + until + " "
      //+ "limit  10"
    )
    return DFnew.orderBy("parkingspotid", "since").collect()
  }
}

object TimeBucket {
  /*
  Keeps track of time buckets and acccumulates its data, then writes the rolled-up values back to aggregation_parking_spot table.
  */

  val bucketSize = Starter.bucketSize

  /*
  TODO:
  Start from the top of the hour, the most recent top of the hour before HWM. Re-processing some records each time, which will be fine from the
  functionality perspective, but we should consider performance cost vs. dealing with data transmission delays. Not sure if we need to worry
  about it right now. What I am really saying is we do need to worry about it.
  */
  val initialBucketStartTime = Starter.dtLast.withMinuteOfHour(0).getMillis * 1000 // start of the hr to handle data delays- in microsecs

  /*
  TODO:
  Look into how we can synchronize the rollup job so that we minimize the need for re-processing. But we may still need to re-process most recent data
  if we want to handle data transfer delays/anomalies/data re-sends. I need to find out about such scenarios.
  */

  var bucketSiteid = "na"
  var bucketGroupid = "na"
  var bucketZoneid = "na"
  var bucketType = "na"
  var bucketSpotid = "na"

  // These are just for a more readable date/time values to help data consunption. They are not used in processing.
  var bucketStart = "na" // yyyy-MM-dd HH:mm
  var bucketEnd = "na" // yyyy-MM-dd HH:mm
  var bucketStartHr = "na" // yyyy-MM-dd HH
  var bucketStartDay = "na" // yyyy-MM-dd

  // Time bucket aggregations, accumulated values
  var startTime = 0L // epoch(microsecs)
  var endTime = 0L // epoch(microsecs)
  var occTime = 0L // duration in microsecs
  var turnovers = 0
  var occPercent = 0.0d

  def writeToCassandra = {
    /*
    Inserts rolled-up values back to a Cassandra table for later consumption by APIs or other ad-hoc data analysis applications.
    */

    /*
    TODO:
    I need to consider bulk writes rather than one Cassandra write for each time bucket. Not sure which is more preferred for Cassandra writes.
    Also, if we choose to persist aggregations elsewhere, this part needs to be reworked. I know that S3 hates frequent small writes like this, but
    Cassandra is probably fine.
    */

    val collection = CassandraConnection.sc.parallelize(Seq((
      bucketSiteid, bucketSpotid, startTime,
      bucketGroupid, bucketZoneid, bucketType, endTime, occTime, turnovers, occPercent, bucketStart, bucketEnd, bucketStartHr, bucketStartDay)
    ))
    collection.saveToCassandra("farallones", "aggregation_parking_spot",
      SomeColumns(
        "siteid", "parkingspotid", "starttime",
        "groupid", "zoneid", "parkingtype", "endtime", "occduration", "turnovers", "occpercent", "startdt", "enddt", "starthr", "startday")
    )
  }

  def newSpot(site: String, group: String, zone: String, typ: String, spot: String) = {
    /*
    New spot detected. Initialize the time bucket.
    */

    bucketSiteid = site
    bucketGroupid = group
    bucketZoneid = zone
    bucketType = typ
    bucketSpotid = spot

    startTime = initialBucketStartTime
    endTime = startTime + bucketSize
    occTime = 0L
    turnovers = 0
    occPercent = 0.0d

    bucketStart = (new DateTime(startTime/1000)).toString("yyyy-MM-dd HH:mm")
    bucketEnd = (new DateTime(endTime/1000)).toString("yyyy-MM-dd HH:mm")
    bucketStartHr = bucketStart.substring(0, 13)
    bucketStartDay = bucketStart.substring(0, 10)
  }
  def addTurnover = {
    /*
    Add a turnover to current bucket
    */

    /*
    TODO:
    Currently I am checking false->true occupy transitions. Need guidance to filter out flapping cases perhaps??, and how please.
    */

    turnovers += 1
  }
  def addOccTime(t: Long) = {
    /*
    Add occupy time to current bucket in microsecs.
    */

    /*
    TODO:
    Need guidance to filter out flapping cases.
    */

    occTime += t
  }
  def nextBucket(until: DateTime): Boolean = {
    /*
    Move the time bucket forward.
    */

    // Reached processing window-end. Do not go more.
    if (until.getMillis()*1000 < endTime+bucketSize) return false

    // Prepare the new bucket
    startTime += bucketSize
    endTime += bucketSize
    bucketStart = (new DateTime(startTime/1000)).toString("yyyy-MM-dd HH:mm")
    bucketEnd = (new DateTime(endTime/1000)).toString("yyyy-MM-dd HH:mm")
    bucketStartHr = bucketStart.substring(0, 13)
    bucketStartDay = bucketStart.substring(0, 10)
    occTime = 0L
    turnovers = 0
    occPercent = 0.0d

    return true
  }
  def endBucket() = {
    /*
    Data for the current bucket is exhausted. Good to finalize it.
    */

    /*
    TODO:
    Do we really need a double data type for occPercent ??
    */
    occPercent = ((occTime.toDouble / bucketSize.toDouble)*100.0).toDouble

    writeToCassandra

    println(bucketSpotid +" for "+startTime.toString+"-"+endTime.toString)

    /*
    println(bucketGroupid)
    println(bucketZoneid)
    println(bucketType)
    println("Occupy time: " + occTime.toString)
    println("Turnovers: " + turnovers)
    println("Occupy percent: " + occPercent)
    println("--")
    */
  }
  def least(num1: Long, num2: Long): Long = {
    /*
    This is needed to handle partially/fully empty buckets. I actually do not need to worry about negative cases. I thought I needed it for some
    hypothetical cases, which I did not really see yet. Not sure I am imagining things now.
    */

    if (num1<0) return num2
    else if (num2<0) return num1
    else if ((num1<0) && (num2<0)) return 0L
    else if (num1>num2) return num2
    else return num1
  }
}

object ProcessSpot {
  // Parking spot level aggregation for the lowest level time bucket (TimeBucket.bucketSize)

  val siteListArray = GetSites.apply()


  def run = {

    /*
    Outer loop for "sites" allows me to follow the existing PK for the parking_spot_historic table. This is how I could implement
    time windowing functionality. This moving time window allows me to detect and process new data based on high water mark
    method, which is minimally invasive to our data source in Cassandra.
    */

    /*
    TODO:
    Look into making a PK change to better serve to high water mark method. But this will probably have a significant impact on the rest of
    components which operate on this table. This means, forget it.
    */

    siteListArray.foreach(s=> {

      println("site: "+s)

      var siteid = s
      var groupid = "na"
      var zoneid = "na"
      var ptype = "na"
      var spotid = "na"

      var prevSpot = "na"
      var prevOccupancy = false
      var prevSince = 0L

      var since = 0L
      var occupancy = false
      var goodToGo = true

      GetDataForSite.apply(siteid, (Starter.dtLast.getMillis * 1000).toString, (Starter.rollupUntil.getMillis * 1000).toString).foreach(r=>{
        /*
        Here we have all the data sorted by parkingspotid, timestamp (since column) PK order.
        */

        spotid = r.getString(0)
        zoneid = r.getString(3)

        since = r.getLong(1)
        occupancy = r.getBoolean(2)

        //println(spotid + "  " + since.toString + "  " + occupancy)

        // New parkingspotid
        if (prevSpot != spotid) {
          /*
          End the buckets for previous spot until processing window-end is reached. Assumption is, the most recent status stays as valid;
          the most recent occupy status is continuing to be valid until the end. That's why the most recent time bucket data may be incomplete
          and should be used with caution.
          */

          // ilker debug
          //if (spotid == "#nFfo1c#Uad}Bzi$>E:?") {
          //    println("at debug spot")
          //}

          if (prevSpot != "na") {
            goodToGo = true
            do {
              if (prevOccupancy) TimeBucket.addOccTime(TimeBucket.least(TimeBucket.bucketSize, TimeBucket.endTime - prevSince))
              TimeBucket.endBucket()
              goodToGo = TimeBucket.nextBucket(Starter.rollupUntil)
            }
            while ((goodToGo))
          }

          prevOccupancy = false
          prevSince = 0L

          // Get the group and parking type from the zone DataFrame.
          GetSites.DFparkingZones.filter("parkingzoneid = '" + zoneid + "' ").select("parkinggroupid", "type").collect().foreach(r=>{
            groupid = r.getString(0)
            ptype = r.getString(1)
          })

          TimeBucket.newSpot(siteid, groupid, zoneid, ptype, spotid)
          goodToGo = true
          println("new spot: " + TimeBucket.startTime + "-" + TimeBucket.endTime)
        }

        if ((since >= TimeBucket.endTime) && goodToGo) {
          // Move the time bucket(s) forward to create/end next bucket(s) until reaching to current record's timestamp.
          do {
            if (prevOccupancy) TimeBucket.addOccTime(TimeBucket.least(TimeBucket.bucketSize, TimeBucket.endTime - prevSince))
            TimeBucket.endBucket()
            goodToGo = TimeBucket.nextBucket(Starter.rollupUntil)
          }
          while ((goodToGo) && since >= TimeBucket.endTime)
        }

        // If we are still in the processing time window, go for it. If not, records are effectively skipped.
        if (goodToGo) {
          if ((since >= TimeBucket.startTime) && (since < TimeBucket.endTime)) {
            // Current record falls into the current time bucket.
            if (occupancy) {
              // Spot is occupied.
              if (!prevOccupancy) TimeBucket.addTurnover // It was empty, now occupied, so add a turnover

              if (prevOccupancy) {
                // Add occupancy time true->true cases, which we apparently regularly receive. Adding time between two trues.
                TimeBucket.addOccTime(TimeBucket.least(since - prevSince, since - TimeBucket.startTime))
              }
            } // end-occupied
            else {
              // Spot is empty.
              if (prevOccupancy) {
                // it was occupied, now it is empty, so adding occupancy time.
                TimeBucket.addOccTime(TimeBucket.least(since - prevSince, since - TimeBucket.startTime))
              }
            } // end-empty
          }
        }

        /*
        So that I have access to previous record's critical values. This allows me to do a single pass on DataFrame. I am moving the record
        pointer (of DataFrame) and the time buckets simultaneously.
        */
        prevSpot = spotid
        prevSince = since
        prevOccupancy = occupancy
      })

      // End of data for a site; end the bucket(s) for the last parking spot until reaching the processing window-end.
      goodToGo = true
      do {
        if (prevOccupancy) TimeBucket.addOccTime(TimeBucket.least(TimeBucket.bucketSize, TimeBucket.endTime - prevSince))
        TimeBucket.endBucket()
        goodToGo = TimeBucket.nextBucket(Starter.rollupUntil)
      }
      while ((goodToGo))

    })
    println("Spot Rollup Done.")
    //log.info("Rollup Done.")

    /*
    TODO:
    Need to follow logging standards for the product.
    */

  }

}

object RollupHigherLevels {
  /*
  Process zone and site level aggregations based on spot level aggregations.
  */

  val from = (Starter.dtLast.withMinuteOfHour(0).getMillis * 1000).toString
  val until = (Starter.rollupUntil.getMillis * 1000).toString

  def run = {
    ProcessSpot.siteListArray.foreach(siteid => {

      /*
      Get the lowest level aggregation records for parking spots in order to "groupBy" values further to higher levels.
      */
      val DFagg = CassandraConnection.sqlContext.sql(
        "SELECT siteid, zoneid, starttime, groupid, parkingtype, endtime, occduration, turnovers, occpercent, startdt, enddt, starthr, startday " +
          "FROM   aggregation_parking_spot " +
          "WHERE  siteid = '" + siteid + "' " +
          "and    starttime >= " + from + " " +
          "and    starttime < " + until
        //+ "limit  10"
      )



      /*
      Zone level rollup for the time buckets.
      */
      DFagg.groupBy("zoneid", "starttime").agg(
        min("siteid"),
        min("groupid"),
        min("parkingtype"),
        min("endtime"),
        avg("occduration"),
        sum("turnovers"),
        avg("occpercent"),
        min("startdt"),
        min("enddt"),
        min("starthr"),
        min("startday")
      ).collect().foreach(R =>{
        //println(R)
        val collection = CassandraConnection.sc.parallelize(Seq((
          R.getString(2), R.getString(0), R.getLong(1),
          R.getString(3), R.getString(4), R.getLong(5), R.getDouble(6).toLong, R.getLong(7).toInt, R.getDouble(8), R.getString(9), R.getString(10), R.getString(11), R.getString(12) )
        ))
        collection.saveToCassandra("farallones", "aggregation_parking_zone",
          SomeColumns(
            "siteid", "zoneid", "starttime",
            "groupid", "parkingtype", "endtime", "occduration", "turnovers", "occpercent", "startdt", "enddt", "starthr", "startday"
          ))
      })



      /*
      Site level rollup for the time buckets.
      */
      DFagg.groupBy("siteid", "starttime", "parkingtype").agg(
        min("endtime"),
        avg("occduration"),
        sum("turnovers"),
        avg("occpercent"),
        min("startdt"),
        min("enddt"),
        min("starthr"),
        min("startday")
      ).collect().foreach(R =>{
        //println(R)
        val collection = CassandraConnection.sc.parallelize(Seq((
          R.getString(0), R.getLong(1), R.getString(2),
          R.getLong(3), R.getDouble(4).toLong, R.getLong(5).toInt, R.getDouble(6), R.getString(7), R.getString(8), R.getString(9), R.getString(10) )
        ))
        collection.saveToCassandra("farallones", "aggregation_parking_site",
          SomeColumns(
            "siteid", "starttime", "parkingtype",
            "endtime", "occduration", "turnovers", "occpercent", "startdt", "enddt", "starthr", "startday"
          ))
      })



    })
  }
}

