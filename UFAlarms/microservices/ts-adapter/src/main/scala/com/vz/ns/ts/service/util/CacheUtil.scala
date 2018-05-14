package com.vz.ns.ts.service.util

import com.vz.ns.ts.service.service.Logging

import scala.concurrent.Future
import scalacache._
import scalacache.guava._

object CacheUtil extends Logging {

  implicit val scalaCache = ScalaCache(GuavaCache())

  def searchCacheForKeyNodeId(nodeId: String): Future[Option[String]] = {
    log.info("Entering CacheUtil::searchCacheForKeyNodeId")
    log.debug(s"with NodeID: $nodeId")
    get(nodeId)
  }
}
