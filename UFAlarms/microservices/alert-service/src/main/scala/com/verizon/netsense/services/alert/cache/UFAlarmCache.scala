package com.verizon.netsense.services.alert.cache

import CacheConfig.{cacheHeapEntries, cacheHeapSize, cacheTTL}
import com.verizon.netsense.services.alert.model.UfAlarm

object UFAlarmCache {

  /**
    * Provides EhCache for easy lookups from cache rather than querying database for orghierarchy_by_nodeid;
    * It uses a TTL for each record which expires on [[cacheTTL]] in [[java.time.Duration.ofMillis()]]
    * It has allowed HeapSize in [[org.ehcache.config.units.MemoryUnit.MB]] [[cacheHeapSize]]
    * and heapEntries [[cacheHeapEntries]]
    *
    */
  protected val ufAlarmsCacheConfig: CacheApiConfig[String, (UfAlarm)] =
    CacheApiConfig(cacheHeapEntries,
      heapSize = cacheHeapSize,
      ttl = Some(cacheTTL),
      tti = None,
      classOf[String],
      classOf[UfAlarm],
      "uf_alarms_cache")

  /**
    * Creates an instance of [[CacheApi]] for typed Key, Value and assigns a Cache name
    */
  val ufAlarmCache =
    new CacheApi[String, UfAlarm](ufAlarmsCacheConfig)
}
