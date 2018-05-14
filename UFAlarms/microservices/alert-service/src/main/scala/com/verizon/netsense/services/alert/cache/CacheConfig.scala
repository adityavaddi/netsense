package com.verizon.netsense.services.alert.cache

import com.verizon.netsense.utils.ConfigLoader

object CacheConfig {

  lazy val cacheConfig          = ConfigLoader.config.getConfig("cache")
  lazy val cacheName            = cacheConfig.getString("name")
  lazy val cacheHeapEntries     = cacheConfig.getLong("heap-entries")
  lazy val cacheHeapSize        = cacheConfig.getLong("heap-size")
  lazy val cacheTTL             = cacheConfig.getLong("ttl")
  lazy val cacheTTI             = cacheConfig.getLong("tti")

}
