package com.verizon.netsense.services.alert.cache

import java.time.Duration

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.alert.customexceptions.DataNotFoundInDbOrCacheException
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer
import org.ehcache.config.builders.{CacheConfigurationBuilder, CacheManagerBuilder, ExpiryPolicyBuilder, ResourcePoolsBuilder}
import org.ehcache.config.units.MemoryUnit
import scala.collection.JavaConverters._
import org.ehcache.{Cache, CacheManager}

import scala.concurrent.{ExecutionContext, Future}

class CacheApi[K  <: AnyRef, V <: AnyRef](cacheConfig: CacheApiConfig[K, V]) extends Logging with Instrumented {

  private[this] val cacheHits: Timer      = metrics.timer("EhCache-Hits")
  private[this] val cacheMiss: Timer      = metrics.timer("EhCache-Miss")

  /**
    * Creates a EhCache in key, value pair by taking the key.type, value.type from parameters
    *
    * */
  lazy val localCache: Cache[K, V] =
    cacheManager.getCache(cacheConfig.name, cacheConfig.keyClazz, cacheConfig.valueClazz)

  /**
    * Tries to get the Record from the LocalCache if present, else uses the
    *
    * @param key Key of type[[K]] used for performing the lookup
    * @param op Operation used when there is a Cache miss
    *
    * @return a Future of Option value else throws [[DataNotFoundInDbOrCacheException]] which should be caught
    *         by Supervisor in the implemented GraphStage and does a Resume.
    * */
  def getOrElseUpdate(key: K, op: => Future[Option[V]])
                     (implicit ec: ExecutionContext): Future[Option[V]] =
    get(key) match  {
      case a@Some(result) => cacheHits.time(Future.successful(a))
      case _ => cacheMiss.time(op.map { v => put(key,
        v.getOrElse(throw new DataNotFoundInDbOrCacheException(s"No Data Found found in Db or Cache for key: $key")))
        v})
    }

  /**
    * Gets the Optional Value from the local Cache by key
    *
    * @param key Key of type[[K]] used for updating the Record in Cache
    *
    * @return a Future of Option value
    * */
  def get(key: K): Option[V] =
    Option(localCache.get(key))

  /**
    * Creates (or) updates record into the local Cache by key
    *
    * @param key    Key of type[[K]] used for updating the Record in Cache
    * @param value  Value of type[[V]] stored in (key, value) pair in Cache
    *
    * */
  def put(key: K, value: V): Unit =
    localCache.put(key, value)

  /**
    * Removes the value from the local Cache, if present
    *
    * @param key Key of type[[K]] used for performing the Cache
    *
    * */
  def remove(key: K): Unit =
    localCache.remove(key)

  def getAll = {
    localCache.iterator().asScala.map(_.getValue).toList
    //localCache.iterator().asScala.toSet.map(_.getValue).asJava
    //localCache.getAll(keyList).asScala.toList
  }

  def putAll(cacheEntry: Map[K, V]): Unit =
    localCache.putAll(cacheEntry.asJava)

  private val cacheManager: CacheManager = CacheManagerBuilder.newCacheManagerBuilder
    .withCache(cacheConfig.name, cacheConfig.cacheConfig)
    .build(true)

}

case class CacheApiConfig[K <: AnyRef, V <: AnyRef](heapEntries: Long,
                                                    heapSize: Long,
                                                    ttl: Option[Long] = None,
                                                    tti: Option[Long] = None,
                                                    keyClazz: Class[K],
                                                    valueClazz: Class[V],
                                                    name: String) {

  lazy val cacheConfig: CacheConfigurationBuilder[K, V] =
    newCacheConfiguration(heapEntries, heapSize, ttl, tti, keyClazz, valueClazz)

  private def newCacheConfiguration(
                                     heapEntries: Long,
                                     heapSize: Long,
                                     ttl: Option[Long],
                                     tti: Option[Long],
                                     kClazz: Class[K],
                                     vClazz: Class[V]
                                   ): CacheConfigurationBuilder[K, V] = {
    val baseCacheConfigurationBuilder: CacheConfigurationBuilder[K, V] = CacheConfigurationBuilder
      .newCacheConfigurationBuilder(
        kClazz,
        vClazz,
        ResourcePoolsBuilder.heap(heapEntries).offheap(heapSize, MemoryUnit.MB))

    val ttlAttachedCacheConfig: CacheConfigurationBuilder[K, V] = ttl match {
      case Some(ttlValue) =>
        baseCacheConfigurationBuilder
          .withExpiry(ExpiryPolicyBuilder.timeToLiveExpiration(Duration.ofMillis(ttlValue)))
      case _ => baseCacheConfigurationBuilder
    }
    val cacheConfigurationBuilder: CacheConfigurationBuilder[K, V] = tti match {
      case Some(ttiValue) =>
        ttlAttachedCacheConfig
          .withExpiry(ExpiryPolicyBuilder.timeToIdleExpiration(Duration.ofMillis(ttiValue)))
      case _ => ttlAttachedCacheConfig
    }
    cacheConfigurationBuilder
  }

}
