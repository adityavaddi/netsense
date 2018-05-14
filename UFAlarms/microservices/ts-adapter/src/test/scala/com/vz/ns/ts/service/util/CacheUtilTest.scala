package com.vz.ns.ts.service.util

import com.google.common.cache.CacheBuilder
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

/**
 * Test class for testing the ObjectMapperUtil object
 *
 * Created by lakssr6
 */
class CacheUtilTest extends FlatSpec with Matchers with BeforeAndAfter with ScalaFutures {

  val cacheUtil = CacheUtil

  import scalacache._
  import guava._

  implicit val scalaCache = ScalaCache(GuavaCache())

  def newGCache = CacheBuilder.newBuilder.build[String, Object]

  behavior of "get"

  it should "return the value stored in the underlying cache" in {
    val underlying = newGCache
    val entry      = Entry("hello", expiresAt = None)
    underlying.put("key1", entry)
    whenReady(GuavaCache(underlying).get[String]("key1")) { result =>
      result should be(Some("hello"))
    }
  }

  it should "return None if the given key does not exist in the underlying cache" in {
    val underlying = newGCache
    whenReady(GuavaCache(underlying).get[String]("non-existent key")) { result =>
      result should be(None)
    }
  }

  behavior of "put"

  it should "store the given key-value pair in the underlying cache with no TTL" in {
    val underlying = newGCache
    GuavaCache(underlying).put("key1", "hello", None)
    underlying.getIfPresent("key1") should be(Entry("hello", None))
  }
}
