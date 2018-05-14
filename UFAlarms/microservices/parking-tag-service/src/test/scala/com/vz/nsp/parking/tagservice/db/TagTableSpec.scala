package com.vz.nsp.parking.tagservice.db

import com.outworkers.phantom.dsl._
import com.vz.nsp.parking.db.EmbeddedDatabase
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.tagservice.constants.TestDataFeed._
import com.vz.nsp.parking.tagservice.db.TagConnector._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class TagTableSpec extends DatabaseSpec with EmbeddedDatabase {


object DatabaseService {

  def init() = {
    Await.ready(tagTable.createTable(), 5.seconds)

  }

  def cleanup() = {
    Await.ready(tagTable.truncateTable(), 5.seconds)
  }

}

  override def beforeAll(): Unit = {
    DatabaseService.init()
    session.init()
  }

  override def afterAll(): Unit = {
    DatabaseService.cleanup()

  }
  "Concrete Tag" should "Store Tag" in {

    val future = database.TagTable.store(tag)
    whenReady(future) { result =>
      result isExhausted() mustBe true
      result wasApplied() mustBe true
      result one()
    }
  }

  it should "getting Tag by id" in {
    val chain = for {
      store <- this.database.TagTable.store(tag)
      get <- database.TagTable.getTagById(orgId, siteId, tag.uid)

    } yield get
    whenReady(chain) { res =>
      res mustBe Some(tag)
    }
  }

  it should "get all Tags" in {
    val chain = for {
      store <- this.database.TagTable.store(tag)
      store <- this.database.TagTable.store(tagTwo)
      get <- database.TagTable.getAllTag(orgId, siteId)

    } yield get
    whenReady(chain) { res =>
      res.size mustBe 2
    }
  }

  it should "update Tag" in {
    val tag2 = Tag(tagId, "Tag2", "Tag2 des", orgId, siteId, timeInMillis, timeInMillis, false)
    val chain = for {
      store <- this.database.TagTable.store(tag)
      store <- this.database.TagTable.updateByTagId(tagId, tag2)
      get <- database.TagTable.getTagById(orgId, siteId, tagId)

    } yield get
    whenReady(chain) { res =>
      res mustBe Some(tag2)
    }
  }

  it should "delete Tag by id" in {
    val chain = for {
      store <- this.database.TagTable.store(tag)
      delete <- this.database.TagTable.deleteByTagId(tagId)

    } yield delete
    whenReady(chain) { res =>
      res wasApplied() mustBe true
    }
  }
}
