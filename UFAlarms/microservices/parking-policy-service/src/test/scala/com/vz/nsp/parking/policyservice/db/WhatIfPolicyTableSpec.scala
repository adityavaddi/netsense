package com.vz.nsp.parking.policyservice.db

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import com.outworkers.phantom.dsl._
import com.vz.nsp.parking.db.EmbeddedDatabase
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.policyservice.config.TestSuiteConfig
import com.vz.nsp.parking.policyservice.constants.TestDataFeed._
import com.vz.nsp.parking.policyservice.db.PolicyConnector._
import org.apache.cassandra.service.{CassandraDaemon, EmbeddedCassandraService}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class WhatIfPolicyTableSpec extends DatabaseSpec with EmbeddedDatabase with TestSuiteConfig {
/* extends TestKit(ActorSystem("WhatIfPolicyTableSpec-System"))
with EmbeddedDatabase
  with BaseSpec{*/

  implicit val ec = ExecutionContext.Implicits.global

 /* implicit val mat = ActorMaterializer()(system)

  lazy val embeddedCassandraServiceObj = new EmbeddedCassandraService
  lazy val cassandraDaemon             = new CassandraDaemon


    def init(): Future[Seq[ResultSet]] = {
      whatIfPolicyTable.WhatIfPolicyCustomTypes()
      whatIfPolicyTable.createWhatIfPolicyTable()
    }

    def cleanup(): Future[ResultSet] =
      whatIfPolicyTable.truncateWhatIfPolicy


  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val start: Unit = embeddedCassandraServiceObj.start()
    init()
    session.init()
  }

  override  protected def afterAll(): Unit ={
    cleanup()
    cassandraDaemon.stop()
    cassandraDaemon.stopNativeTransport()
    super.afterAll()}

*/

  object DatabaseService {
    def init(): Future[Seq[ResultSet]] = {
      whatIfPolicyTable.WhatIfPolicyCustomTypes()
      whatIfPolicyTable.createWhatIfPolicyTable()
    }

    def cleanup(): Future[ResultSet] =
      whatIfPolicyTable.truncateWhatIfPolicy
  }

  override def beforeAll(): Unit = {
    DatabaseService.init()
    session.init()
  }

  override def afterAll(): Unit = {
    DatabaseService.cleanup()
  }

  "WhatIfPolicyTableSpec" should " Store What If Policy " in {

    val storePolicyQuery      = database.WhatIfPolicyTable.storeWhatIfPolicy(policy)
    val executeGetPolicyQuery = Await.result(storePolicyQuery, 20.second)
    executeGetPolicyQuery isExhausted () mustBe true
    executeGetPolicyQuery wasApplied () mustBe true
    executeGetPolicyQuery one ()

  }

  it should "Get WhatIf Policy by id and Version" in {

    val storePolicyQuery      = database.WhatIfPolicyTable.storeWhatIfPolicy(policy)
    val getPolicyQuery        = database.WhatIfPolicyTable.getWhatIfPolicyByIdAndVersion(policyId, 10, orgId, siteId)
    val executeStorageQuery   = Await.result(storePolicyQuery, 20.second)
    val executeGetPolicyQuery = Await.result(getPolicyQuery, 20.second)
    executeGetPolicyQuery mustBe Some(policy)
  }

  it should "Get All What If Policies" in {
    val policy2 = Policy(
      policyIdTwo,
      "newupdateon 13 july",
      siteId,
      orgId,
      Set("Sunday policy"),
      Set(policyLevelViolation),
      false,
      "7653612563",
      14986863999977L,
      14986963999977L,
      false,
      10,
      "new policy",
      Some("Parking description"),
      "Africa/Costarica",
      Some("active"),
      Set(policyRule)
    )

    val storePolicyQuery      = database.WhatIfPolicyTable.storeWhatIfPolicy(policy)
    val storePolicywQuery     = database.WhatIfPolicyTable.storeWhatIfPolicy(policy2)
    val getPolicyQuery        = database.WhatIfPolicyTable.getAllPolicies(orgId, siteId)
    val executeStorageQuery   = Await.result(storePolicyQuery, 20.second)
    val executeUpdatePolicy   = Await.result(storePolicywQuery, 20.second)
    val executeGetPolicyQuery = Await.result(getPolicyQuery, 20.second)
    executeGetPolicyQuery.map(policy => policy).size > 0
  }

  it should "Get All Live What If Policies" in {
    val policy3 = Policy(
      policyIdThree,
      "newupdateon 13 july",
      "siteid",
      "orgid",
      Set("Sunday policy"),
      Set(policyLevelViolation),
      false,
      "7653612563",
      14986863999977L,
      14986963999977L,
      false,
      10,
      "new policy",
      None,
      "Africa/Costarica",
      Some("active"),
      Set(policyRule)
    )

    val storePolicyQuery      = database.WhatIfPolicyTable.storeWhatIfPolicy(policy3)
    val updatePolicyQuery     = database.WhatIfPolicyTable.updateWhatIfPolicyById(policyIdThree, 11, policy3)
    val getPolicyQuery        = database.WhatIfPolicyTable.getAllLiveWhatIfPoliciesById(policyIdThree, "orgid", "siteid")
    val executeStorageQuery   = Await.result(storePolicyQuery, 20.second)
    val executeUpdatePolicy   = Await.result(updatePolicyQuery, 20.second)
    val executeGetPolicyQuery = Await.result(getPolicyQuery, 20.second)
    executeGetPolicyQuery.map(x => x.isDeleted mustBe false)
    executeGetPolicyQuery.map(policy => policy).size > 0
  }

 it should "Update What if Policy By Id and Version" in {
    val policy2 = Policy(
      policyId,
      "newupdateon 13 july",
      "siteid",
      "orgid",
      Set("Sunday policy"),
      Set(policyLevelViolation),
      false,
      "7653612563",
      14986863999977L,
      14986963999977L,
      false,
      10,
      "new policy",
      Some(""),
      "Africa/Costarica",
      Some("active"),
      Set(policyRule)
    )
    val chain = for {
      store <- this.database.policyMappingTable.storePolicy(policy)
      store <- this.database.policyMappingTable.updateByPolicyId(policyId, policy2.version, policy2)
      get <- database.policyMappingTable.getPolicyById(policyId)

    } yield get
    whenReady(chain) { res =>
      res mustBe Some(policy2)
    }
  }

  it should "delete What If Policy with in site and org" in {
    val storageQuery        = database.WhatIfPolicyTable.storeWhatIfPolicy(policy)
    val fetchQuery          = database.WhatIfPolicyTable.deleteByWhatIfPolicyId(policyId, policy.version, orgId, siteId)
    val executeStorageQuery = Await.result(storageQuery, 20.second)
    val getRecordsQuery     = Await.result(fetchQuery, 20.second)
    getRecordsQuery.wasApplied() mustBe true
  }

}
