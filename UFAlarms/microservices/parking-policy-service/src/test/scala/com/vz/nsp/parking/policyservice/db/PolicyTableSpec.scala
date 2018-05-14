package com.vz.nsp.parking.policyservice.db

import com.outworkers.phantom.dsl._
import com.vz.nsp.parking.db.EmbeddedDatabase
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.policyservice.config.TestSuiteConfig
import com.vz.nsp.parking.policyservice.constants.TestDataFeed._
import com.vz.nsp.parking.policyservice.db.PolicyConnector._

class PolicyTableSpec extends DatabaseSpec with EmbeddedDatabase with TestSuiteConfig {

  object DatabaseService {

    def init() = {
      policyMappingTable.policyCustomTypes
      policyMappingTable.createPolicyTable
    }

    def cleanup() = {
      policyMappingTable.truncatePolicy
    }
  }

  override def beforeAll(): Unit = {
    DatabaseService.init()
    session.init()
  }

  override def afterAll(): Unit = {
    DatabaseService.cleanup()
  }

  behavior of "Store Policy"
  "Store Policy" should "Store Policy " in {

    val future = database.policyMappingTable.storePolicy(policy)
    whenReady(future) { result =>
      result isExhausted() mustBe true
      result wasApplied() mustBe true
      result one()
    }
  }

  behavior of "Get Policy"
  "Get Policy" should "Get Policy by id" in {
    val chain = for {
      store <- this.database.policyMappingTable.storePolicy(policy)
      get <- database.policyMappingTable.getPolicyById(policyId)
    } yield get
    whenReady(chain) { res =>
      res mustBe Some(policy)
    }
  }

  behavior of "Get All Policies"
  "Get All Policies" should "Get All Policies" in {
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

    val chain = for {
      store <- this.database.policyMappingTable.storePolicy(policy)
      store <- this.database.policyMappingTable.storePolicy(policy2)
      get <- database.policyMappingTable.getAllPolicies(orgId, siteId)

    } yield get
    whenReady(chain) { res =>
      res.size mustBe 2
    }
  }

  behavior of "Update Policy"
  "Update Policy" should "Update Policy By Id and Version" in {
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

  behavior of "delete Policy"
  "delete Policy" should "delete Policy by id" in {
    val chain = for {
      store <- this.database.policyMappingTable.storePolicy(policy)
      delete <- this.database.policyMappingTable.deleteByPolicyId(policyId, policy.version)

    } yield delete
    whenReady(chain) { res =>
      res wasApplied() mustBe true
    }
  }
  behavior of "Version history Policy"
  "Version history Policy" should "Version history by Policy id" in {
    val chain = for {
      store <- this.database.policyMappingTable.storePolicy(policy)
      history <- this.database.policyMappingTable.getVersionHistoryById(policyId, orgId, siteId)

    } yield history
    whenReady(chain) { res =>
      res mustBe List(policy)
    }
  }

  behavior of "Version Policy"
  "Version Policy" should "version by Policy id" in {
    val chain = for {
      store <- this.database.policyMappingTable.storePolicy(policy)
      version <- this.database.policyMappingTable.getPolicyByIdAndVerison(policyId, 10, orgId, siteId)

    } yield version
    whenReady(chain) { res =>
      res mustBe Some(policy)
    }
  }

  "Get All Live Policies" should "Get All Live Policies" in {
    val policy3 = Policy(policyIdThree, "newupdateon 13 july", "siteid", "orgid",
      Set("Sunday policy"), Set(policyLevelViolation), false, "7653612563", 14986863999977L, 14986963999977L, false, 10, "new policy", None, "Africa/Costarica", Some("active"), Set(policyRule))
    val chain = for {
      store <- this.database.policyMappingTable.storePolicy(policy3)
      store <- this.database.policyMappingTable.updateByPolicyId(policyIdThree, 11, policy3)
      get <- database.policyMappingTable.getAllLivePoliciesById(policyIdThree, "orgid", "siteid")
    } yield get
    whenReady(chain) { res =>
      res.map(x => x.isDeleted.equals(false)).size mustBe 2
    }
  }


  "Get Max Version of Policy" should "Get Max Version of Policy" in {
    val policy4 = Policy(policyIdFour, "newupdateon 13 july", "siteid", "orgid",
      Set("Sunday policy"), Set(policyLevelViolation), false, "7653612563", 14986863999977L, 14986963999977L, true, 10, "new policy", None, "Africa/Costarica", Some("active"), Set(policyRule))
    val chain = for {
      store <- this.database.policyMappingTable.storePolicy(policy4)
      store <- this.database.policyMappingTable.updateByPolicyId(policyIdFour, 11, policy4)
      version <- database.policyMappingTable.getMaxVersionById(policyIdFour)
    } yield version

    whenReady(chain) { res =>
      res mustBe 11
    }
  }

  "Get Policy by Tag Id" should "Get Policy by Tag Id" in {
    val policy5 = Policy(policyIdFive, "newupdateon 13 july", "siteid", "orgid",
      Set("Tag policy"), Set(policyLevelViolation), false, "7653612563", 14986863999977L, 14986963999977L, true, 10, "new policy", None, "Africa/Costarica", Some("active"), Set(policyRule))
    val policy6 = Policy(policyIdSix, "newupdateon 13 july", "siteid", "orgid",
      Set("Tag policy"), Set(policyLevelViolation), false, "7653612563", 14986863999977L, 14986963999977L, true, 10, "new policy", None, "Africa/Costarica", Some("active"), Set(policyRule))
    val policy7 = Policy(policyIdSeven, "newupdateon 13 july", "siteid", "orgid",
      Set("Missing policy"), Set(policyLevelViolation), false, "7653612563", 14986863999977L, 14986963999977L, true, 10, "new policy", None, "Africa/Costarica", Some("active"), Set(policyRule))
    val chain = for {
      store <- this.database.policyMappingTable.storePolicy(policy5)
      store <- this.database.policyMappingTable.storePolicy(policy6)
      store <- this.database.policyMappingTable.storePolicy(policy7)
      tag <- database.policyMappingTable.getPolicyByTag("Tag policy")
    } yield tag

    whenReady(chain) { res =>
      res.map(x => x.tags.contains("Tag policy")).size == 2
      res.filter(y => y.tags.contains("Missing policy")).size == 1
    }
  }


  "Get Policy by Tag Org Site Ids" should "Get Policy by Tag Org Site Ids" in {
    val policy5 = Policy(policyIdFive, "newupdateon 13 july", "Lowell", "Verizon",
      Set("Tag policy"), Set(policyLevelViolation), false, "7653612563", 14986863999977L, 14986963999977L, true, 10, "new policy", None, "Africa/Costarica", Some("active"), Set(policyRule))
    val policy6 = Policy(policyIdSix, "newupdateon 13 july", "Lowell", "Verizon",
      Set("Tag policy"), Set(policyLevelViolation), false, "7653612563", 14986863999977L, 14986963999977L, true, 10, "new policy", None, "Africa/Costarica", Some("active"), Set(policyRule))
    val chain = for {
      store <- this.database.policyMappingTable.storePolicy(policy5)
      store <- this.database.policyMappingTable.storePolicy(policy6)
      tag <- database.policyMappingTable.getPolicyByTagidWithinOrg("Tag policy", "Verizon", "Lowell")
    } yield tag

    whenReady(chain) { res =>
      res.map(x => x.tags.contains("Tag policy"))
      res.size >= 2
    }
  }

}


