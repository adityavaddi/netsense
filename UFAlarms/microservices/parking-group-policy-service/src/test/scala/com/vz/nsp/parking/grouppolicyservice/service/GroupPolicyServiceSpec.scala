package com.vz.nsp.parking.grouppolicyservice.service

import java.util.{Calendar, UUID}

import com.vz.nsp.parking.db.{PhantomService, ProductionDatabase}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.grouppolicyservice.BaseSpec
import com.vz.nsp.parking.grouppolicyservice.constants.TestDataFeed._
import com.vz.nsp.parking.grouppolicyservice.db.parkingServicesDbTest._
import com.vz.nsp.parking.model.CaselType.productionApi
import com.vz.nsp.parking.model._
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * Created by thangth on 8/17/17.
  */
class GroupPolicyServiceSpec extends BaseSpec {

  object DatabaseService {

    def init() = {
      Await.ready(policyMappingTable.customTypesPolicy(), 15.seconds)
      Await.ready(policyMappingTable.createPolicyTable, 15.seconds)
      Await.ready(parkingGroupPolicyLinkTable.createGroupTable(), 5.seconds)

    }

    def cleanup() = {
      Await.ready(policyMappingTable.truncatePolicy, 5.seconds)
      Await.ready(parkingGroupPolicyLinkTable.truncateTable(), 5.seconds)
    }

  }

  override def beforeAll(): Unit = {
    DatabaseService.init()
    session.init()
  }

  override def afterAll(): Unit = {
    DatabaseService.cleanup()

  }


  val uid = UUID.randomUUID().toString
  val uid1 = UUID.randomUUID().toString
  val uid3 = UUID.randomUUID().toString

  val messageId = UUID.randomUUID().toString
  val messageId1 = UUID.randomUUID().toString
  val messageId2 = UUID.randomUUID().toString
  val messageId3 = UUID.randomUUID().toString

  val instanceId = "1"
  val resTopic = "responseTopic"

  val requestId = UUID.randomUUID().toString

  val parkingGroupId = UUID.randomUUID().toString
  val parkingGroupId1 = UUID.randomUUID().toString
  val parkingGroupId2 = UUID.randomUUID().toString
  val parkingGroupId3 = UUID.randomUUID().toString

  val policyId = UUID.randomUUID().toString
  val policyId1 = UUID.randomUUID().toString
  val policyId2 = UUID.randomUUID().toString
  val policyId3 = UUID.randomUUID().toString

  val successMessage = SuccessMessage(true)
  val failureMessage = SuccessMessage(false)
  val futureSuccessMessage = Future.successful(successMessage)
  val futureFailureMessage = Future.successful(failureMessage)
  val failure = Future.successful(Nil)

  val someParkingGroupInputList = Some(new ParkingGroupPolicyLinkRequest(parkingGroupId))
  val someParkingGroupInputList3 = Some(new ParkingGroupPolicyLinkRequest(parkingGroupId3))

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid, parkingGroupId, policyId, 1, Calendar.getInstance().getTimeInMillis, -1)
  val parkingGroupPolicyLink1 = ParkingGroupPolicyLink(uid1, parkingGroupId1, policyId1, 1, Calendar.getInstance().getTimeInMillis, -1)
  val parkingGroupPolicyLink3 = ParkingGroupPolicyLink(uid3, parkingGroupId3, policyId3, 1, Calendar.getInstance().getTimeInMillis, -1)

  val policyAssociationAppRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
    ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None,
      None, None, None, None, None), AppUserDataProps(None, "", "", None)))


  val policyDisAssociationAppRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "policydisassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
    ConfigProps(Some(policyId1), None, None, Some(parkingGroupId1), None, None, None, None, None,
      None, None, None, None, None), AppUserDataProps(None, "", "", None)))

  val policyAssociatedParkingGroupsAppRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "associatedparkinggroups", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
    ConfigProps(Some(policyId2), None, None, Some(parkingGroupId2), None, None, None, None, None,
      None, None, None, None, None), AppUserDataProps(None, "", "", None)))

  val policyAssociationAppRequestValidPolicyLink = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
    ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None,
      None, someParkingGroupInputList, None, None), AppUserDataProps(None, "", "", None)))

  val policyAssociationAppRequestValidPolicyLink1 = AppRequest(messageId1, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
    ConfigProps(Some(policyId1), None, None, Some(parkingGroupId1), None, None, None, None, None,
      None, None, someParkingGroupInputList, None, None), AppUserDataProps(None, "", "", None)))

  val policyAssociationAppRequestValidPolicyLink2 = AppRequest(messageId2, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
    ConfigProps(Some(policyId2), None, None, Some(parkingGroupId2), None, None, None, None, None,
      None, None, someParkingGroupInputList, None, None), AppUserDataProps(None, "", "", None)))

  val policyAssociationAppRequestValidPolicyLink3 = AppRequest(messageId3, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
    ConfigProps(Some(policyId3), None, None, Some(parkingGroupId3), None, None, None, None, None,
      None, None, someParkingGroupInputList3, None, None), AppUserDataProps(None, "", "", None)))

  val policyAssociationAppRequestNoPolicyID = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
    ConfigProps(None, None, None, Some(parkingGroupId), None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))

  val policyAssociationAppRequestNoPolicyLink = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
    ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))

  val dbLayer = spy(new DbLayer(new PhantomService with ProductionDatabase {}))
  val groupPcHelper = spy(new GroupPolicyService(dbLayer))

  val parkingGroupPolicy = Future.successful(
    List(ParkingGroupPolicyLink(uid, parkingGroupId, policyId, 1, Calendar.getInstance().getTimeInMillis, -1)))

  val policy: Policy = Policy(uid, "newupdateon 13 july", siteid, orgid, Set("Sunday policy"), Set(policyLevelViolation),
    false, "7653612563", 14986863999977L, 14986963999977L, false, 10, "new policy",
    Some("Policy description"), "Africa/Costarica", None, Set(policyRule))


  /* Method: groupPolicyAssociationProcess */
  "groupPolicyAssociationProcess" should "return Success" in {

    val messageId = UUID.randomUUID().toString
    val policyId = UUID.randomUUID().toString
    val parkingGroupId = UUID.randomUUID().toString

    val someParkingGroupInputList = Some(new ParkingGroupPolicyLinkRequest(parkingGroupId))

    val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid, parkingGroupId, policyId, 10, Calendar.getInstance().getTimeInMillis, -1)

    val policyAppRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None, None,
        someParkingGroupInputList, None, None), AppUserDataProps(None, "", "", None)))

    doReturn(Future(10)).when(dbLayer).getMaxVersionById(policyId,productionApi.value)
    doReturn(Future(List(policy))).when(dbLayer).getPolicyByIdAndVersion(policyId, 10, orgid, siteid,productionApi.value)
    doReturn(failure).when(dbLayer).checkGroupLinkedWithPolicy(parkingGroupId)
    doReturn(futureSuccessMessage).when(dbLayer).updateStateOfPolicy(policyId, 10, Some("active"))
    doReturn(futureSuccessMessage).when(dbLayer).applyGroupPolicyLink(parkingGroupPolicyLink, 10)

    val chain = for {
      store <- groupPcHelper.groupPolicyAssociationProcess(orgid, siteid, policyAppRequest)
    } yield store


    whenReady(chain, timeout(Span(5, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[ParkingGroupPolicyLink]].messageid.equals(messageId))
    }
  }


  /* Method: getMaxVersionAndInsertRecord */
  "getMaxVersionAndInsertRecord" should "Returns Success" in {

    val messageId = UUID.randomUUID().toString
    val policyId = UUID.randomUUID().toString
    val parkingGroupId = UUID.randomUUID().toString

    val version = 10

    val someParkingGroupInputList = Some(new ParkingGroupPolicyLinkRequest(parkingGroupId))

    val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid, parkingGroupId, policyId, version, Calendar.getInstance().getTimeInMillis, -1)

    val policyAppRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None,
        None, someParkingGroupInputList, None, None), AppUserDataProps(None, "", "", None)))

    doReturn(Future(version)).when(dbLayer).getMaxVersionById(policyId,productionApi.value)
    doReturn(futureSuccessMessage).when(dbLayer).applyGroupPolicyLink(parkingGroupPolicyLink, version)

    val chain = for {
      store <- groupPcHelper.getMaxVersionAndInsertRecord(parkingGroupPolicyLink, policyAppRequest)
    } yield store


    whenReady(chain, timeout(Span(5, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[ParkingGroupPolicyLink]].messageid.equals(messageId))
    }
  }

  "getMaxVersionAndInsertRecord" should "Returns Failure" in {

    val messageId = UUID.randomUUID().toString
    val policyId = UUID.randomUUID().toString
    val parkingGroupId = UUID.randomUUID().toString

    val version = 10

    val someParkingGroupInputList = Some(new ParkingGroupPolicyLinkRequest(parkingGroupId))

    val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid, parkingGroupId, policyId, version, Calendar.getInstance().getTimeInMillis, -1)

    val policyAppRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None,
        None, someParkingGroupInputList, None, None), AppUserDataProps(None, "", "", None)))

    doReturn(Future(version)).when(dbLayer).getMaxVersionById(policyId,productionApi.value)
    doReturn(futureFailureMessage).when(dbLayer).applyGroupPolicyLink(parkingGroupPolicyLink, version)

    val chain = for {
      store <- groupPcHelper.getMaxVersionAndInsertRecord(parkingGroupPolicyLink, policyAppRequest)
    } yield store


    whenReady(chain, timeout(Span(5, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  /* Method: associatingPolicyToGroup */
  "associatingPolicyToGroup" should "Returns Success" in {

    val version = 2

    doReturn(failure).when(dbLayer).checkGroupLinkedWithPolicy(parkingGroupId)
    doReturn(Future(version)).when(dbLayer).getMaxVersionById(policyId,productionApi.value)
    doReturn(futureSuccessMessage).when(dbLayer).updateStateOfPolicy(policyId, version, Some("active"))
    doReturn(parkingGroupPolicyLink).when(groupPcHelper).generateGroupPolicyObject(policyId, parkingGroupId)
    doReturn(futureSuccessMessage).when(dbLayer).applyGroupPolicyLink(parkingGroupPolicyLink, version)

    val chain = for {
      store <- groupPcHelper.associatingPolicyToGroup(policyId, parkingGroupId, policyAssociationAppRequestValidPolicyLink)
    } yield store

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[ParkingGroupPolicyLink]].messageid.equals(messageId))
    }
  }

  "associatingPolicyToGroup" should "Return Error Response when 'applyGroupPolicyLink' returns failed future, error with \"Specified parking group is already associated with other policy\" " in {

    val version = 2

    doReturn(parkingGroupPolicy).when(dbLayer).checkGroupLinkedWithPolicy(parkingGroupId)
    doReturn(parkingGroupPolicyLink).when(groupPcHelper).generateGroupPolicyObject(policyId, parkingGroupId)
    doReturn(futureSuccessMessage).when(dbLayer).updateStateOfPolicy(policyId, version, Some("active"))
    doReturn(Future(version)).when(dbLayer).getMaxVersionById(policyId,productionApi.value)
    doReturn(futureFailureMessage).when(dbLayer).applyGroupPolicyLink(parkingGroupPolicyLink, version)

    val chain = for {
      store <- groupPcHelper.associatingPolicyToGroup(policyId, parkingGroupId, policyAssociationAppRequestValidPolicyLink)
    } yield store


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  "associatingPolicyToGroup" should "Return Error Response when 'updateStateOfPolicy' returns failed future, error with \"Specified parking group is already associated with other policy\" " in {

    val version = 2

    doReturn(parkingGroupPolicy).when(dbLayer).checkGroupLinkedWithPolicy(parkingGroupId)
    doReturn(parkingGroupPolicyLink).when(groupPcHelper).generateGroupPolicyObject(policyId, parkingGroupId)
    doReturn(futureFailureMessage).when(dbLayer).updateStateOfPolicy(policyId, version, Some("active"))
    doReturn(Future(version)).when(dbLayer).getMaxVersionById(policyId,productionApi.value)
    doReturn(futureFailureMessage).when(dbLayer).applyGroupPolicyLink(parkingGroupPolicyLink, version)

    val chain = for {
      store <- groupPcHelper.associatingPolicyToGroup(policyId, parkingGroupId, policyAssociationAppRequestValidPolicyLink)
    } yield store


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  "associatingPolicyToGroup" should "Return Error Response when 'updateStateOfPolicy' returns failed future & 'applyGroupPolicyLink' returns success future, error with \"Specified parking group is already associated with other policy\" " in {

    val version = 2

    doReturn(parkingGroupPolicy).when(dbLayer).checkGroupLinkedWithPolicy(parkingGroupId)
    doReturn(parkingGroupPolicyLink).when(groupPcHelper).generateGroupPolicyObject(policyId, parkingGroupId)
    doReturn(futureFailureMessage).when(dbLayer).updateStateOfPolicy(policyId, version, Some("active"))
    doReturn(Future(version)).when(dbLayer).getMaxVersionById(policyId,productionApi.value)
    doReturn(futureSuccessMessage).when(dbLayer).applyGroupPolicyLink(parkingGroupPolicyLink, version)

    val chain = for {
      store <- groupPcHelper.associatingPolicyToGroup(policyId, parkingGroupId, policyAssociationAppRequestValidPolicyLink)
    } yield store


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  "associatingPolicyToGroup" should "Return Error Response when no valid input received from underlying layers -\"Specified parking group is already associated with other policy\" " in {

    doReturn(parkingGroupPolicy).when(dbLayer).checkGroupLinkedWithPolicy(parkingGroupId)

    val chain = for {
      store <- groupPcHelper.associatingPolicyToGroup(policyId, parkingGroupId, policyAssociationAppRequestValidPolicyLink)
    } yield store


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  "associatingPolicyToGroup" should "return Failure Response \"Internal server error while associating parking group to policy\" " in {

    val chain = for {
      appResponse <- groupPcHelper.associatingPolicyToGroup(policyId, parkingGroupId, policyAssociationAppRequestValidPolicyLink)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  "associatingPolicyToGroup" should "return Success " in {

    doReturn(failure).when(dbLayer).checkGroupLinkedWithPolicy(parkingGroupId2)
    doReturn(Future(1)).when(dbLayer).getMaxVersionById(policyId2,productionApi.value)
    doReturn(futureSuccessMessage).when(dbLayer).updateStateOfPolicy(policyId2, 1, Some("active"))

    val chain = for {
      appResponse <- groupPcHelper.associatingPolicyToGroup(policyId2, parkingGroupId2, policyAssociationAppRequestValidPolicyLink2)
    } yield appResponse


    whenReady(chain, timeout(Span(5, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[ParkingGroupPolicyLink]].messageid.equals(messageId2))
    }
  }

  /* Method: generateGroupPolicyObject */
  "generateGroupPolicyObject" should "return SUCCESS with populated and default \"ParkingGroupPolicyLink\" data" in {

    val pkgGpPolicy = groupPcHelper.generateGroupPolicyObject(policyId, parkingGroupId)

    assert(pkgGpPolicy.asInstanceOf[ParkingGroupPolicyLink].policyid.equals(policyId))

  }

  /* Method: updateStateOfPolicy */
  "updateStateOfPolicy" should "return Failure Response \"Internal server error while associating parking group to policy\" " in {

    val chain = for {
      appResponse <- groupPcHelper.updateStateOfPolicy(parkingGroupPolicyLink, policyAssociationAppRequestValidPolicyLink)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  "updateStateOfPolicy" should "return Success " in {

    doReturn(Future(1)).when(dbLayer).getMaxVersionById(policyId1,productionApi.value)
    doReturn(futureSuccessMessage).when(dbLayer).updateStateOfPolicy(policyId1, 1, Some("active"))

    val chain = for {
      appResponse <- groupPcHelper.updateStateOfPolicy(parkingGroupPolicyLink1, policyAssociationAppRequestValidPolicyLink1)
    } yield appResponse

    whenReady(chain, timeout(Span(5, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[ParkingGroupPolicyLink]].messageid.equals(messageId1))
    }
  }


  /* Method: getMaxVersionAndUpdateRecord */
  "getMaxVersionAndUpdateRecord" should "return Success " in {

    val uid = UUID.randomUUID().toString
    val messageId = UUID.randomUUID().toString
    val policyId = UUID.randomUUID().toString
    val parkingGroupId = UUID.randomUUID().toString

    val version = 10

    val someParkingGroupInputList = Some(new ParkingGroupPolicyLinkRequest(parkingGroupId))

    val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid, parkingGroupId, policyId, version, Calendar.getInstance().getTimeInMillis, -1)

    val policyAppRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None,
        None, someParkingGroupInputList, None, None), AppUserDataProps(None, "", "", None)))

    doReturn(Future(Option(parkingGroupPolicyLink))).when(dbLayer).getGroupPolicyByGroupId(parkingGroupId)
    doReturn(futureSuccessMessage).when(dbLayer).updateGroupPolicyLink(uid)
    doReturn(futureSuccessMessage).when(dbLayer).getAllActicveGroupsByPolicyId(policyId, version)

    val chain = for {
      appResponse <- groupPcHelper.getMaxVersionAndUpdateRecord(policyId, parkingGroupId, version, policyAppRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[ParkingGroupPolicyLink]].messageid.equals(messageId))
    }
  }

  "getMaxVersionAndUpdateRecord" should "return Failure " in {

    val uid = UUID.randomUUID().toString
    val messageId = UUID.randomUUID().toString
    val policyId = UUID.randomUUID().toString
    val parkingGroupId = UUID.randomUUID().toString

    val version = 10

    val someParkingGroupInputList = Some(new ParkingGroupPolicyLinkRequest(parkingGroupId))

    val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid, parkingGroupId, policyId, version, Calendar.getInstance().getTimeInMillis, -1)

    val policyAppRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None,
        None, someParkingGroupInputList, None, None), AppUserDataProps(None, "", "", None)))

    doReturn(Future(Option(parkingGroupPolicyLink))).when(dbLayer).getGroupPolicyByGroupId(parkingGroupId)
    doReturn(futureFailureMessage).when(dbLayer).updateGroupPolicyLink(uid)

    val chain = for {
      appResponse <- groupPcHelper.getMaxVersionAndUpdateRecord(policyId, parkingGroupId, version, policyAppRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }


  /* Method: checkPolicyAssociatedtoAnyGroup */
  "checkPolicyAssociatedtoAnyGroup" should "return Success " in {

    val uid = UUID.randomUUID().toString
    val messageId = UUID.randomUUID().toString
    val policyId = UUID.randomUUID().toString
    val parkingGroupId = UUID.randomUUID().toString

    val version = 10

    val someParkingGroupInputList = Some(new ParkingGroupPolicyLinkRequest(parkingGroupId))

    val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid, parkingGroupId, policyId, version, Calendar.getInstance().getTimeInMillis, -1)

    val policyAppRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None,
        None, someParkingGroupInputList, None, None), AppUserDataProps(None, "", "", None)))

    doReturn(futureSuccessMessage).when(dbLayer).getAllActicveGroupsByPolicyId(policyId, version)

    val chain = for {
      appResponse <- groupPcHelper.checkPolicyAssociatedtoAnyGroup(policyId, version, policyAppRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(5, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[ParkingGroupPolicyLink]].messageid.equals(messageId))
    }
  }

  "checkPolicyAssociatedtoAnyGroup" should "return Failure " in {

    val uid = UUID.randomUUID().toString
    val messageId = UUID.randomUUID().toString
    val policyId = UUID.randomUUID().toString
    val parkingGroupId = UUID.randomUUID().toString

    val version = 10

    val someParkingGroupInputList = Some(new ParkingGroupPolicyLinkRequest(parkingGroupId))

    val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid, parkingGroupId, policyId, version, Calendar.getInstance().getTimeInMillis, -1)

    val policyAppRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None,
        None, someParkingGroupInputList, None, None), AppUserDataProps(None, "", "", None)))

    doReturn(Future(Option(parkingGroupPolicyLink))).when(dbLayer).getGroupPolicyByGroupId(parkingGroupId)
    doReturn(futureSuccessMessage).when(dbLayer).updateGroupPolicyLink(uid)

    val chain = for {
      appResponse <- groupPcHelper.checkPolicyAssociatedtoAnyGroup(policyId, version, policyAppRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }


  /* Method: getAssociatedParkingGroupsOfActivePolicy */
  "getAssociatedParkingGroupsOfActivePolicy" should "return Success " in {

    val uid = UUID.randomUUID().toString
    val messageId = UUID.randomUUID().toString
    val policyId = UUID.randomUUID().toString
    val parkingGroupId = UUID.randomUUID().toString

    val version = 10

    val policyAppRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None,
        None, someParkingGroupInputList, None, None), AppUserDataProps(None, "", "", None)))

    val parkingGroupInputList = ParkingGroupInputList(parkingGroupId, 14986863999977L, 14986963999977L, version)

    doReturn(Future(List(policy))).when(dbLayer).getAllLivePoliciesById(policyId, orgid, siteid,productionApi.value)
    doReturn(Future(List(parkingGroupInputList))).when(dbLayer).getAssociatedParkingGroupsOfPolicyId(policyId)

    val chain = for {
      appResponse <- groupPcHelper.getAssociatedParkingGroupsOfActivePolicy(orgid, siteid, policyAppRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(5, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[ParkingGroupPolicyLink]].messageid.equals(messageId))
    }
  }

  "getAssociatedParkingGroupsOfActivePolicy" should "return Failure " in {

    val uid = UUID.randomUUID().toString
    val messageId = UUID.randomUUID().toString
    val policyId = UUID.randomUUID().toString
    val parkingGroupId = UUID.randomUUID().toString

    val version = 10

    val policyAppRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None,
        None, someParkingGroupInputList, None, None), AppUserDataProps(None, "", "", None)))

    doReturn(Future(List(policy))).when(dbLayer).getAllLivePoliciesById(policyId, orgid, siteid,productionApi.value)

    val chain = for {
      appResponse <- groupPcHelper.checkPolicyAssociatedtoAnyGroup(policyId, version, policyAppRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }


  /* Method: groupPolicyDisassociationProcess */
  "groupPolicyDisassociationProcess" should "return Success " in {

    val uid = UUID.randomUUID().toString
    val messageId = UUID.randomUUID().toString
    val policyId = UUID.randomUUID().toString
    val parkingGroupId = UUID.randomUUID().toString

    val version = 10

    val someParkingGroupInputList = Some(new ParkingGroupPolicyLinkRequest(parkingGroupId))

    val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid, parkingGroupId, policyId, version, Calendar.getInstance().getTimeInMillis, -1)

    val policyAppRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyassociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None,
        None, someParkingGroupInputList, None, None), AppUserDataProps(None, "", "", None)))

    doReturn(Future(version)).when(dbLayer).getMaxVersionById(policyId,productionApi.value)
    doReturn(Future(List(parkingGroupPolicyLink))).when(dbLayer).checkGroupAssociatedWithPolicy(parkingGroupPolicyLink.parkinggroupid, policyId)
    doReturn(Future(Option(parkingGroupPolicyLink))).when(dbLayer).getGroupPolicyByGroupId(parkingGroupId)
    doReturn(futureSuccessMessage).when(dbLayer).updateGroupPolicyLink(uid)
    doReturn(futureSuccessMessage).when(dbLayer).getAllActicveGroupsByPolicyId(policyId, version)
    doReturn(Future(List(policy))).when(dbLayer).getPolicyByIdAndVersion(policyId, version, orgid, siteid,productionApi.value)

    val chain = for {
      appResponse <- groupPcHelper.groupPolicyDisassociationProcess(orgid, siteid, policyAppRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[ParkingGroupPolicyLink]].messageid.equals(messageId))
    }
  }

  "groupPolicyDisassociationProcess" should "return Failure Response \"Required policyid missing in request\" " in {

    val chain = for {
      appResponse <- groupPcHelper.groupPolicyDisassociationProcess(orgid, siteid, policyAssociationAppRequestNoPolicyID)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  "groupPolicyDisassociationProcess" should "return Failure Response \"Required groupLink object missing in request\" " in {

    val chain = for {
      appResponse <- groupPcHelper.groupPolicyDisassociationProcess(orgid, siteid, policyAssociationAppRequestNoPolicyLink)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  "groupPolicyDisassociationProcess" should "return error - \"Internal server error while updating the state of policy to inactive\" " in {

    doReturn(Future(1)).when(dbLayer).getMaxVersionById(policyId3,productionApi.value)
    doReturn(Future(List(parkingGroupPolicyLink3))).when(dbLayer).checkGroupAssociatedWithPolicy(parkingGroupId3, policyId3)
    doReturn(Future(Option(parkingGroupPolicyLink3))).when(dbLayer).getGroupPolicyByGroupId(parkingGroupId3)
    doReturn(futureSuccessMessage).when(dbLayer).updateGroupPolicyLink(uid3)

    val chain = for {
      appResponse <- groupPcHelper.groupPolicyDisassociationProcess(orgid, siteid, policyAssociationAppRequestValidPolicyLink3)
    } yield appResponse


    whenReady(chain, timeout(Span(5, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId3))
    }
  }


  val endTimeEpochDefault = -1

  val startTime = "2444-11-30T01:46:39.977Z[UTC]"
  val endTime = "2444-11-30T01:46:39.977Z[UTC]"

  val ParkingGroupInputList1 = ParkingGroupInputList(parkingGroupId1, 123456, endTimeEpochDefault, 10)
  val ParkingGroupInputList2 = ParkingGroupInputList(parkingGroupId2, 879875, endTimeEpochDefault, 10)
  val parkingIpList = List(ParkingGroupInputList1, ParkingGroupInputList2)

  val parkingGroupIP = Future.successful(parkingIpList)
  val parkingGroupOP = Future.successful(List(ParkingGroupOutputList(parkingGroupId, startTime, endTime, 10)))


  /* Method: getAssignedParkingGroupsOfLivePolicy */
  "getAssignedParkingGroupsOfLivePolicy" should "Return SUCCESS Response" in {

    doReturn(parkingGroupIP).when(dbLayer).getAssociatedParkingGroupsOfPolicyId(policyId)

    val chain = for {
      appResponse <- groupPcHelper.getAssignedParkingGroupsOfLivePolicy(policyAssociationAppRequest, policyId)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[ParkingGroupOutputList]].messageid.equals(messageId))
    }
  }

  /* Method: convertToUTCDateTimeAtParkingGroup
     If the policy is still active, end Time is returned as empty string */
  "convertToUTCDateTimeAtParkingGroup" should "Contain -1 for endTime, if Policy is active" in {

    val endTimeCheck = groupPcHelper.convertToUTCDateTimeAtParkingGroup(parkingIpList)

    forAll(endTimeCheck) { a =>
      assert(a.endTime contentEquals (""))
    }
  }


}
