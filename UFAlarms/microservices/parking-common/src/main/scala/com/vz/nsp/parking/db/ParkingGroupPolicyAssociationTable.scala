package com.vz.nsp.parking.db

import java.util.Calendar

import com.outworkers.phantom.dsl.{ConsistencyLevel, _}
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.parking.config.CassandraConfig
import com.vz.nsp.parking.model.{ParkingGroupInputList, ParkingGroupPolicyLink, ParkingGroupPolicyLinkRequest, Policy}

import scala.concurrent.Future

abstract class ParkingGroupPolicyLinkTable extends Table[ParkingGroupPolicyLinkTable, ParkingGroupPolicyLink] {

  override def tableName: String = CassandraConfig.parkingGroupPolicyTableName

  object uid extends StringColumn with PartitionKey

  object parkinggroupid extends StringColumn

  object policyid extends StringColumn

  object parkinggroupname extends StringColumn

  object version extends IntColumn

  object starttime extends LongColumn

  object endtime extends LongColumn

}

abstract class ConcreteGroupPolicy extends ParkingGroupPolicyLinkTable with RootConnector {

  def associateGroupPolicy(groupPolicy: ParkingGroupPolicyLink, version: Int): Future[ResultSet] =
    insert
      .value(_.uid, groupPolicy.uid)
      .value(_.parkinggroupid, groupPolicy.parkinggroupid)
      .value(_.policyid, groupPolicy.policyid)
      .value(_.version, version)
      .value(_.starttime, groupPolicy.startTime)
      .value(_.endtime, groupPolicy.endTime)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def checkGroupLinkedWithPolicy(groupid: String): Future[List[ParkingGroupPolicyLink]] =
    select
      .where(_.parkinggroupid is groupid)
      .and(_.endtime is -1)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def checkGroupAssociatedWithPolicy(groupid: String, policyid: String): Future[List[ParkingGroupPolicyLink]] =
    select
      .where(_.parkinggroupid is groupid)
      .and(_.policyid is policyid)
      .and(_.endtime is -1)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def getPolicyByIdInTimeline(policyid: String): Future[List[ParkingGroupPolicyLink]] =
    select.where(_.policyid is policyid).consistencyLevel_=(ConsistencyLevel.ONE).allowFiltering().fetch()

  def getGroupPolicyByGroupId(groupid: String): Future[Option[ParkingGroupPolicyLink]] =
    select
      .where(_.parkinggroupid is groupid)
      .and(_.endtime is -1)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .one()

  def updateGroupPolicyLink(uid: String): Future[ResultSet] =
    update
      .where(_.uid eqs uid)
      .modify(_.endtime setTo Calendar.getInstance().getTimeInMillis)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def getAllPoliciesWithGroupId(parkinggroupid: String): Future[List[ParkingGroupPolicyLink]] =
    select.where(_.parkinggroupid is parkinggroupid).consistencyLevel_=(ConsistencyLevel.ONE).allowFiltering().fetch()

  def getAllActicveGroupsByPolicyId(policyid: String): Future[List[ParkingGroupPolicyLink]] =
    select
      .where(_.policyid is policyid)
      .and(_.endtime is -1)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def getAllAssignedParkingGroupsForPolicyId(policyId: String): Future[List[ParkingGroupInputList]] = {
    val data = select(_.parkinggroupid, _.starttime, _.endtime, _.version)
      .where(_.policyid is policyId)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

    data.map(lst => {
      lst.map(_ match {
        case (gpId: String, stTime: Long, edTime: Long, version: Int) =>
          ParkingGroupInputList(gpId, stTime, edTime, version)
      })
    })
  }

}
