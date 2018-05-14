package com.vz.nsp.parking.model
import com.vz.nsp.parking.CommonHelper._
import com.vz.nsp.parking.model.ValidationMessages._

case class Tag(uid: String,
               name: String,
               description: String,
               orgid: String,
               siteid: String,
               createdon: Long,
               lastupdated: Long,
               isdeleted: Boolean)

case class TagResponse(tagId: String,
                       name: String,
                       description: String,
                       orgId: String,
                       siteId: String,
                       createdOn: String,
                       lastUpdated: String,
                       isDeleted: Option[Boolean]=None)

case class TagRequest(uid: Option[String], name: String, description: Option[String]) {
  require(!isEmpty(name), RequiredFieldMissing.value + "name")
}
