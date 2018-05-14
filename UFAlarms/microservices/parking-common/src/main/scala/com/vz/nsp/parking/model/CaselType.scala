package com.vz.nsp.parking.model

sealed case class CaselType(value: String)

object CaselType {

  object getParkingTag extends CaselType("getPolicyCategory")

  object getAllParkingTags extends CaselType("getAllPolicyCategory")

  object postParkingTag extends CaselType("createPolicyCategory")

  object deleteParkingTag extends CaselType("deletePolicyCategory")

  object updateParkingTag extends CaselType("updatePolicyCategory")

  object postParkingPolicy extends CaselType("createParkingPolicy")

  object getAllParkingPolicies extends CaselType("getAllParkingPolicy")

  object getParkingPolicy extends CaselType("getParkingPolicy")

  object deleteParkingPolicy extends CaselType("deleteParkingPolicy")

  object updateParkingPolicy extends CaselType("updateParkingPolicy")

  object policyassociation extends CaselType("policyAssociation")

  object policydisassociation extends CaselType("policyDisassociation")

  object getAssociatedParkingGroups extends CaselType("associatedParkingGroups")

  object getPolicyVersionHistory extends CaselType("getAllVersionsOfParkingPolicy")

  object getPolicyByVersionNumber extends CaselType("getParkingPolicyVersion")

  object postToSearchPolicy extends CaselType("searchParkingPolicy")

  object getActivePoliciesWithInTimeline extends CaselType("getAllActiveParkingPolicyForPeriod")

  object getActivePolicy extends CaselType("getActiveParkingPolicy")

  object validationfailed extends CaselType("validationfailed")

  object getParkingSpace extends CaselType("getMetadataForParkingSpot")

  object getAllParkingSpaces extends CaselType("getAllMetadataForParkingSpot")

  object postParkingSpace extends CaselType("createMetadataForParkingSpot")

  object deleteParkingSpace extends CaselType("deleteMetadataForParkingSpot")

  object updateParkingSpace extends CaselType("updateMetadataForParkingSpot")

  object policyTagsAssociation extends CaselType("policyTagsAssociation")

  object policyTagsDisassociation extends CaselType("policyTagsDisassociation")

  object postUserData extends CaselType("createAppUserData")

  object getAllUserData extends CaselType("getAllAppUserData")

  object getUserData extends CaselType("getAppUserData")

  object deleteUserData extends CaselType("deleteAppUserData")

  object updateUserData extends CaselType("updateAppUserData")

  object activeValue extends CaselType("active")

  object inactiveValue extends CaselType("inactive")

  object unassignedValue extends CaselType("unassigned")

  object getAllWhatIfTags extends CaselType("getAllWhatIfTags")

  object getWhatIfTag extends CaselType("getWhatIfTag")

  object createWhatIfTag extends CaselType("createWhatIfTag")

  object updateWhatIfTag extends CaselType("updateWhatIfTag")

  object deleteWhatIfTag extends CaselType("deleteWhatIfTag")

  object syntheticApi extends CaselType("synthetic")

  object productionApi extends CaselType("production")

  object createWhatIfPolicy extends CaselType("createWhatIfPolicy")

  object getAllWhatIfPolicies extends CaselType("getAllWhatIfPolicies")

  object getWhatIfPolicy extends CaselType("getWhatIfPolicy")

  object updateWhatIfPolicy extends CaselType("updateWhatIfPolicy")

  object deleteWhatIfPolicy extends CaselType("deleteWhatIfPolicy")

  object whatIfPolicyTagsAssociation extends CaselType("whatIfPolicyTagsAssociation")

  object whatIfPolicyTagsDisassociation extends CaselType("whatIfPolicyTagsDisassociation")


}
