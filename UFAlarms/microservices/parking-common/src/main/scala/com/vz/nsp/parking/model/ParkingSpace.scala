package com.vz.nsp.parking.model

import com.outworkers.phantom.udt.Udt

@Udt
case class GeoCoordinate(latitude: Double, longitude: Double)

@Udt
case class SpaceGeometry(p1: GeoCoordinate, p2: GeoCoordinate, p3: GeoCoordinate, p4: GeoCoordinate)

case class ParkingSpace(parkingspaceid: String,
                        name: String,
                        typeOfVehicle: List[String],
                        reservation: Boolean,
                        handicap: Boolean,
                        monitoringSensorid: String,
                        businessUse: String,
                        howMetered: String,
                        active: Boolean,
                        PPV: Boolean,
                        areaType: List[String],
                        meterid: String,
                        paystationid: String,
                        level: String,
                        parkingSpaceType: String,
                        geoCoordinates: Option[SpaceGeometry],
                        createdOn: Long,
                        lastUpdated: Long,
                        isDeleted: Boolean)

case class ParkingSpaceRequest(parkingspaceid: String,
                               name: Option[String],
                               typeOfVehicle: Option[List[String]],
                               reservation: Option[Boolean],
                               handicap: Option[Boolean],
                               monitoringSensorid: Option[String],
                               businessUse: Option[String],
                               howMetered: Option[String],
                               active: Option[Boolean],
                               PPV: Option[Boolean],
                               areaType: Option[List[String]],
                               meterid: Option[String],
                               paystationid: Option[String],
                               level: Option[String],
                               parkingSpaceType: Option[String],
                               geoCoordinates: Option[SpaceGeometry])

case class SuccessMessageWithIds(success: Boolean, invalidParkingspaceids: Option[List[String]] = None)
case class SuccessMessageWithSpaceAttributes(success: Boolean, spaceAttributes: Option[String] = None)
case class SuccessMessageWithInvalidSpaces(success: Boolean,
                                           spaceAttributesNotUpdatedForParkingspaceids: Option[List[String]] = None)

case class ParkingSpaceResponse(parkingspaceid: String,
                                name: String,
                                typeOfVehicle: List[String],
                                reservation: Boolean,
                                handicap: Boolean,
                                monitoringSensorid: String,
                                businessUse: String,
                                howMetered: String,
                                active: Boolean,
                                PPV: Boolean,
                                areaType: List[String],
                                meterid: String,
                                paystationid: String,
                                level: String,
                                parkingSpaceType: String,
                                geoCoordinates: Option[SpaceGeometry],
                                createdOn: String,
                                lastUpdated: String)
