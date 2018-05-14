package com.vz.ns.ts.service.model

import com.fasterxml.jackson.annotation.JsonProperty
import com.outworkers.phantom.dsl.DateTime

case class Mapping(@JsonProperty nodeId: String, @JsonProperty tsDeviceId: String, @JsonProperty createdOn: DateTime)
