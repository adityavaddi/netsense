package com.vz.nsp.parking.model

import com.vz.nsp.parking.CommonHelper._
import com.vz.nsp.parking.model.ValidationMessages.RequiredFieldMissing

/**
  * Created by thangth on 8/22/17.
  */

  case class TagsPolicyLinkRequest(tagids: Set[String])
