package com.vz.ns.ts.service.rest

import com.vz.ns.ts.service.util.BaseSpec

/**
 * Created by dasarst on 5/10/17.
 */
class HttpServicesTest extends BaseSpec {

  it should "Create the route" in {

    assert(HttpServices.devProvRoute != null)
    HttpServices.routes mustNot equal(null)
  }

}
