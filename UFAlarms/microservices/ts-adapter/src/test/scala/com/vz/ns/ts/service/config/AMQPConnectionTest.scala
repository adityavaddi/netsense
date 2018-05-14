package com.vz.ns.ts.service.config

import com.vz.ns.ts.service.util.BaseSpec

class AMQPConnectionTest extends BaseSpec {

  val amqpCon = AMQPConnection;

  it should "Create AMQP Connection Factory" in {
    //doNothing().when(amqpCon.getConnection)
    assert(amqpCon.factory != null)
  }

  ignore should "Create AMQP Connection" in {
    assert(amqpCon.getConnection != null)
  }
}
