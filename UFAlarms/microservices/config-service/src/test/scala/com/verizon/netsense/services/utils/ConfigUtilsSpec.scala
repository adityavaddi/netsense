package com.verizon.netsense.services.utils

import com.verizon.netsense.utils.ConfigUtils
import org.scalatest.{FlatSpec, Matchers}
import org.json4s.jackson.JsonMethods.{compact, parse}
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.Serialization.{read, write}

import scala.util.matching.Regex

/**
 * Created by refsdbr on 8/3/17.
 */
class ConfigUtilsSpec extends FlatSpec with Matchers {

  implicit val formats = DefaultFormats

  "ConfigUtils" should "validate config regex" in {

    val path = "global/GET/REPLY/6776cd16-87c0-4fb2-a9af-5c2274d346ae/config/list"

    val replyConfigPattern: Regex              = """.*(GET|PUT)\/REPLY\/([0-9a-zA-Z-]+)\/config\/(list|single)""".r
    val replyConfigPattern(verb, uuid, flavor) = path

    verb should be("GET")
    uuid should be("6776cd16-87c0-4fb2-a9af-5c2274d346ae")
    flavor should be("list")
  }

  "JSON" should "be sorted" in {
    val result = """[{"a":1},{"b":2},{"bb":3},{"c":4}]"""

    val map = ("bb" -> 3) ~ ("a" -> 1) ~ ("c" -> 4) ~ ("b" -> 2)

    val sorted = map.values.toSeq.sortBy(_._1)

    write(sorted) should be(result)
  }

  "CRC" should "be equal for differently sorted configs" in {
    val cfg1 = Map("server" -> "nsn.sensity.com", "debugmode" -> true)
    val cfg2 = Map("debugmode" -> true, "server" -> "nsn.sensity.com")

    val crc1 = ConfigUtils.getToken(cfg1)
    val crc2 = ConfigUtils.getToken(cfg2)

    crc1 shouldBe(crc2)

  }
}
