package com.verizon.netsense.util

import java.util.UUID

import com.verizon.netsense.constants.Constants
import com.verizon.netsense.helper.BaseSpec

/**
  * Created by maidapr on 4/20/18.
  */
class TokenUtilTest extends BaseSpec with TokenUtil {

  def generateRandomUUID = UUID.randomUUID().toString
  def currentMillis = System.currentTimeMillis()

  "TokenUtil" should "validate the schedule token" in new TokenUtil  {
    val timeForTest = currentMillis
    val idForTest   = generateRandomUUID
    override def timeMillis = timeForTest

    val scheduleTokenForTest = convertToScheduleKeyValuePair(idForTest, timeForTest)
    val tokenValue: Option[String] = extractScheduleTokenValueFromToken(scheduleTokenForTest)

    tokenValue match {
      case Some(token) =>
        assert(token.contains(convertScheduleIdToToken(idForTest, timeForTest)))
        extractScheduleIdFromToken(token) match {
          case Some(scheduleId) =>
            assert(scheduleId.contains(idForTest))
          case b =>
            assert(false, s"token value: $scheduleTokenForTest parsed from token is not valid")
        }
      case _ => assert(false, s"$scheduleTokenForTest is invalid")
    }
  }

  it should "validate token with invalid epoch millis" in new TokenUtil  {
    val timeForTest = currentMillis
    val idForTest   = generateRandomUUID
    override def timeMillis = timeForTest
    val tokenWithInvalidEpoch = convertToScheduleKeyValuePair(idForTest, timeForTest / 1000)
    val tokenValue: Option[String] = extractScheduleTokenValueFromToken(tokenWithInvalidEpoch)
    tokenValue match {
      case Some(token) =>
        assert(false, s"function failed to validate invalid epoch millis $tokenWithInvalidEpoch")
      case _ => assert(true)
    }

  }

  it should "validate token with invalid prefix" in {

    val invalidPrefix = s"asdfgasd" +
      s"${Constants.SCHEDULE_TOKEN_DELIMITER_STR}" +
      s"$timeMillis" +
      s"${Constants.SCHEDULE_TOKEN_VALUE_DELIMITER_STR}"

    val tokenTestForInvalidPrefix = extractScheduleIdFromToken(invalidPrefix)
    assert(tokenTestForInvalidPrefix.isEmpty, "function failed to validate prefix")

  }

  it should "validate token with invalid delimiter" in {

    val invalidDelimitedToken = s"${Constants.SCHEDULE_TOKEN_PREFIX_STR}" +
      s"&" +
      s"$timeMillis" +
      s"${Constants.SCHEDULE_TOKEN_VALUE_DELIMITER_STR}"

    val tokenTestForInvalidDemiliter = extractScheduleIdFromToken(invalidDelimitedToken)
    assert(tokenTestForInvalidDemiliter.isEmpty, "function failed to validate delimiter")
  }


  it should "split the scheduleId and scheduleTime from token" in new TokenUtil {

    val timeForTest = currentMillis
    val idForTest = generateRandomUUID

    override def timeMillis = timeForTest

    val scheduleTokenForTest = convertScheduleIdToToken(idForTest, timeForTest)

    private val scheduleIdTime: (Option[Long], Option[String]) = extractScheduleIdTime(Some(scheduleTokenForTest))
    assert(scheduleIdTime._1.isDefined && scheduleIdTime._1.get === timeForTest, "Invalid ScheduleTime from token")
    assert(scheduleIdTime._2.isDefined && scheduleIdTime._2.get === idForTest, "Invalid ScheduleId from token")
  }

  it should "validate the token array with valid tokens" in new TokenUtil {

    val timeForTest = currentMillis
    val idForTest   = generateRandomUUID
    override def timeMillis = timeForTest


    val scheduleTokenForTest = convertToScheduleKeyValuePair(idForTest, timeForTest)

    val invalidDelimitedToken = s"${Constants.SCHEDULE_TOKEN_PREFIX_STR}" +
      s"&" +
      s"$timeMillis" +
      s"${Constants.SCHEDULE_TOKEN_VALUE_DELIMITER_STR}"

    val tokenWithInvalidEpoch = convertToScheduleKeyValuePair(idForTest, timeForTest / 1000)

    val tokenArray = Vector(scheduleTokenForTest, invalidDelimitedToken, tokenWithInvalidEpoch)

    val tokenValue: Option[String] = getScheduleTokenFromTokenArray(tokenArray)
    assert(tokenValue.isDefined, s"function failed to parse valid token $scheduleTokenForTest")
    tokenValue match {
      case Some(tokenV) =>
        assert(tokenV == convertScheduleIdToToken(idForTest, timeForTest))
      case _ => assert(false, "failed to extract valid token value")
    }

  }

}
