package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.UnitSpec
import org.scalatest.BeforeAndAfterAll


class DataTypeSchemaHeaderParserSpec extends UnitSpec with BeforeAndAfterAll {

  // Construction of the test data
  val testData: String = {
    val structureDescArrLen = "5 0 0 0"
    val dataTypeId = "1 1 1 0"
    val nsIndex = "0 0"
    val strLengthAndName = "8 0 0 0 83 80 83 86 97 108 117 101"
    structureDescArrLen + dataTypeId + nsIndex + strLengthAndName
  }
}
