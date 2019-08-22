package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.UnitSpec
import com.eon.opcuapubsubclient.UnitSpec._
import com.eon.opcuapubsubclient.domain.CommonTypes.StatusCode
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.{ParsePosition => P}
import scodec.bits.{ByteVector => B}


class ParserUtilsSpec extends UnitSpec {

  type S = String

  private val byteVector = "4 0 0 0 65 56 48 48 77 88 2 1".asByteVector

  "it" should "do something" in {

    def unTuple[A](fn: (B, P) => (A, P)): A = {
      val (res, _) = fn(byteVector, initParsePosition)
      res
    }

    val resultMessage = ">> parsed result was not what was expected for parser function ParserUtils."
    val positionMessage = ">> position in the ByteVector for the parsed result was not what was expected for parser function ParserUtils."

    def test[A](fn: (B, P) => (A, P))(expectedResult: A, expectedPos: P)(resultMsg: S, positionMsg: S) = {
      val (res, pos) = fn(byteVector, initParsePosition)
      assert(res == expectedResult,resultMsg)
      assert(pos == expectedPos, positionMsg)
    }

    test(ParserUtils.parseBoolean)(true,1)(s"${resultMessage}parseBoolean",s"${positionMessage}parseBoolean")

    test(ParserUtils.parseByte)(4,1)(s"${resultMessage}parseByte",s"${positionMessage}parseByte")
    test(ParserUtils.parseUByte)(4,1)(s"${resultMessage}parseUByte",s"${positionMessage}parseUByte")
    test(ParserUtils.parseByteAsInt)(4,1)(s"${resultMessage}parseByteAsInt",s"${positionMessage}parseByteAsInt")

    test(ParserUtils.parseInt16)(4,2)(s"${resultMessage}parseInt16",s"${positionMessage}parseInt16")
    test(ParserUtils.parseUInt16)(4,2)(s"${resultMessage}parseUInt16",s"${positionMessage}parseUInt16")
    test(ParserUtils.parseInt32)(4,4)(s"${resultMessage}parseInt32",s"${positionMessage}parseInt32")
    test(ParserUtils.parseUInt32)(4,4)(s"${resultMessage}parseUInt32",s"${positionMessage}parseUInt32")

    test(ParserUtils.parseUInt64)(3472337164526682116L, 8)(s"${resultMessage}parseUInt64",s"${positionMessage}parseUInt64")

    test(ParserUtils.parseString)("A800",8)(s"${resultMessage}parseString",s"${positionMessage}parseString")
    test(ParserUtils.parseStatusCode)( StatusCode(4),4)(s"${resultMessage}parseStatusCode",s"${positionMessage}parseStatusCode")

    // TODO: Add additional tests for the remaining parsers!

  }
}
