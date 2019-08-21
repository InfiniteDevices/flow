package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.UnitSpec.StringAsByteVector
import com.eon.opcuapubsubclient.UnitSpec
import com.eon.opcuapubsubclient.domain.HeaderTypes.NetworkMessageTypes.DiscoveryResponseType
import com.eon.opcuapubsubclient.domain.HeaderTypes._
import com.eon.opcuapubsubclient.domain.HeaderTypes.PayloadHeader.DiscoveryResponseMessagePayloadHeader
import com.eon.opcuapubsubclient.domain.PayloadTypes.DiscoveryResponseMessageTypes
import org.scalatest.BeforeAndAfterAll


class PayloadHeaderParserSpec extends UnitSpec with BeforeAndAfterAll {

  "it" should "parse PayloadHeader successfully" in {
    val byteVector = TestData.dataSetMetaDataPayloadHeader.asByteVector

    val networkMsgType = DiscoveryResponseType

    val Right((payloadHeader, pos2)) = PayloadHeaderParser(byteVector)(networkMsgType)(initParsePosition)

    // Test the assertions
    payloadHeader match {
      case discoveryResponse: DiscoveryResponseMessagePayloadHeader =>
        assert(discoveryResponse.responseType == DiscoveryResponseMessageTypes.DataSetMetaData)
        assert(discoveryResponse.sequenceNumber == 19)

        // We should have finished all the bytes
        assert(pos2 == byteVector.size)
      case x: PayloadHeader =>
        fail(s"Expected a DiscoveryResponseMessageType of DataSetMetaData, but got ${x.getClass.getCanonicalName}")
    }
  }
}
