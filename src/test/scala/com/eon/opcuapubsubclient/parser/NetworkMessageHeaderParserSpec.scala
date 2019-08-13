package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.UnitSpec
import com.eon.opcuapubsubclient.UnitSpec.StringAsByteVector
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.NetworkMessageTypes.DiscoveryResponseType
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.startParsePosition
import org.scalatest.BeforeAndAfterAll


class NetworkMessageHeaderParserSpec extends UnitSpec with BeforeAndAfterAll {

  private val byteVector = TestData.networkMsgHeader.asByteVector

  "it" should "parse NetworkMessageHeader successfully" in {
    val Right((actualNetworkMsgHeader, pos)) = NetworkMessageHeaderParser(byteVector)(startParsePosition)

    // assert
    assert(actualNetworkMsgHeader.version == 1, s"Expected Version is 1, but Actual Version is ${actualNetworkMsgHeader.version }")
    assert(actualNetworkMsgHeader.publisherIdEnabled, s"publisherId should be enabled, but it is not!")
    assert(!actualNetworkMsgHeader.groupHeaderEnabled, s"groupHeader should be disabled, but it is not!")
    assert(!actualNetworkMsgHeader.payloadHeaderEnabled, s"payloadHeader should be disabled, but it is not!")
    assert(actualNetworkMsgHeader.extendedFlags1Enabled, s"extendedFlags1Enabled should be enabled, but it is not!")

    val extendedFlags1 = actualNetworkMsgHeader.extendedFlags1
    assert(!extendedFlags1.dataSetClassIDEnabled, s"dataSetClassIDEnabled should be disabled, but it is not!")
    assert(!extendedFlags1.securityEnabled, s"securityEnabled should be disabled, but it is not!")
    assert(!extendedFlags1.timeStampEnabled, s"timeStampEnabled should be disabled, but it is not!")
    assert(extendedFlags1.extendedFlags2Enabled, s"extendedFlags2Enabled should be enabled, but it is not!")

    val extendedFlags2 = actualNetworkMsgHeader.extendedFlags2
    assert(!extendedFlags2.isChunkMessage, s"isChunkMessage should be false, but it is not!")
    assert(!extendedFlags2.promotedFieldsEnabled, s"promotedFieldsEnabled should be false, but it is not!")
    assert(extendedFlags2.networkMessageType == DiscoveryResponseType, s"networkMessageType should be DiscoveryResponseType, " +
      s"but it ${extendedFlags2.networkMessageType}!")

    assert(actualNetworkMsgHeader.publisherId.contains("A8000_CP802x_essen_lab_CP_8021_2_GF1818181818"))
    assert(actualNetworkMsgHeader.dataSetClassId.isEmpty)

    //case class Message(version: Int, publisherIdEnabled: Boolean, groupHeaderEnabled: Boolean, payloadHeaderEnabled: Boolean, extendedFlags1Enabled: Boolean)
    //val codec = (uint4 :: bool :: bool :: bool :: bool).as[Message]

    // Assert that we have consumed all the bytes from the input
    assert(pos == byteVector.length)
    println(actualNetworkMsgHeader)
    println(pos)
    println(byteVector.length)
  }
}
