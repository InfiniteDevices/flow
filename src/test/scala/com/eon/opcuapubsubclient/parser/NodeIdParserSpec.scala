package com.eon.opcuapubsubclient.parser

import java.util.UUID

import com.eon.opcuapubsubclient.UnitSpec
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.NodeIdIdentifier.{GuidIdentifier, NumericFourByteIdentifier, NumericTwoByteIdentifier, OpaqueIdentifier, StringIdentifier, UnknownIdentifier}
import org.scalatest.BeforeAndAfterAll
import scodec.bits.ByteVector

// TODO: Write additional tests for other types
class NodeIdParserSpec extends UnitSpec with BeforeAndAfterAll {

  //val numericNodeIdByteSeq: Array[Byte]  = toByteArray("")
  //val guidNodeIdByteSeq  : Array[Byte]   = toByteArray("")
  //val opaqueNodeIdByteSeq: Array[Byte]   = toByteArray("")

  "NodeIdParser # parseTwoByteNodeId" should "parse a 2 byte NodeId successfully" in {
    val testData1 = toByteArray("00 72")
    val (nodeId1, pos1) = NodeIdParser(ByteVector(testData1))(initParsePosition)
    assert(nodeId1.namespaceIndex == 0, s"namespaceIndex should be 0, but we got ${nodeId1.namespaceIndex}")
    assert(nodeId1.identifier == NumericTwoByteIdentifier(72))
    assert(pos1 == testData1.length)
  }

  "NodeIdParser # parseFourByteNodeId" should "parse a 4 byte NodeId successfully" in {
    val testData1 = toByteArray("1 1 1 0")
    val (nodeId1, pos1) = NodeIdParser(ByteVector(testData1))(initParsePosition)
    assert(pos1 == testData1.length)
    assert(nodeId1.namespaceIndex == 1, s"namespaceIndex should be 1, but we got ${nodeId1.namespaceIndex}")
    assert(nodeId1.identifier == NumericFourByteIdentifier(1))


    val testData2 = toByteArray("01 05 01 04")
    val (nodeId2, pos2) = NodeIdParser(ByteVector(testData2))(initParsePosition)
    assert(pos2 == testData2.length)
    assert(nodeId2.namespaceIndex == 5, s"namespaceIndex should be 5, but we got ${nodeId2.namespaceIndex}")
    assert(nodeId2.identifier == NumericFourByteIdentifier(1025))
  }

  "NodeIdParser # parseStringNodeId" should "parse a string NodeId successfully" in {
    val testData1 = toByteArray("03 01 00 06 00 00 00 72 111 74 230 176 180")
    val (nodeId1, pos1) = NodeIdParser(ByteVector(testData1))(initParsePosition)
    assert(pos1 == testData1.length)
    assert(nodeId1.namespaceIndex == 1, s"namespaceIndex should be 1, but we got ${nodeId1.namespaceIndex}")
    assert(nodeId1.identifier == StringIdentifier("HoJ水"))
  }

  // FIXME: This shit does not yet work now!
  ignore
  "NodeIdParser # parseGuidNodeId" should "parse a Guid NodeId successfully" in {
    val testData1 = toByteArray("04 04 00 91 43 96 72 75 250 230 74 141 18 180 04 220 125 175 63")
    val (nodeId1, pos1) = NodeIdParser(ByteVector(testData1))(initParsePosition)
    assert(pos1 == testData1.length)
    assert(nodeId1.namespaceIndex == 4, s"namespaceIndex should be 4, but we got ${nodeId1.namespaceIndex}")
    assert(nodeId1.identifier == GuidIdentifier(UUID.fromString("72962B91-FA75-4AE6-8D28-B404DC7DAF63")))
  }

  "NodeIdParser # parseByteStringNodeId" should "parse a ByteString / Opaque NodeId successfully" in {
    val testData1 = toByteArray("05 03 00 8 0 0 0 43 96 72 75 12 15 74 32")
    val (nodeId1, pos1) = NodeIdParser(ByteVector(testData1))(initParsePosition)
    assert(pos1 == testData1.length)
    assert(nodeId1.namespaceIndex == 3, s"namespaceIndex should be 3, but we got ${nodeId1.namespaceIndex}")
    assert(nodeId1.identifier == OpaqueIdentifier(toByteArray("43 96 72 75 12 15 74 32").toVector))
  }

  "NodeIdParser # parseUnknownNodeId" should "parse into an UnknownNodeIdentifier type" in {
    val testData1 = toByteArray("06 03 00")
    val (nodeId1, pos1) = NodeIdParser(ByteVector(testData1))(initParsePosition)
    assert(pos1 == 0) // ParsePosition will not be incremented!
    assert(nodeId1.namespaceIndex == 0, s"namespaceIndex should be 0, but we got ${nodeId1.namespaceIndex}")
    assert(nodeId1.identifier == UnknownIdentifier)
  }

  "NodeIdParser # random 1" should "parse into the appropriate NodeId structure" in {
    val testData1 = toByteArray("1 0 76 0 1 0 22 0 0 0 0 0")
    val (nodeId1, pos1) = NodeIdParser(ByteVector(testData1))(initParsePosition)
    assert(pos1 == 4) // ParsePosition will not be incremented!
    assert(nodeId1.namespaceIndex == 0, s"namespaceIndex should be 0, but we got ${nodeId1.namespaceIndex}")
    assert(nodeId1.identifier == NumericFourByteIdentifier(76))
  }
}
