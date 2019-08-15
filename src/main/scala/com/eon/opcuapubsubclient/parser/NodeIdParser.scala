package com.eon.opcuapubsubclient.parser

import java.nio.charset.StandardCharsets
import java.util.UUID

import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.{NodeId, NodeIdIdentifier}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteOrdering.{BigEndian, LittleEndian}
import scodec.bits.ByteVector

// FIXME: See this this implementation could be simplified and moved into ParserUtils!
object NodeIdParser extends (ByteVector => ParsePosition => (NodeId, ParsePosition)) {

  override def apply(byteVector: ByteVector): ParsePosition => (NodeId, ParsePosition) = parsePosition => {
    parseNodeId(byteVector, parsePosition)
  }

  def parseNodeId(byteVector: ByteVector, parsePosition: ParsePosition): (NodeId, ParsePosition) = {
    // 1. Get the Encoding Byte, which defines the size of the NodeId
    val (encodingByte, pos1) = (byteVector(parsePosition), parsePosition + 1)

    def nodeId(encodingByte: Byte, pos: ParsePosition): (NodeId, ParsePosition) = {
      val format = encodingByte & 0x0F
      val (defaultNsIndex, nsIndexPos) = // This applies to Numeric, String, Guid and Opaque types
        (byteVector.slice(from = pos, until = pos + 2).toShort(signed = false, ordering = LittleEndian), pos + 2)

      format match {
        case 0x00 => // Numeric 2 Byte
          (NodeId(
            namespaceIndex = 0, // See OPC UA Spec version 1.04, Part 6, Page 12, Figure 8
            NodeIdIdentifier.NumericTwoByteIdentifier(value = byteVector(pos))
          ), parsePosition + 2)
        case 0x01 => // Numeric 4 Byte
          val (nsIndex, pos1) = (byteVector(pos), pos + 1)
          val numericValue = byteVector.slice(from = pos1, until = pos1 + 2).toShort(signed = false, ordering = LittleEndian)
          (NodeId(
            namespaceIndex = nsIndex, // See OPC UA Spec version 1.04, Part 6, Page 13, Figure 9
            NodeIdIdentifier.NumericFourByteIdentifier(value = numericValue)
          ), parsePosition + 4)
        case 0x02 => // Numeric
          val (numericValue, pos1) =
            (byteVector.slice(from = nsIndexPos, until = nsIndexPos + 4).toInt(signed = false, ordering = LittleEndian), nsIndexPos + 4)
          (NodeId(
            namespaceIndex = defaultNsIndex,
            NodeIdIdentifier.NumericIdentifier(value = numericValue)
          ), parsePosition + pos1)
        case 0x03 => // String
          // In case of Strings, the length is captured as 4 bytes long
          val (lengthStr, pos1) =
            (byteVector.slice(from = nsIndexPos, until = nsIndexPos + 4).toInt(signed = false, ordering = LittleEndian), nsIndexPos + 4)

          /* FIXME: Consioder using scodec.codec which brings with it parsing capabilities wrapped in some context - Yes a Monad!
          val decoded = utf8.decodeValue(byteVector.slice(from = pos1, until = pos1 + lengthStr).toBitVector)
          decoded match {
            case Successful(reuslt) => println(reuslt)
            case Failure(cause) => println(cause)
          } */
          val (str, pos2) =
            (new String(byteVector.slice(from = pos1, until = pos1 + lengthStr).toArray, StandardCharsets.UTF_8), pos1 + lengthStr)
          (NodeId(
            namespaceIndex = defaultNsIndex, // See OPC UA Spec version 1.04, Part 6, Page 12, Figure 7
            NodeIdIdentifier.StringIdentifier(value = str)
          ), parsePosition + pos2)
        case 0x04 => // GUID: See OPC UA Spec version 1.04, Part 6, Page 11, Figure 5
          (NodeId(
            namespaceIndex = defaultNsIndex,
            NodeIdIdentifier.GuidIdentifier(value = ParserUtils.parseGuid(byteVector, pos1))
          ), nsIndexPos + 16) // GUID is always 16 bytes long as defined in the Spec!
        case 0x05 => // Opaque (ByteString) or can be treated as a Vector[Byte]
          val (byteStr, pos1) = ParserUtils.parseByteString(byteVector, nsIndexPos)
          (NodeId(
            namespaceIndex = defaultNsIndex,
            NodeIdIdentifier.OpaqueIdentifier(value = byteStr)
          ), parsePosition + pos1)
        case _    =>
          (NodeId(
            0, // This does not matter as anyway the NodeId is invalid!
            NodeIdIdentifier.UnknownIdentifier
          ), parsePosition)
      }
    }

    // 2. Populate the NodeId based on the encoding Byte
    nodeId(encodingByte, pos1)
  }
}
