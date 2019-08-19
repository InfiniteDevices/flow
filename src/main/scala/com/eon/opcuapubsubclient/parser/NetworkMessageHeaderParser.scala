package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient._
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.{ExtendedFlags1, ExtendedFlags2, NetworkMessageHeader, NetworkMessageTypes, PublisherIDTypes}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.{BitVector, ByteVector}


object NetworkMessageHeaderParser extends (ByteVector => ParsePosition => V[(NetworkMessageHeader, ParsePosition)]) {

  override def apply(v1: ByteVector): ParsePosition => V[(NetworkMessageHeader, ParsePosition)] =
    parsePosition => validated { parseNetworkMessageHeader(v1, parsePosition) }

  def networkMessageHeader(bitVector: BitVector, position: Int): (NetworkMessageHeader, Int) = {
    (NetworkMessageHeader(
      // Bit range 0-3: Version of the UADP NetworkMessage
      version = bitVector.takeRight(4).toInt(signed = false),
      // Bit range 4-7 contains the flags
      publisherIdEnabled = bitVector.get(3),
      groupHeaderEnabled = bitVector.get(2),
      payloadHeaderEnabled = bitVector.get(1),
      extendedFlags1Enabled = bitVector.get(0)
    ), position + 1)
  }

  def extendedFlags1(bitVector: BitVector, position: Int): (ExtendedFlags1, Int) = {
    // Bit range 0-2 is the PublisherIdType
    val publisherIdType = bitVector.takeRight(3).toInt(signed = false) match {
      case 0 => PublisherIDTypes.UByte
      case 1 => PublisherIDTypes.UInt16
      case 2 => PublisherIDTypes.UInt32
      case 3 => PublisherIDTypes.UInt64
      case 4 => PublisherIDTypes.String
      // This is the default
      case _ => PublisherIDTypes.UByte
    }
    (ExtendedFlags1(
      publisherIdType = publisherIdType,
      dataSetClassIDEnabled = bitVector(4),
      securityEnabled = bitVector(3),
      timeStampEnabled = bitVector(2),
      picoSecondsEnabled = bitVector(1),
      extendedFlags2Enabled = bitVector(0)
    ), position + 1)
  }

  def extendedFlags2(bitVector: BitVector, position: Int): (ExtendedFlags2, Int) = {
    val networkMessageType = bitVector.slice(from = 3, until = 6).toInt(signed = false) match {
      case 0 => NetworkMessageTypes.DataSetMessageType
      case 1 => NetworkMessageTypes.DiscoveryRequestType
      case 2 => NetworkMessageTypes.DiscoveryResponseType
      // This is the default
      case _ => NetworkMessageTypes.DataSetMessageType
    }
    (ExtendedFlags2(
      isChunkMessage = bitVector(7),
      promotedFieldsEnabled = bitVector(6),
      networkMessageType
    ), position + 1)
  }

  def parseNetworkMessageHeader(byteVector: ByteVector, parsePosition: Int = 0): (NetworkMessageHeader, Int) = {
    val (networkMsgHeader, pos1) = networkMessageHeader(BitVector(byteVector.head), parsePosition)

    // Check if we have ExtendedFlags1 set, if not, do not increment the position
    val (extFlags1, pos2) = if (networkMsgHeader.extendedFlags1Enabled)
      extendedFlags1(BitVector(byteVector(pos1)), pos1)
    else (ExtendedFlags1(), pos1)

    // Check if we have ExtendedFlags2 set, if not, do not increment the position
    val (extFlags2, pos3) = if (networkMsgHeader.extendedFlags1Enabled)
      extendedFlags2(BitVector(byteVector(pos2)), pos2)
    else (ExtendedFlags2(), pos2)

    // The PublisherId shall be omitted if bit 4 of the UADPFlags is false
    val (somePublisherId, pos4): (Option[String], Int) = if (networkMsgHeader.publisherIdEnabled) {
      val (publisherId, nPos) = ParserUtils.parseString(byteVector, pos3)
      (Some(publisherId), nPos)
    } else (None, pos3)

    // The DataSetClassId shall be omitted if bit 3 of the ExtendedFlags1 is false
    val (someDataSetClassId, pos5) = if (extFlags1.dataSetClassIDEnabled) {
      val (dataSetClassId, nPos) = ParserUtils.parseGuid(byteVector, pos4)
      (Some(dataSetClassId), nPos) // GUID is always 16 bytes long
    } else (None, pos4)

    // Finally at the end of the world, we get this ugly NetworkMessageHeader
    (networkMsgHeader.copy(
      publisherId = somePublisherId,
      dataSetClassId = someDataSetClassId,
      extendedFlags1 = extFlags1,
      extendedFlags2 = extFlags2
    ), pos5)
  }
}
