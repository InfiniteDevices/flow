package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient._
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.{ExtendedFlags1, ExtendedFlags2, NetworkMessageHeader, NetworkMessageTypes, PublisherIDTypes}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.{BitVector, ByteVector}


object NetworkMessageHeaderParser extends (ByteVector => ParsePosition => V[(NetworkMessageHeader, ParsePosition)]) {
/*
  object FuncA extends (String => Int => V[String]) {
    override def apply(str: String): ParsePosition => V[String] = i => {
     validated ((str.toInt + i).toString )
    }
  }

  object FuncB extends (String => V[Int]) {
    override def apply(str: String): V[Int] = {
      Right(str.toInt)
    }
  }

  val sdsdsd = FuncA("1")(1)

  for {
    funcAStr <- FuncA("1")(1)
    funcBStr <- FuncB(funcAStr)
  } yield {
    println(s"Final String incremented as int is $funcBStr")
  } */

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

    // The PublisherId shall be omitted if bit 4 of the UADPFlags is false // TODO: Refactor this shit later!
    val (somePublisherId, pos4): (Option[String], Int) = if (networkMsgHeader.publisherIdEnabled) { // TODO: PublisherID type check and parse accordingly!!
      val publisherIdSize = ParserUtils.sliceToUInt(byteVector, pos3, pos3 + 4)
      val pubId = Some(byteVector.slice(from = pos3 + 4, until = pos3 + 4 + publisherIdSize).foldLeft("")((a, b) => {
        a + b.toChar
      }))
      (pubId, pos3 + 4 + publisherIdSize)
    } else (None, pos3)

    // The DataSetClassId shall be omitted if bit 3 of the ExtendedFlags1 is false
    val (someDataSetClassId, pos5) = if (extFlags1.dataSetClassIDEnabled) {
      (Some(ParserUtils.parseGuid(byteVector, pos4)), pos4 + 16) // GUID is always 16 bytes long
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
