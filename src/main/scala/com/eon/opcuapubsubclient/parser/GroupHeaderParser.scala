package com.eon.opcuapubsubclient.parser

import java.nio.{ByteBuffer, ByteOrder}

import com.eon.opcuapubsubclient._
import com.eon.opcuapubsubclient.domain.HeaderTypes.GroupHeader
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.{BitVector, ByteVector}


object GroupHeaderParser extends (ByteVector => ParsePosition => V[(GroupHeader, ParsePosition)]) {

  override def apply(byteVector: ByteVector): ParsePosition => V[(GroupHeader, ParsePosition)] =
    parsePosition => validated { parseGroupHeader(byteVector, parsePosition) }

  def parseGroupHeader(byteVector: ByteVector, parsePosition: ParsePosition): (GroupHeader, ParsePosition) = {

    def groupHeaderFlags(bitVector: BitVector): (GroupHeader, Int) = {
      (GroupHeader(
        writerGroupIdEnabled = bitVector(7),
        groupVersionEnabled = bitVector(6),
        networkMessageNumberEnabled = bitVector(5),
        sequenceNumberEnabled =bitVector(4)
      ), parsePosition + 1)
    }

    val (grpHeader, pos1) = groupHeaderFlags(BitVector(byteVector(parsePosition)))

    val (writerGrpId, pos2) = if (grpHeader.writerGroupIdEnabled)
      (Some(ByteBuffer.wrap(byteVector.slice(from = pos1, until = pos1 + 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt), pos1 + 4) else (None, pos1)
    val (grpVersion, pos3) = if (grpHeader.writerGroupIdEnabled)
      (Some(ByteBuffer.wrap(byteVector.slice(from = pos1, until = pos1 + 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt), pos2 + 4) else (None, pos2)
    val (networkMsgNumber, pos4) = if (grpHeader.writerGroupIdEnabled)
      (Some(ByteBuffer.wrap(byteVector.slice(from = pos1, until = pos1 + 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt), pos3 + 4) else (None, pos3)
    val (sequenceNumber, pos5) = if (grpHeader.writerGroupIdEnabled)
      (Some(ByteBuffer.wrap(byteVector.slice(from = pos1, until = pos1 + 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt), pos4 + 4) else (None, pos4)

    (grpHeader.copy(
      writerGroupId = writerGrpId,
      groupVersion = grpVersion,
      networkMessageNumber = networkMsgNumber,
      sequenceNumber = sequenceNumber,
    ), pos5)
  }
}
