package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient._
import com.eon.opcuapubsubclient.domain.HeaderTypes.{ExtendedFlags1, ExtendedFlags2, ExtendedNetworkMessageHeader}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

// TODO: Implementation Pending
object ExtendedNetworkMsgHeaderParser extends (ByteVector => ExtendedFlags1 => ExtendedFlags2 => ParsePosition => V[(ExtendedNetworkMessageHeader, ParsePosition)]) {

  override def apply(byteVector: ByteVector): ExtendedFlags1 => ExtendedFlags2 => ParsePosition => V[(ExtendedNetworkMessageHeader, ParsePosition)] =
    extFlags1 => extFlags2 => parsePosition => validated { parseExtNetworkMsgHeader(byteVector, extFlags1, extFlags2, parsePosition) }

  def parseExtNetworkMsgHeader(byteVector: ByteVector, extlags1: ExtendedFlags1, extFlags2: ExtendedFlags2, pos: ParsePosition): (ExtendedNetworkMessageHeader, ParsePosition) = {
    val (someTimestamp, pos1) = if (extlags1.timeStampEnabled) {
      (None, pos) // TODO: Parsing pending!
    } else (None, pos)

    val (somePicoSeconds, pos2) = if (extlags1.picoSecondsEnabled) { // PicoSeconds is UInt16
      val (picoSeconds, nPos) = ParserUtils.parseUInt16(byteVector, pos1)
      (Some(picoSeconds), nPos)
    } else (None, pos)

    val (promotedFields, pos3) = if (extFlags2.promotedFieldsEnabled) {
      (Vector.empty, pos2) // TODO: Parsing pending!
    } else (Vector.empty, pos2)

    (ExtendedNetworkMessageHeader(
      someTimestamp,
      somePicoSeconds,
      promotedFields
    ), pos3)
  }
}
