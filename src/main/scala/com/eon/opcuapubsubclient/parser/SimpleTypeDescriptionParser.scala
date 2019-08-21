package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.domain.PayloadTypes.SimpleTypeDescription
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

// TODO: Implementation pending
object SimpleTypeDescriptionParser extends (ByteVector => ParsePosition => (Vector[SimpleTypeDescription], ParsePosition)) {

  override def apply(v1: ByteVector): ParsePosition => (Vector[SimpleTypeDescription], ParsePosition) =
    parsePosition => (Vector.empty, parsePosition)
}
