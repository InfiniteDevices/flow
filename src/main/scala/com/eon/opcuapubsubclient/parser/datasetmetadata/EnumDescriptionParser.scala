package com.eon.opcuapubsubclient.parser.datasetmetadata

import com.eon.opcuapubsubclient.domain.PayloadTypes.EnumDescription
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

// TODO: Implementation pending
object EnumDescriptionParser extends (ByteVector => ParsePosition => (Vector[EnumDescription], ParsePosition)) {

  override def apply(v1: ByteVector): ParsePosition => (Vector[EnumDescription], ParsePosition) =
    parsePosition => (Vector.empty, parsePosition)
}
