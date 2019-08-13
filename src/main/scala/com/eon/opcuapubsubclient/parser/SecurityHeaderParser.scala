package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient._
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.SecurityHeader
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

// TODO: Implementation pending
object SecurityHeaderParser extends (ByteVector => ParsePosition =>  V[(SecurityHeader, ParsePosition)]) {

  override def apply(byteVector: ByteVector): ParsePosition => V[(SecurityHeader, ParsePosition)] =
    parsePosition => validated { (SecurityHeader(), parsePosition) }
}
