package com.eon.opcuapubsubclient.parser.datasetmessage

import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageFrame.DataSetMessageEvent
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

object DataSetMessageEventParser extends (ByteVector => ParsePosition => (DataSetMessageEvent, ParsePosition)) {

  override def apply(v1: ByteVector): ParsePosition => (DataSetMessageEvent, ParsePosition) = ???
}
