package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageFrame.DataSetMessageKeyFrame
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector


object DataSetMessageKeyFrameParser extends (ByteVector => ParsePosition => (DataSetMessageKeyFrame, ParsePosition)) {

  override def apply(v1: ByteVector): ParsePosition => (DataSetMessageKeyFrame, ParsePosition) = ???
}
