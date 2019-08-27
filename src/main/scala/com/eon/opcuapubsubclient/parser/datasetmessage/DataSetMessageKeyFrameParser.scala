package com.eon.opcuapubsubclient.parser.datasetmessage

import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageFrame.DataSetMessageKeyFrame
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

// TODO: Implementation pending
object DataSetMessageKeyFrameParser extends (ByteVector => ParsePosition => (DataSetMessageKeyFrame, ParsePosition)) {

  override def apply(v1: ByteVector): ParsePosition => (DataSetMessageKeyFrame, ParsePosition) = ???
}
