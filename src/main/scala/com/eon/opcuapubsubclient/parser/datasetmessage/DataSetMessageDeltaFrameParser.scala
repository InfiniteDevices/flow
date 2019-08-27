package com.eon.opcuapubsubclient.parser.datasetmessage

import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageFrame.DataSetMessageDeltaFrame
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector


object DataSetMessageDeltaFrameParser extends (ByteVector => ParsePosition => (DataSetMessageDeltaFrame, ParsePosition)) {

  override def apply(v1: ByteVector): ParsePosition => (DataSetMessageDeltaFrame, ParsePosition) = ???
}
