package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageFrame.DataSetMessageKeepAlive
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector


object DataSetMessageKeepAliveParser extends (ByteVector => Int => ParsePosition => (DataSetMessageKeepAlive, ParsePosition)) {

  /**
    * As per the OPC UA PubSub spec., Part 14, version 1.04, page 74, Chapter 7.2.2.3.8,
    * the KeepAlive DataSetMessage does not add any additional fields!
    *
    * TODO: Check if this implementation is correct
    *
    * @param v1
    * @return
    */
  override def apply(v1: ByteVector): Int => ParsePosition => (DataSetMessageKeepAlive, ParsePosition) = sequenceNumber => parsePosition => {
    (DataSetMessageKeepAlive(sequenceNumber), parsePosition)
  }
}
