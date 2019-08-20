package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient._
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.DataSetMessage
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.PayloadHeader.{DataSetMessagePayloadHeader => DSMPH}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

import scala.annotation.tailrec


object DataSetMessageParser extends (ByteVector => ParsePosition => DSMPH => V[(DataSetMessage, ParsePosition)]) {

  override def apply(v1: ByteVector): ParsePosition => DSMPH => V[(DataSetMessage, ParsePosition)] =
    parsePosition => dataSetMsgPayloadHdr => validated { parseDataSetMessage(v1, parsePosition, dataSetMsgPayloadHdr) }

  // OPC UA PubSub Spec., Part 14, version 1.04, Page 68, Chapter 7.2.2.3
  def parseDataSetMessage(byteVector: ByteVector, parsePosition: ParsePosition, dataSetMsgPayloadHdr: DSMPH): (DataSetMessage, ParsePosition) = {

    @tailrec
    def dataSetMessageSize(count: Int, from: ParsePosition, acc: Vector[Int]): (Vector[Int], ParsePosition) = {
      if (count < 0) (acc, from)
      else {
        val (size, pos) = ParserUtils.parseUInt16(byteVector, from)
        dataSetMessageSize(count - 1, pos, acc :+ size)
      }
    }

    // For the size in the header, read 2 bytes for each element which would give the size of the actual DataSetMessage
    val (sizes, pos1) = dataSetMessageSize(dataSetMsgPayloadHdr.messageCount, parsePosition, Vector.empty)
  }
}
