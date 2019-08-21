package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient._
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.PayloadHeader.{DataSetMessagePayloadHeader => DSMPH}
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessage
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageFrame._
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageTypes.DataSetMessageType._
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

import scala.annotation.tailrec


object DataSetMessageParser extends (ByteVector => ParsePosition => DSMPH => V[(Vector[DataSetMessage], ParsePosition)]) {

  override def apply(v1: ByteVector): ParsePosition => DSMPH => V[(Vector[DataSetMessage], ParsePosition)] =
    parsePosition => dataSetMsgPayloadHdr => validated { parseDataSetMessages(v1, parsePosition, dataSetMsgPayloadHdr) }

  // OPC UA PubSub Spec., Part 14, version 1.04, Page 68, Chapter 7.2.2.3
  def parseDataSetMessages(byteVector: ByteVector, parsePosition: ParsePosition, dataSetMsgPayloadHdr: DSMPH): (Vector[DataSetMessage], ParsePosition) = {

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

    @tailrec
    def dataSetMessageSeq(dataSetMsgSizeSeq: Seq[Int], from: ParsePosition, acc: Vector[DataSetMessage]): (Vector[DataSetMessage], ParsePosition) = dataSetMsgSizeSeq match {
      case dataSetMsgSize :: Nil =>
        val (dataSetMsg, pos) = toDataSetMessage(dataSetMsgSize, from)
        (acc :+ dataSetMsg, pos)
      case dataSetMsgSize :: xs =>
        val (dataSetMsg, pos) = toDataSetMessage(dataSetMsgSize, from)
        dataSetMessageSeq(xs, pos, acc :+ dataSetMsg)
    }

    def toDataSetMessage(messageSize: Int, pos: ParsePosition): (DataSetMessage, ParsePosition) = {
      val (dataSetMsgHeader, pos1) = DataSetMessageHeaderParser(byteVector)(pos)
      val (dataSetMsgFrame, pos2) = dataSetMsgHeader.dataSetFlags2.dataSetMessageType match {
        case KeyFrame => dataSetKeyFrame(messageSize, pos1)
        case DeltaFrame => dataSetDeltaFrame(messageSize, pos1)
        case Event => dataSetEvent(messageSize, pos1)
        case KeepAlive => dataSetKeepAlive(messageSize, pos1)
      }
      (DataSetMessage(
        dataSetMsgHeader,
        dataSetMsgFrame
      ), pos2)
    }

    // TODO: Implement!
    def dataSetKeyFrame(messageSize: Int, pos: ParsePosition): (DataSetMessageKeyFrame, ParsePosition) = {
      val (fieldCount, pos1) = ParserUtils.parseUInt16(byteVector, pos)
      (DataSetMessageKeyFrame(), pos1)
    }

    def dataSetDeltaFrame(messageSize: Int, pos: ParsePosition): (DataSetMessageDeltaFrame, ParsePosition) = {
      (DataSetMessageDeltaFrame(), pos)
    }

    def dataSetEvent(messageSize: Int, pos: ParsePosition): (DataSetMessageEvent, ParsePosition) = {
      (DataSetMessageEvent(), pos)
    }

    def dataSetKeepAlive(messageSize: Int, pos: ParsePosition): (DataSetMessageKeepAlive, ParsePosition) = {
      (DataSetMessageKeepAlive(), pos)
    }

    dataSetMessageSeq(sizes, pos1, Vector.empty)
  }
}
