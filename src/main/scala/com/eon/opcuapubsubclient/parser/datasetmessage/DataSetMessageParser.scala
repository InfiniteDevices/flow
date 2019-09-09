package com.eon.opcuapubsubclient.parser.datasetmessage

import com.eon.opcuapubsubclient.domain.PayloadTypes.{DataSetMessage, DataSetMessageHeader}
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageFrame.{DataSetMessageDeltaFrame, DataSetMessageEvent, DataSetMessageKeepAlive, DataSetMessageKeyFrame}
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageTypes.DataSetMessageType.{DeltaFrame, Event, KeepAlive, KeyFrame}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import com.eon.opcuapubsubclient.parser.ParserUtils
import com.eon.opcuapubsubclient.domain.HeaderTypes.PayloadHeader.{DataSetMessagePayloadHeader => DSMPH}
import com.eon.opcuapubsubclient.{V, validated}
import scodec.bits.ByteVector

import scala.annotation.tailrec


object DataSetMessageParser extends (ByteVector => ParsePosition => DSMPH => V[(Vector[DataSetMessage], ParsePosition)]) {

  override def apply(v1: ByteVector): ParsePosition => DSMPH => V[(Vector[DataSetMessage], ParsePosition)] =
    parsePosition => dataSetMsgPayloadHdr => validated { parseDataSetMessages(v1, parsePosition, dataSetMsgPayloadHdr) }

  // OPC UA PubSub Spec., Part 14, version 1.04, Page 68, Chapter 7.2.2.3
  def parseDataSetMessages(byteVector: ByteVector, parsePosition: ParsePosition, dataSetMsgPayloadHdr: DSMPH): (Vector[DataSetMessage], ParsePosition) = {

    @tailrec
    def dataSetMessageSize(count: Int, from: ParsePosition, acc: Vector[Int]): (Vector[Int], ParsePosition) = {
      if (count <= 0) (acc, from)
      else {
        val (size, pos) = ParserUtils.parseUInt16(byteVector, from)
        dataSetMessageSize(count - 1, pos, acc :+ size)
      }
    }

    // For the size in the header, read 2 bytes for each element which would give the size of the actual DataSetMessage
    val (sizes, pos1) = dataSetMessageSize(dataSetMsgPayloadHdr.messageCount, parsePosition, Vector.empty)

    @tailrec
    def dataSetMessageSeq(dataSetMsgSizeSeq: Seq[Int], from: ParsePosition, acc: Vector[DataSetMessage]): (Vector[DataSetMessage], ParsePosition) = dataSetMsgSizeSeq match {
      case Seq(dataSetMsgSize) =>
        val (dataSetMsg, pos) = toDataSetMessage(dataSetMsgSize, from)
        (acc :+ dataSetMsg, pos)
      case dataSetMsgSize :: xs =>
        val (dataSetMsg, pos) = toDataSetMessage(dataSetMsgSize, from)
        dataSetMessageSeq(xs, pos, acc :+ dataSetMsg)
    }

    def toDataSetMessage(messageSize: Int, pos: ParsePosition): (DataSetMessage, ParsePosition) = {
      val (dataSetMsgHeader, pos1) = DataSetMessageHeaderParser(byteVector)(pos)
      val (dataSetMsgFrame, pos2) = dataSetMsgHeader.dataSetFlags2.dataSetMessageType match {
        case KeyFrame => dataSetKeyFrame(dataSetMsgHeader, pos1) // This is the default as mentioned in the spec!
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
    def dataSetKeyFrame(dataSetMsgHeader: DataSetMessageHeader, pos: ParsePosition): (DataSetMessageKeyFrame, ParsePosition) = {
      DataSetMessageKeyFrameParser(byteVector)(dataSetMsgHeader)(pos)
    }

    // TODO: Implement!
    def dataSetDeltaFrame(messageSize: Int, pos: ParsePosition): (DataSetMessageDeltaFrame, ParsePosition) = {
      DataSetMessageDeltaFrameParser(byteVector)(pos)
    }

    // TODO: Implement!
    def dataSetEvent(messageSize: Int, pos: ParsePosition): (DataSetMessageEvent, ParsePosition) = {
      (DataSetMessageEvent(), pos)
    }

    // TODO: Implement!
    def dataSetKeepAlive(messageSize: Int, pos: ParsePosition): (DataSetMessageKeepAlive, ParsePosition) = {
      DataSetMessageKeepAliveParser(byteVector)(0)(pos) // TODO: What about the MessageSequenceNumber, Here I'm using 0 as default and this is wrong!
    }

    dataSetMessageSeq(sizes, pos1, Vector.empty)
  }
}
