package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient._
import com.eon.opcuapubsubclient.domain.HeaderTypes.NetworkMessageTypes.NetworkMessageType
import com.eon.opcuapubsubclient.domain.HeaderTypes.{NetworkMessageTypes, PayloadHeader}
import com.eon.opcuapubsubclient.domain.HeaderTypes.PayloadHeader._
import com.eon.opcuapubsubclient.domain.PayloadTypes.DiscoveryRequestMessageTypes
import com.eon.opcuapubsubclient.domain.PayloadTypes.DiscoveryResponseMessageTypes._
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

import scala.annotation.tailrec

// TODO: Refactor & reuse from ParserUtils!
object PayloadHeaderParser extends (ByteVector => NetworkMessageType => ParsePosition => V[(PayloadHeader, ParsePosition)]) {

  override def apply(byteVector: ByteVector): NetworkMessageType => ParsePosition => V[(PayloadHeader, ParsePosition)]= {
    msgType => parsePosition => validated { parsePayloadHeader(byteVector, msgType, parsePosition) }
  }

  def parsePayloadHeader(byteVector: ByteVector, msgType: NetworkMessageType, parsePosition: ParsePosition): (PayloadHeader, ParsePosition) = {
    msgType match {
      case NetworkMessageTypes.DiscoveryRequestType =>
        val (requestType, pos1) = ParserUtils.parseByteAsInt(byteVector, parsePosition)
        requestType match {
          case 0 =>
            (InvalidPayloadHeader("Discovery Request Header cannot be of type " +
              "Reserved (See OPC UA Pub Sub Part 14 Spec., Page 75 Table 85"), pos1)
          case 1 =>
            val (byteAsInt, pos2) = ParserUtils.parseByteAsInt(byteVector, pos1)
            val informationType = byteAsInt match {
              case 0 => DiscoveryRequestMessageTypes.Reserved
              case 1 => DiscoveryRequestMessageTypes.PublisherServer
              case 2 => DiscoveryRequestMessageTypes.DataSetMetaData
              case 3 => DiscoveryRequestMessageTypes.DataSetWriterConfig
            }
            // TODO FIXME: DataSetWriterId parsing is pending
            (DiscoveryRequestMessagePayloadHeader(informationType, Vector.empty), pos2)
        }

      case NetworkMessageTypes.DiscoveryResponseType =>
        /*
          From Page 75 of the OPC UA Pub Sub Spec - Part 14
          The following types of discovery response messages are defined.
          0 Reserved
          1 Publisher Endpoint message (see 7.2.2.4.2.3)
          2 DataSet Metadata message (see 7.2.2.4.2.4)
          3 DataSetWriter configuration message (see 7.2.2.4.2.5)
         */
        val (byteAsInt, pos1) = ParserUtils.parseByteAsInt(byteVector, parsePosition)
        val discoveryResponseMsgTyp = byteAsInt match {
          case 0 => Reserved
          case 1 => PublisherEndPoint
          case 2 => DataSetMetaData
          case 3 => DataSetWriterConfig
        }
        val (sequenceNumber, pos2) = ParserUtils.parseUInt16(byteVector, pos1)

        if (discoveryResponseMsgTyp == Reserved)
          (InvalidPayloadHeader("Discovery Response Message Header cannot be of type " +
            "Reserved (See OPC UA Pub Sub Part 14 Spec., Page 75 Table 87"), pos2)
        else
          (DiscoveryResponseMessagePayloadHeader(discoveryResponseMsgTyp, sequenceNumber), pos2)

      // This is the default
      case NetworkMessageTypes.DataSetMessageType | _ =>
        /*
          Number of DataSetMessages contained in the NetworkMessage.
          The NetworkMessage shall contain at least one DataSetMessages
          if the NetworkMessage type is DataSetMessage payload.
        */
        val (count, pos1) = ParserUtils.parseByte(byteVector, parsePosition)

        @tailrec
        def dataSetWriterIds(size: Int, from: Int, acc: Vector[Int]): (Vector[Int], ParsePosition) = {
          if (count < size) {
            val (dataSetWriterId, nPos) = ParserUtils.parseUInt16(byteVector, from)
            (acc :+ dataSetWriterId, nPos)
          } else {
            val (dataSetWriterId, nPos) = ParserUtils.parseUInt16(byteVector, from)
            dataSetWriterIds(size + 1, nPos, acc :+ dataSetWriterId)
          }
        }

        if (count < 1) {
          val msg = s"As per the Spec., we should have at least one DataSetMessage in the Payload, " +
            s"but seems this is not the case count in the DataSet PayloadHeader is = $count and is not valid!"
          (InvalidPayloadHeader(msg), pos1)
        }
        else {
          val (dataSetWriterIdSeq, newPos) = dataSetWriterIds(0, pos1, Vector.empty)
          (DataSetMessagePayloadHeader(count, dataSetWriterIdSeq), newPos)
        }
    }
  }
}
