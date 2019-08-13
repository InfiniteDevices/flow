package com.eon.opcuapubsubclient.parser

import java.nio.{ByteBuffer, ByteOrder}

import com.eon.opcuapubsubclient._
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.DiscoveryResponseMessageTypes.{DataSetMetaData, DataSetWriterConfig, PublisherEndPoint, Reserved}
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.NetworkMessageTypes.NetworkMessageType
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.{DiscoveryRequestMessageTypes, NetworkMessageTypes, PayloadHeader}
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.PayloadHeader.{DataSetPayloadHeader, DiscoveryRequestPayloadHeader, DiscoveryResponsePayloadHeader, InvalidPayloadHeader}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.{BitVector, ByteVector}

import scala.annotation.tailrec


object PayloadHeaderParser extends (ByteVector => NetworkMessageType => ParsePosition => V[(PayloadHeader, ParsePosition)]) {

  override def apply(byteVector: ByteVector): NetworkMessageType => ParsePosition => V[(PayloadHeader, ParsePosition)]= {
    msgType => parsePosition => validated { parsePayloadHeader(byteVector, msgType, parsePosition) }
  }

  def parsePayloadHeader(byteVector: ByteVector, msgType: NetworkMessageType, parsePosition: ParsePosition): (PayloadHeader, ParsePosition) = {
    msgType match {
      case NetworkMessageTypes.DiscoveryRequestType =>
        val (requestType, pos1) = (BitVector(byteVector(parsePosition)).toInt(signed = false), parsePosition + 1)
        requestType match {
          case 0 =>
            (InvalidPayloadHeader("Discovery Request Header cannot be of type " +
              "Reserved (See OPC UA Pub Sub Part 14 Spec., Page 75 Table 85"), pos1)
          case 1 =>
            val (informationType, pos2) = (BitVector(byteVector(pos1)).toInt(signed = false) match {
              case 0 => DiscoveryRequestMessageTypes.Reserved
              case 1 => DiscoveryRequestMessageTypes.PublisherServer
              case 2 => DiscoveryRequestMessageTypes.DataSetMetaData
              case 3 => DiscoveryRequestMessageTypes.DataSetWriterConfig
            }, pos1 + 1)
            // TODO FIXME: DataSetWriterId parsing is pending
            (DiscoveryRequestPayloadHeader(informationType, Vector.empty), pos2)
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
        val (discoveryResponseMsgTyp, pos1) = (BitVector(byteVector(parsePosition)).toInt(signed = false) match {
          case 0 => Reserved
          case 1 => PublisherEndPoint
          case 2 => DataSetMetaData
          case 3 => DataSetWriterConfig
        }, parsePosition + 1)
        val (sequenceNumber, pos2) = (ParserUtils.sliceToUInt(byteVector, pos1, pos1 + 2), pos1 + 2)

        if (discoveryResponseMsgTyp == Reserved)
          (InvalidPayloadHeader("Discovery Response Message Header cannot be of type " +
            "Reserved (See OPC UA Pub Sub Part 14 Spec., Page 75 Table 87"), pos2)
        else
          (DiscoveryResponsePayloadHeader(discoveryResponseMsgTyp, sequenceNumber), pos2)

      // This is the default
      case NetworkMessageTypes.DataSetMessageType | _ =>
        /*
          Number of DataSetMessages contained in the NetworkMessage.
          The NetworkMessage shall contain at least one DataSetMessages
          if the NetworkMessage type is DataSetMessage payload.
        */
        val (count, pos1) = (BitVector(byteVector(parsePosition)).toInt(signed = false), parsePosition + 1)

        @tailrec
        def populateDataSetWriterIds(size: Int, newPos: Int, dataSetWriterIds: Vector[Int]): (Vector[Int], ParsePosition) = {
          if (count <= size) {
            val dataSetWriterId = ByteBuffer.wrap(byteVector.slice(from = newPos, until = newPos + 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt
            (dataSetWriterIds :+ dataSetWriterId, newPos + 4)
          } else {
            val dataSetWriterId = ByteBuffer.wrap(byteVector.slice(from = newPos, until = newPos + 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt
            populateDataSetWriterIds(size + 1, newPos + 4, dataSetWriterIds :+ dataSetWriterId)
          }
        }

        if (count < 1) {
          val msg = s"As per the Spec., we should have at least one DataSetMessage in the Payload, " +
            s"but seems this is not the case count in the DataSet PayloadHeader is = $count and is not valid!"
          (InvalidPayloadHeader(msg), pos1)
        }
        else {
          val (dataSetWriterIds, newPos) = populateDataSetWriterIds(0, pos1, Vector.empty)
          (DataSetPayloadHeader(count, dataSetWriterIds), newPos)
        }
    }
  }
}
