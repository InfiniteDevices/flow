package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.V
import com.eon.opcuapubsubclient._
import com.eon.opcuapubsubclient.domain.HeaderTypes.NetworkMessageTypes
import com.eon.opcuapubsubclient.domain.HeaderTypes.NetworkMessageTypes.NetworkMessageType
import com.eon.opcuapubsubclient.domain.PayloadTypes.Payload
import com.eon.opcuapubsubclient.domain.PayloadTypes.Payload.{DiscoveryResponsePayload, InvalidPayload}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

// TODO: Implementation pending
object PayloadParser extends (ByteVector => NetworkMessageType => ParsePosition => V[(Payload, ParsePosition)]) {

  override def apply(v1: ByteVector): NetworkMessageType => ParsePosition => V[(Payload, ParsePosition)] =
    networkMsgType => parsePosition => validated { parsePayload(v1, networkMsgType, parsePosition) }

  def parsePayload(byteVector: ByteVector, networkMsgType: NetworkMessageType, parsePosition: ParsePosition): (Payload, ParsePosition) = {
    networkMsgType match {
      case NetworkMessageTypes.DiscoveryRequestType =>
        (InvalidPayload(), parsePosition) // TODO: Implementation pending!
      case NetworkMessageTypes.DiscoveryResponseType =>
        val (dataSetWriterId, pos1) = ParserUtils.parseUInt16(byteVector, parsePosition)
        val (dataSetMetadata, pos2) = DataSetMetaDataParser(byteVector)(pos1)
        val (status, pos3) = ParserUtils.parseStatusCode(byteVector, pos2)
        (DiscoveryResponsePayload(dataSetWriterId, dataSetMetadata, status), pos3)
      case NetworkMessageTypes.DataSetMessageType =>
        (InvalidPayload(), parsePosition) // TODO: Implementation pending!
      case _ =>
        (InvalidPayload(), parsePosition)
    }
  }
}
