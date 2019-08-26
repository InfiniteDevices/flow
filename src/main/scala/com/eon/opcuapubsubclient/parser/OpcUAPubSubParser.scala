package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.config.OpcUAPubSubConfig
import OpcUAPubSubParser._
import com.eon.opcuapubsubclient._
import com.eon.opcuapubsubclient.domain.HeaderTypes.{GroupHeader, SecurityHeader}
import scodec.bits.ByteVector

/**
  * TODO: Scaladoc
  * @param config The configuration representing the underlying OPC UA PubSub broker
  */
class OpcUAPubSubParser(config: OpcUAPubSubConfig) {

  def parse(bytes: Seq[Byte]): Unit = {
    val byteVector = ByteVector(bytes)

    for {
      // STEP 1: Parse the NetworkMessageHeader and move the ParsePosition accordingly
      parsedNetworkMsg <- NetworkMessageHeaderParser(byteVector)(startParsePosition).right
      (networkMsgHeader, networkMsgParsePosition) = parsedNetworkMsg

      // STEP 2: Parse the GroupHeader and move the ParsePosition accordingly
      parsedGrpHeader <- {
        if (networkMsgHeader.groupHeaderEnabled)
          GroupHeaderParser(byteVector)(networkMsgParsePosition).right
        else
          validated { (GroupHeader(), networkMsgParsePosition) }.right
      }
      (_, grpHeaderParsePosition) = parsedGrpHeader

      extFlags1 = networkMsgHeader.extendedFlags1
      extFlags2 = networkMsgHeader.extendedFlags2

      // STEP 3: Parse the PayloadHeader and move the ParsePosition accordingly
      networkMsgType = extFlags2.networkMessageType
      parsedPayloadHeader <- PayloadHeaderParser(byteVector)(networkMsgType)(grpHeaderParsePosition).right
      (payloadHeader, payloadHeaderParsePosition) = parsedPayloadHeader

      // STEP 4: Parse the ExtendedNetworkMessageHeader and move the ParsePosition accordingly
      parsedExtNetworkMsgHeader <- ExtendedNetworkMsgHeaderParser(byteVector)(extFlags1)(extFlags2)(payloadHeaderParsePosition).right
      (_, extNetworkMsgHeaderParsePosition) = parsedExtNetworkMsgHeader

      // STEP 5: Parse the SecurityHeader and move the ParsePosition accordingly
      parsedSecurityHeader <- {
        if (networkMsgHeader.extendedFlags1.securityEnabled)
          SecurityHeaderParser(byteVector)(extNetworkMsgHeaderParsePosition).right
        else
          validated { (SecurityHeader(), extNetworkMsgHeaderParsePosition) }.right
      }
      (_, securityHeaderParsePosition) = parsedSecurityHeader

      // STEP 6: Based on the NetworkMessageType, we parse the Payload accordingly
      parsedPayload <- PayloadParser(byteVector)(networkMsgType)(securityHeaderParsePosition)
      (payload, payloadParsePosition) = parsedPayload

      //parsedDataSetMetaData <- DataSetMetaDataParser(byteVector)(securityHeaderParsePosition)
      //(_, dataSetMetaDataParsePos) = parsedDataSetMetaData
    } yield {
      println(dataSetMetaDataParsePos)
      // TODO: This is where we shoot the parsed objects into a JSON structure
      println("OK")
    }
  }
}
object OpcUAPubSubParser {

  type ParsePosition = Int
  val startParsePosition: ParsePosition = 0

  def apply(opcUAPubSubConfig: OpcUAPubSubConfig) =
    new OpcUAPubSubParser(opcUAPubSubConfig)
}
