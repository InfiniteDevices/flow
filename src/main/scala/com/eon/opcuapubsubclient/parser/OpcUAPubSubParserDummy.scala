package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.config.OpcUAPubSubConfig
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.{NetworkMessageTypes, PublisherIDTypes}

import scala.annotation.tailrec

import scodec.bits._


final class OpcUAPubSubParserDummy(config: OpcUAPubSubConfig) {

  def thunk(byte: Byte, elem: Int): String = {
    val pow2 = Math.pow(2, elem - 1).toInt
    if ((pow2 & byte) == 0) "0"
    else "1"
  }

  def bitValueAsString(byte: Byte, range: Range = 1 to 8): String = {
    new Character(bitValueAsInt(byte, range).toChar).toString
  }

  def bitValueAsInt(byte: Byte, range: Range = 1 to 8): Int = {
    Integer.parseInt(range
      .map(elem => thunk(byte, elem))
      .reverse
      .fold("")(_ ++ _),2
    )
  }

  def bitValueAsBoolean(byte: Byte, bytePosition: Byte): Boolean = {
    val pow2 = Math.pow(2, bytePosition - 1).toInt
    if ((pow2 & byte) == 0) false
    else true
  }

  def parseNetworkMessageHeader(bytes: Seq[Byte], parsePosition: Int = 0): Unit = {

    // ********** ********** Byte 1 ********** ********** //
    // Bit range 0-3: Version of the UADP NetworkMessage
    val uadpVersion = bitValueAsInt(bytes.head,1 to 4)
    // Bit range 4-7 contains the flags
    val publisherIdEnabled = bitValueAsBoolean(bytes.head, 5)
    val groupHeaderEnabled = bitValueAsBoolean(bytes.head, 6)
    val payloadHeaderEnabled = bitValueAsBoolean(bytes.head, 7)
    val extendedFlags1Enabled = bitValueAsBoolean(bytes.head, 8)

    // ********** ********** Byte 2 ********** ********** //
    // Contains the ExtendedFlags1 booleans if extendedFlags1Enabled from Byte1 is enabled
    if (extendedFlags1Enabled) {
      // Bit range 0-2 is the PublisherIdType
      val publisherIdType = bitValueAsInt(bytes(1),1 to 3) match {
        case 0 => PublisherIDTypes.UByte
        case 1 => PublisherIDTypes.UInt16
        case 2 => PublisherIDTypes.UInt32
        case 3 => PublisherIDTypes.UInt64
        case 4 => PublisherIDTypes.String
        case _ => PublisherIDTypes.UByte  // This is the default
      }
      val dataSetClassIDEnabled = bitValueAsBoolean(bytes(1), 4)
      val securityEnabled = bitValueAsBoolean(bytes(1), 5)
      val timeStampEnabled = bitValueAsBoolean(bytes(1), 6)
      val picoSecondsEnabled = bitValueAsBoolean(bytes(1), 7)
      val extendedFlags2Enabled = bitValueAsBoolean(bytes(1), 8)

      println(s"publisherIdType $publisherIdType")
      println(s"publisherIdType $dataSetClassIDEnabled")
      println(s"securityEnabled $securityEnabled")
      println(s"timeStampEnabled $timeStampEnabled")
      println(s"picoSecondsEnabled $picoSecondsEnabled")
      println(s"extendedFlags2Enabled $extendedFlags2Enabled")

      // ********** ********** Byte 3 ********** ********** //
      // Contains the ExtendedFlags2 booleans if extendedFlags1Enabled from Byte2 is enabled
      if (extendedFlags2Enabled) {
        val isChunkMessage = bitValueAsBoolean(bytes(2), 1)
        val promotedFieldsEnabled = bitValueAsBoolean(bytes(2), 2)
        val networkMessageType = bitValueAsInt(bytes(1),3 to 5) match {
          case 0 => NetworkMessageTypes.DataSetMessageType
          case 1 => NetworkMessageTypes.DiscoveryRequestType
          case 2 => NetworkMessageTypes.DiscoveryResponseType
          case _ => NetworkMessageTypes.DataSetMessageType     // This seems to be the default
        }
      } else {
        val networkMessageType = NetworkMessageTypes.DataSetMessageType
      }

      // ********** ********** Byte 4 to Byte N ********** ********** //
      // Contains the PublisherID (Shall be omitted if bit 4 of the UADPFlags is false)
      @tailrec // TODO: Alternatively get the length of the known PublisherID
      def probePublisherId(bytes: Seq[Byte],
        bytePosition: Int = 0,
        somePubId: Option[String] = None): (Int, Option[String]) = config.publisherIds match {
        case Nil => (bytePosition, somePubId)
        case x :: xs if somePubId.isEmpty =>
          if (x == bitValueAsString(bytes(bytePosition)))
            probePublisherId(bytes, bytePosition, somePubId)
          else
            probePublisherId(bytes.drop(bytePosition), bytePosition + 1, somePubId)
        case _ if somePubId.isDefined => (bytePosition, somePubId)
      }
      val publisherID = if (publisherIdEnabled) {
        probePublisherId(bytes.drop(3))
      } else None
    }

    println(s"uadpVersion $uadpVersion")
    println(s"publisherIdEnabled $publisherIdEnabled")
    println(s"groupHeaderEnabled $groupHeaderEnabled")
    println(s"payloadHeaderEnabled $payloadHeaderEnabled")
    println(s"extendedFlags1Enabled $extendedFlags1Enabled")

  }
}
object OpcUAPubSubParserDummy {

  def apply(opcUAPubSubConfig: OpcUAPubSubConfig) =
    new OpcUAPubSubParserDummy(opcUAPubSubConfig)
}