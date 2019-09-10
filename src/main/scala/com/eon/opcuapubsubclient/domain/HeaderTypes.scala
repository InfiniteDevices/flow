package com.eon.opcuapubsubclient.domain

import java.util.UUID

import com.eon.opcuapubsubclient.domain.CommonTypes._
import com.eon.opcuapubsubclient.domain.PayloadTypes._
import julienrf.json.derived
import org.joda.time.DateTime
import play.api.libs.json.OFormat

object HeaderTypes {

  object PublisherIDTypes {
    sealed abstract class PublisherIDType(val int: Int)
    case object UByte extends PublisherIDType(int = 0) // 000 in binary
    case object UInt16 extends PublisherIDType(int = 1) // 001 in binary
    case object UInt32 extends PublisherIDType(int = 2) // 010 in binary
    case object UInt64 extends PublisherIDType(int = 3) // 011 in binary
    case object String extends PublisherIDType(int = 4) // 100 in binary
  }

  object NetworkMessageTypes {
    sealed abstract class NetworkMessageType(val int: Int)
    case object DataSetMessageType extends NetworkMessageType(int = 0) // 000 in binary (this is the default)
    case object DiscoveryRequestType extends NetworkMessageType(int = 1) // 001 in binary
    case object DiscoveryResponseType extends NetworkMessageType(int = 2) // 010 in binary
  }

  object PayloadTypes {
    sealed trait PayloadType
    case object PayloadType {
      case object DataSetMessage extends PayloadType
      case object DiscoveryRequest extends PayloadType
      case object DiscoveryResponse extends PayloadType
    }
  }

  case class NetworkMessageHeader(
    version: Int,
    publisherIdEnabled: Boolean = false,
    groupHeaderEnabled: Boolean = false,
    payloadHeaderEnabled: Boolean = false,
    extendedFlags1Enabled: Boolean = false,
    extendedFlags1: ExtendedFlags1 = ExtendedFlags1(),
    extendedFlags2: ExtendedFlags2 = ExtendedFlags2(),
    publisherId: Option[String] = None,
    dataSetClassId: Option[UUID] = None) {
    override def toString: String = {
      s"""
         |version               [bit 0-3] = $version
         |publisherIdEnabled    [bit 4]   = $publisherIdEnabled
         |groupHeaderEnabled    [bit 5]   = $groupHeaderEnabled
         |payloadHeaderEnabled  [bit 6]   = $payloadHeaderEnabled
         |extendedFlags1Enabled [bit 7]   = $extendedFlags1Enabled
         |extendedFlags1 = $extendedFlags1
         |extendedFlags2 = $extendedFlags2
         |publisherId = $publisherId
         |dataSetClassId = $dataSetClassId
       """.stripMargin
    }
  }

  case class ExtendedFlags1(
    publisherIdType: PublisherIDTypes.PublisherIDType = PublisherIDTypes.UByte,
    dataSetClassIDEnabled: Boolean = false,
    securityEnabled: Boolean = false,
    timeStampEnabled: Boolean = false,
    picoSecondsEnabled: Boolean = false,
    extendedFlags2Enabled: Boolean = false) {
    override def toString: String = {
      s"""
         |publisherIdType       [bit 0-2] = $publisherIdType
         |dataSetClassIDEnabled [bit 3]   = $dataSetClassIDEnabled
         |securityEnabled       [bit 4]   = $securityEnabled
         |timeStampEnabled      [bit 5]   = $timeStampEnabled
         |picoSecondsEnabled    [bit 6]   = $picoSecondsEnabled
         |extendedFlags2Enabled [bit 7]   = $extendedFlags2Enabled
       """.stripMargin
    }
  }

  case class ExtendedFlags2(
    isChunkMessage: Boolean = false,
    promotedFieldsEnabled: Boolean = false,
    networkMessageType: NetworkMessageTypes.NetworkMessageType = NetworkMessageTypes.DataSetMessageType) {
    override def toString: String = {
      s"""
         |isChunkMessage        [bit 0]   = $isChunkMessage
         |promotedFieldsEnabled [bit 1]   = $promotedFieldsEnabled
         |networkMessageType    [bit 2-4] = $networkMessageType
       """.stripMargin
    }
  }

  case class GroupHeader(
    writerGroupIdEnabled: Boolean = false,
    groupVersionEnabled: Boolean = false,
    networkMessageNumberEnabled: Boolean = false,
    sequenceNumberEnabled: Boolean = false,
    writerGroupId: Option[Int] = None,
    groupVersion: Option[Int] = None,
    networkMessageNumber: Option[Int] = None,
    sequenceNumber: Option[Int] = None)

  /**
   * The payload header depends on the UADP NetworkMessage Type flags defined in the ExtendedFlags2 bit range 2-4.
   * The default is DataSetMessage if the ExtendedFlags2 field is not enabled.
   * The PayloadHeader shall be omitted if bit 6 of the UADPFlags is false.
   * The PayloadHeader is not contained in the payload but it is contained in the unencrypted NetworkMessage
   * header since it contains information necessary to filter DataSetMessages on the Subscriber side.
   */
  sealed trait PayloadHeader
  object PayloadHeader {
    case class InvalidPayloadHeader(
      msg: String) extends PayloadHeader

    case class DataSetMessagePayloadHeader(
      messageCount: Int = 0,
      dataSetWriterIds: Vector[Int] = Vector.empty) extends PayloadHeader

    case class DiscoveryResponseMessagePayloadHeader(
      responseType: DiscoveryResponseMessageTypes.DiscoveryResponseMessageType,
      sequenceNumber: Int) extends PayloadHeader

    case class DiscoveryRequestMessagePayloadHeader(
      informationType: DiscoveryRequestMessageTypes.DiscoveryRequestMessageType,
      dataSetWriterIds: Vector[Int] = Vector.empty) extends PayloadHeader
  }

  case class ExtendedNetworkMessageHeader(
    timeStamp: Option[DateTime] = None,
    picoSeconds: Option[Int] = None,
    promotedFields: Vector[PromotedField] = Vector.empty[PromotedField])

  case class PromotedField(
    size: Int,
    fields: String // TODO: This should be mapped to the BaseDataType.... Figure out what this is?????
  )

  case class SecurityHeader(
    networkMessageSigned: Boolean = false,
    networkMessageEncrypted: Boolean = false,
    securityFooterEnabled: Boolean = false,
    forceKeyReset: Boolean = false,
    securityTokenId: Int = 0,
    nonceLength: Byte = 0,
    messageNonce: Byte = 0,
    securityFooterSize: Int = 0)
  object SecurityHeader {
    implicit val jsonFormat: OFormat[SecurityHeader] = derived.oformat[SecurityHeader]()
  }

  sealed trait ExtensionObjectEncoding
  case class ByteStringEncoding(bytes: Vector[Byte]) extends ExtensionObjectEncoding
  case class XmlElementEncoding(xmlElement: String) extends ExtensionObjectEncoding
  object ExtensionObjectEncoding {
    implicit val jsonFormat: OFormat[ExtensionObjectEncoding] = derived.oformat[ExtensionObjectEncoding]()
  }

  case class ExtensionObject(
    encodingTypeId: NodeId,
    encodedBody: ExtensionObjectEncoding)
  object ExtensionObject {
    implicit val jsonFormat: OFormat[ExtensionObject] = derived.oformat[ExtensionObject]()
  }

  case class DataValue(
    value: Variant,
    status: StatusCode,
    sourceTime: Long, // TODO: Need to be a Datetime type
    sourcePicoseconds: Int,
    serverTime: Long, // TODO: Need to be a Datetime type
    serverPicoseconds: Int)
  object DataValue {
    implicit val jsonFormat: OFormat[DataValue] = derived.oformat[DataValue]()
  }

  case class KeyValueProperty(
    qName: QualifiedName,
    value: Variant)
  object KeyValueProperty {
    implicit val jsonFormat: OFormat[KeyValueProperty] = derived.oformat[KeyValueProperty]()
  }

  case class ConfigVersion(
    majorVersion: Long,
    minorVersion: Long)
  object ConfigVersion {
    implicit val jsonFormat: OFormat[ConfigVersion] = derived.oformat[ConfigVersion]()
  }
}
