package com.eon.opcuapubsubclient.domain

import java.util.UUID

import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.DataSetFieldEncodings.DataSetFieldEncoding
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.DataSetMessageTypes.DataSetMessageType
import org.joda.time.DateTime


object OpcUAPubSubTypes {

  object PublisherIDTypes {
    sealed abstract class PublisherIDType(val int: Int)
    case object UByte  extends PublisherIDType(int = 0) // 000 in binary
    case object UInt16 extends PublisherIDType(int = 1) // 001 in binary
    case object UInt32 extends PublisherIDType(int = 2) // 010 in binary
    case object UInt64 extends PublisherIDType(int = 3) // 011 in binary
    case object String extends PublisherIDType(int = 4) // 100 in binary
  }

  object NetworkMessageTypes {
    sealed abstract class NetworkMessageType(val int: Int)
    case object DataSetMessageType    extends NetworkMessageType(int = 0) // 000 in binary (this is the default)
    case object DiscoveryRequestType  extends NetworkMessageType(int = 1) // 001 in binary
    case object DiscoveryResponseType extends NetworkMessageType(int = 2) // 010 in binary
  }

  object DiscoveryResponseMessageTypes {
    sealed abstract class DiscoveryResponseMessageType(val int: Int)
    case object Reserved extends DiscoveryResponseMessageType(int = 0)
    case object PublisherEndPoint extends DiscoveryResponseMessageType(int = 1)
    case object DataSetMetaData extends DiscoveryResponseMessageType(int = 2)
    case object DataSetWriterConfig extends DiscoveryResponseMessageType(int = 3)
  }

  object DiscoveryRequestMessageTypes {
    sealed abstract class DiscoveryRequestMessageType(val int: Int)
    case object Reserved extends DiscoveryRequestMessageType(int = 0)
    case object PublisherServer extends DiscoveryRequestMessageType(int = 1)
    case object DataSetMetaData extends DiscoveryRequestMessageType(int = 2)
    case object DataSetWriterConfig extends DiscoveryRequestMessageType(int = 3)
  }

  object PayloadTypes {
    sealed trait PayloadType
    case object PayloadType {
      case object DataSetMessage    extends PayloadType
      case object DiscoveryRequest  extends PayloadType
      case object DiscoveryResponse extends PayloadType
    }
  }

  object DataSetMessageTypes {
    sealed trait DataSetMessageType
    case object DataSetMessageType {
      case object KeyFrame   extends DataSetMessageType
      case object DeltaFrame extends DataSetMessageType
      case object Event      extends DataSetMessageType
      case object KeepAlive  extends DataSetMessageType
    }
  }

  object DataSetFieldEncodings {
    sealed trait DataSetFieldEncoding
    case object DataSetFieldEncoding {
      case object Variant extends DataSetFieldEncoding
      case object Raw     extends DataSetFieldEncoding
      case object Value   extends DataSetFieldEncoding
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
    dataSetClassId: Option[UUID] = None
  ) {
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
    extendedFlags2Enabled: Boolean = false
  ) {
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
    networkMessageType: NetworkMessageTypes.NetworkMessageType = NetworkMessageTypes.DataSetMessageType
  ) {
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
    sequenceNumber: Option[Int] = None
  )

  /**
    * The payload header depends on the UADP NetworkMessage Type flags defined in the ExtendedFlags2 bit range 0-3.
    * The default is DataSetMessage if the ExtendedFlags2 field is not enabled.
    * The PayloadHeader shall be omitted if bit 6 of the UADPFlags is false.
    * The PayloadHeader is not contained in the payload but it is contained in the unencrypted NetworkMessage
    * header since it contains information necessary to filter DataSetMessages on the Subscriber side.
    */
  sealed trait PayloadHeader
  object PayloadHeader {
    case class InvalidPayloadHeader(
      msg: String
    ) extends PayloadHeader

    case class DataSetPayloadHeader(
      messageCount: Int = 0,
      dataSetWriterIds: Vector[Int] = Vector.empty
    ) extends PayloadHeader

    case class DiscoveryResponsePayloadHeader(
      responseType: DiscoveryResponseMessageTypes.DiscoveryResponseMessageType,
      sequenceNumber: Int
    ) extends PayloadHeader

    case class DiscoveryRequestPayloadHeader(
      informationType: DiscoveryRequestMessageTypes.DiscoveryRequestMessageType,
      dataSetWriterIds: Vector[Int] = Vector.empty
    ) extends PayloadHeader
  }

  case class ExtendedNetworkMessageHeader(
    timeStamp: Option[DateTime] = None,
    picoSeconds: Option[Int] = None,
    promotedFields: Vector[PromotedField] = Vector.empty[PromotedField]
  )

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
    securityFooterSize: Int = 0
  )

  // ******************************************* DataSetMetaData **************************************************** //

  sealed trait ValueRank { def value: Int }
  case object ValueRank {
    case object OneDimension extends ValueRank {val value: Int = 1}
    case object OneOrMoreDimensions extends ValueRank {val value: Int = 0}
    case object Scalar extends ValueRank {val value: Int = -1}
    case object Any extends ValueRank {val value: Int = -2}
    case object ScalarOrOneDimension extends ValueRank {val value: Int = -3}
  }

  sealed trait StructureType
  case object StructureType {
    case object Simple extends StructureType
    case object OptionalFields extends StructureType
    case object Union extends StructureType
  }

  case class DataSetMetaData(
    dataTypeSchemaHeader: DataTypeSchemaHeader,
    name: String,
    description: String,
    fields: Seq[FieldMetaData],
    dataSetClassId: UUID,
    configurationVersion: Int // TODO: Should this be a VersionType field
  )

  case class DataTypeSchemaHeader(
    namespaces: Vector[String] = Vector.empty,
    structureDataTypes: Vector[StructureDescription] = Vector.empty,
    enumDataTypes: Vector[EnumDescription] = Vector.empty,
    simpleDataTypes: Vector[SimpleTypeDescription] = Vector.empty
  )

  // TODO: Fix me with proper definition of the fields
  case class EnumDescription()
  // TODO: Fix me with proper definition of the fields
  case class SimpleTypeDescription()

  case class StructureDescription(
    dataTypeId: NodeId,
    name: QualifiedName,
    structureDefinition: StructureDefinition
  )

  case class StructureDefinition(
    defaultEncodingId: NodeId,
    baseDataType: NodeId,
    structureType: StructureType,
    fields: Vector[StructureField]
  )

  case class StructureField(
    name: String,
    description: LocalizedText,
    dataType: NodeId,
    valueRank: Int,
    arrayDimensions: Int,
    maxStringLength: Int,
    sOptional: Boolean
  )

  case class LocalizedText(
    locale: Option[String] = None,
    text: Option[String] = None
  )
  case class QualifiedName(nameSpaceIndex: Int, name: String)

  sealed trait NodeIdIdentifier
  case object NodeIdIdentifier {
    case class NumericTwoByteIdentifier(value: Byte) extends NodeIdIdentifier
    case class NumericFourByteIdentifier(value: Short) extends NodeIdIdentifier
    case class NumericIdentifier(value: Int) extends NodeIdIdentifier
    case class StringIdentifier(value: String) extends NodeIdIdentifier
    case class GuidIdentifier(value: UUID) extends NodeIdIdentifier
    case class OpaqueIdentifier(value: Vector[Byte]) extends NodeIdIdentifier
    case object UnknownIdentifier extends NodeIdIdentifier
  }
  case class NodeId(
    namespaceIndex: Short,
    identifier: NodeIdIdentifier,
  )

  case class FieldMetaData(
    name: String,
    description: LocalizedText,
    optionSet: OptionSet,
    builtInType: Int,
    dataType: NodeId,
    valueRank: Int,
    arrayDimensions: Int,
    maxStringLength: Int,
    dataSetFieldId: UUID,
    properties: Vector[(QualifiedName, Variant)]
  )

  case class OptionSet(
    value: Vector[Byte],
    validBits: Vector[Byte]
  )

  case class Variant(any: BuiltInType)

  // ******************************************* DataSetMetaData **************************************************** //

  // ******************************************* DataSetMessage  **************************************************** //
  case class Payload(
    sizes: Seq[Int],
    dataSetMessages: Seq[DataSetMessage]
  )

  case class DataSetMessage(
    dataSetFlags1: DataSetFlags1,
    dataSetFlags2: DataSetFlags2,
    messageSequenceNumber: Int,
    timeStamp: DateTime,
    picoSeconds: Int,
    statusCode: Int,
    majorVersion: Int,
    minorVersion: Int
  )

  case class DataSetFlags1(
    dataSetMessageValid: Boolean = false,
    dataSetFieldEncoding: DataSetFieldEncoding,
    dataSetMessageSequenceNumber: Boolean = false,
    statusEnabled: Boolean = false,
    configMajorVersionEnabled: Boolean = false,
    configMinorVersionEnabled: Boolean = false,
    dataSetFlags2Enabled: Boolean = false
  )
  case class DataSetFlags2(
    dataSetMessageType: DataSetMessageType,
    timeStampEnabled: Boolean = false,
    picoSecondsIncluded: Boolean = false
  )

  // ******************************************* DataSetMessage  **************************************************** //

  // ******************************************* BuiltInTypes  ****************************************************** //

  sealed trait BuiltInType
  object BuiltInType {
    case object BooleanType extends BuiltInType
    case object ByteType    extends BuiltInType
    case object UByteType   extends BuiltInType
    case object Int16Type   extends BuiltInType
    case object UInt16Type  extends BuiltInType
    case object Int32Type   extends BuiltInType
    case object UInt32Type  extends BuiltInType
    case object Int64Type   extends BuiltInType
    case object UInt64Type  extends BuiltInType
    case object FloatType   extends BuiltInType
    case object DoubleType  extends BuiltInType
    case object StringType  extends BuiltInType
    case object DateTimeType extends BuiltInType
    case object GuidType     extends BuiltInType
    case object ByteStringType extends BuiltInType
    case object XmlElementType extends BuiltInType
    case object NodeIdType     extends BuiltInType
    case object ExpandedNodeIdType extends BuiltInType
    case object StatusCodeType     extends BuiltInType
    case object QualifiedNameType  extends BuiltInType
    case object LocalizedTextType  extends BuiltInType
    case object ExtensionObjectType extends BuiltInType
    case object DataValueType       extends BuiltInType
    case object VariantType         extends BuiltInType
    case object DiagnosticInfoType  extends BuiltInType
    case object ZombieType          extends BuiltInType

    val builtInTypes: Map[Int, BuiltInType] = Map(
      0 -> ZombieType,
      1 -> ByteType,
      2 -> UByteType,
      3 -> Int16Type,
      4 -> UInt16Type,
      5 -> Int32Type,
      6 -> UInt32Type,
      7 -> Int64Type,
      8 -> UInt64Type,
      10 -> FloatType,
      11 -> DoubleType,
      12 -> DateTimeType,
      13 -> GuidType,
      14 -> ByteStringType,
      15 -> XmlElementType,
      16 -> NodeIdType,
      17 -> ExpandedNodeIdType,
      18 -> StatusCodeType,
      19 -> QualifiedNameType,
      20 -> LocalizedTextType,
      21 -> ExtensionObjectType,
      22 -> DataValueType,
      23 -> VariantType,
      24 -> BooleanType,
      25 -> DiagnosticInfoType
    )
  }
  // ******************************************* BuiltInTypes  ****************************************************** //
}
