package com.eon.opcuapubsubclient.domain

import java.util.UUID

import com.eon.opcuapubsubclient.domain.CommonTypes.{LocalizedText, NodeId, QualifiedName}
import com.eon.opcuapubsubclient.domain.HeaderTypes.KeyValueProperty
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetFieldEncodings.DataSetFieldEncoding
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageTypes.DataSetMessageType
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageTypes.DataSetMessageType.KeyFrame


object PayloadTypes {

  sealed trait Payload
  object Payload {
    case class DataSetMessagePayload(
      dataSetMessages: Vector[DataSetMessage] = Vector.empty
    ) extends Payload

    case class DiscoveryRequestPayload() extends Payload
    case class DiscoveryResponsePayload(
      dataSetMetaData: DataSetMetaData
    ) extends Payload
    case class InvalidPayload() extends Payload
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
      case object VariantFieldEncoding  extends DataSetFieldEncoding
      case object RawFieldEncoding      extends DataSetFieldEncoding
      case object ValueFieldEncoding    extends DataSetFieldEncoding
      case object ReservedFieldEncoding extends DataSetFieldEncoding
    }
  }

  // ******************************************* DataSetMessage  **************************************************** //

  case class DataSetFlags1(
    dataSetMessageValid: Boolean = false,
    dataSetFieldEncoding: DataSetFieldEncoding,
    dataSetMsgSeqNrEnabled: Boolean = false,
    statusEnabled: Boolean = false,
    cfgMajorVersionEnabled: Boolean = false,
    cfgMinorVersionEnabled: Boolean = false,
    dataSetFlags2Enabled: Boolean = false
  )
  case class DataSetFlags2(
    dataSetMessageType: DataSetMessageType = KeyFrame,
    tsEnabled: Boolean = false,
    picoSecondsIncluded: Boolean = false
  )

  case class DataSetMessageHeader(
    dataSetFlags1: DataSetFlags1,
    dataSetFlags2: DataSetFlags2,
    dataSetMsgSeqNr: Option[Int] = None,
    timeStamp: Option[Long] = None,
    picoSeconds: Option[Int] = None,
    status: Option[Int] = None,
    configMajorVersion: Option[Int] = None,
    configMinorVersion: Option[Int] = None
  )

  sealed trait DataSetMessageFrame
  object DataSetMessageFrame {
    case class DataSetMessageKeyFrame(

    ) extends DataSetMessageFrame
    case class DataSetMessageDeltaFrame() extends DataSetMessageFrame
    case class DataSetMessageEvent() extends DataSetMessageFrame
    case class DataSetMessageKeepAlive(
      nextSequenceNumber: Int
    ) extends DataSetMessageFrame
  }

  case class DataSetMessage(
    dataSetMessageHeader: DataSetMessageHeader,
    messageFrame: DataSetMessageFrame
  )
  // ******************************************* DataSetMessage  **************************************************** //

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
    properties: Vector[KeyValueProperty]
  )

  case class OptionSet(
    value: Vector[Byte],
    validBits: Vector[Byte]
  )

  // ******************************************* DataSetMetaData **************************************************** //
}
