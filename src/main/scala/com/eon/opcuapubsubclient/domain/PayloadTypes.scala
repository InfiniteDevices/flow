package com.eon.opcuapubsubclient.domain

import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetFieldEncodings.DataSetFieldEncoding
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageTypes.DataSetMessageType
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageTypes.DataSetMessageType.KeyFrame


object PayloadTypes {

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

  sealed trait Payload
  object Payload {
    case class DataSetMessagePayload(
      dataSetMessages: Vector[DataSetMessage] = Vector.empty
    ) extends Payload

    case class DiscoveryRequestPayload() extends Payload
    case class DiscoveryResponsePayload() extends Payload
    case class InvalidPayload() extends Payload
  }

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
    case class DataSetMessageKeepAlive() extends DataSetMessageFrame
  }

  case class DataSetMessage(
    dataSetMessageHeader: DataSetMessageHeader,
    messageFrame: DataSetMessageFrame
  )
}
