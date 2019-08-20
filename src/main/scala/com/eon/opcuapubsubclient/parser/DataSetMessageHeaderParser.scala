package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient._
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.DataSetFieldEncodings.DataSetFieldEncoding._
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.DataSetMessageTypes.DataSetMessageType.{DeltaFrame, Event, KeepAlive, KeyFrame}
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.{DataSetFlags1, DataSetFlags2, DataSetMessageHeader}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.{BitVector, ByteVector}


object DataSetMessageHeaderParser extends (ByteVector => ParsePosition => (DataSetMessageHeader, ParsePosition)) {

  override def apply(byteVector: ByteVector): ParsePosition => (DataSetMessageHeader, ParsePosition) = parsePosition => {
    val (dsFlags1, pos1) = dataSetFlag1(BitVector(byteVector(parsePosition)), parsePosition)
    val (dsFlags2, pos2) = dataSetFlag2(BitVector(byteVector(parsePosition)), pos1, dsFlags1.dataSetFlags2Enabled)
    val (seqNr, pos3) = dataSetMsgSequenceNumber(byteVector, pos2, dsFlags1.dataSetMsgSeqNrEnabled)
    val (timeStamp, pos4) = dsFlags2.tsEnabled.toOption(ParserUtils.parseInt64, byteVector, pos3)
    val (picoSeconds, pos5) = dsFlags2.picoSecondsIncluded.toOption(ParserUtils.parseUInt16, byteVector, pos4)
    val (status, pos6) = dsFlags1.statusEnabled.toOption(ParserUtils.parseUInt16, byteVector, pos5)
    val (majorCfgVersion, pos7) = dsFlags1.cfgMajorVersionEnabled.toOption(ParserUtils.parseVersionTime, byteVector, pos6)
    val (minorCfgVersion, pos8) = dsFlags1.cfgMajorVersionEnabled.toOption(ParserUtils.parseVersionTime, byteVector, pos7)

    (DataSetMessageHeader(
      dsFlags1,
      dsFlags2,
      seqNr,
      timeStamp,
      picoSeconds,
      status,
      majorCfgVersion,
      minorCfgVersion
    ), pos8)
  }

  def dataSetFlag1(bitV: BitVector, from: ParsePosition): (DataSetFlags1, ParsePosition) = {
    val bitVector = bitV.reverse
    val fieldEncoding = bitVector.slice(from = 1, until = 3).toInt(signed = false) match {
      case 0 => VariantFieldEncoding
      case 1 => RawFieldEncoding
      case 2 => ValueFieldEncoding
      case 3 => ReservedFieldEncoding
    }
    (DataSetFlags1(
      dataSetMessageValid = bitVector(0),
      fieldEncoding,
      dataSetMsgSeqNrEnabled = bitVector(3),
      statusEnabled = bitVector(4),
      cfgMajorVersionEnabled = bitVector(5),
      cfgMinorVersionEnabled = bitVector(6),
      dataSetFlags2Enabled = bitVector(7)
    ), from + 1)
  }

  /**
    * TODO: Understand the meaning in the spec!
    * Spec on Page 70 says that the DataSetFlags2 shall be omitted if bit 7 of the DataSetFlags1 is false
    * If the field is omitted, the Subscriber shall handle the related bits as false
    * Asked this as a question in the OPC Discussion Forum!
    * https://opcfoundation.org/forum/opc-ua-standard/datasetmessage-header-parsing-understanding-the-meaning-from-spec/#p1930
    */
  def dataSetFlag2(bitV: BitVector, from: ParsePosition, isEnabled: Boolean): (DataSetFlags2, ParsePosition) = {
    if (isEnabled) {
      val bitVector = bitV.reverse
      val dataSetMsgTyp = bitVector.slice(from = 0, until = 4).toInt(signed = false) match {
        case 0 => KeyFrame
        case 1 => DeltaFrame
        case 2 => Event
        case 3 => KeepAlive
      }
      (DataSetFlags2(
        dataSetMsgTyp,
        tsEnabled = bitVector(4),
        picoSecondsIncluded = bitVector(5)
      ), from + 1)
    } else (DataSetFlags2(), from) // Use the default as per the Spec!
  }

  // TODO: There is some logic defined in Page 80, Table 81 of the Spec., It is yet to be implemented here!
  def dataSetMsgSequenceNumber(byteVector: ByteVector, from: ParsePosition, isEnabled: Boolean): (Option[Int], ParsePosition) = {
    isEnabled.toOption(ParserUtils.parseUInt16, byteVector, from)
  }
}
