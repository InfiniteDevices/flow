package com.eon.opcuapubsubclient.parser.datasetmessage

import com.eon.opcuapubsubclient.cache._
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetFieldEncodings.{RawFieldEncoding, ReservedFieldEncoding, ValueFieldEncoding, VariantFieldEncoding}
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageFrame.DataSetMessageKeyFrame
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMessageHeader
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import com.eon.opcuapubsubclient.parser.ParserUtils
import scodec.bits.ByteVector

// TODO: Implementation pending
object DataSetMessageKeyFrameParser extends (ByteVector => DataSetMessageHeader => ParsePosition => (DataSetMessageKeyFrame, ParsePosition)) {

  override def apply(v1: ByteVector): DataSetMessageHeader => ParsePosition => (DataSetMessageKeyFrame, ParsePosition) =
    dataSetMsgHeader => parsePosition => (DataSetMessageKeyFrame(), parsePosition)

  def parseDataSetMessageKeyFrame(byteVector: ByteVector, dataSetMsgHeader: DataSetMessageHeader, parsePosition: ParsePosition) = {
    /*
      Look at Page OPC UA PubSub Spec version 1.04, Part 14, Page 71, Table 82
      The FieldCount shall be omitted if RawData field encoding is set in the EncodingFlags defined in 7.2.2.3.4.
     */
    val (fieldCount, pos1) = dataSetMsgHeader.dataSetFlags1.dataSetFieldEncoding match {
      case RawFieldEncoding =>
        (None, parsePosition)
      case _ =>
        val (fldCnt, pos) = ParserUtils.parseUInt16(byteVector, parsePosition)
        (Some(fldCnt), pos)
    }

    // Depending on the field encoding, we need to parse the payload
    dataSetMsgHeader.dataSetFlags1.dataSetFieldEncoding match {
      case VariantFieldEncoding =>
        parseVariant
      case RawFieldEncoding =>
        // Here we know that parsing needs a DataSetMetaData, so let us get it from the Cache
        // TODO: The key to look up the Map should also contain the PublisherId, Right now it is not yet injected
        // val key = s"$publisherId-${dataSetMsgHeader.configVersion.majorVersion}-${dataSetMsgHeader.configVersion.minorVersion}"
        val key = s"${dataSetMsgHeader.configVersion.majorVersion}-${dataSetMsgHeader.configVersion.minorVersion}"
        val dataSetMetaData = SimpleDataSetMetaDataCache.get(key)
        parseRawFields(dataSetMetaData, dataSetMetaData.fields.length, byteVector, parsePosition)
      case ValueFieldEncoding =>
      case ReservedFieldEncoding =>

    }

    // TODO: PublisherId need to be injected into the class so that we can do a look up for it!

    // Get the DataSetMetaData from the cache for the given PublisherId and ConfigVersion

    // Iterate over the fieldCount

    // Get the FieldMetaData for the given fieldCount

    // Get the StructureDataType from the DataSetMetaData and iterate over it

    //
  }
}
