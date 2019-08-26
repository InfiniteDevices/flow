package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMetaData
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector


object DataSetMetaDataParser extends (ByteVector => ParsePosition => (DataSetMetaData, ParsePosition)) {

  override def apply(byteVector: ByteVector): ParsePosition => (DataSetMetaData, ParsePosition) =
    parsePosition => parseDataSetMetaData(byteVector, parsePosition)

  def parseDataSetMetaData(byteVector: ByteVector, parsePosition: ParsePosition): (DataSetMetaData, ParsePosition) = {
    // 1. Parse the DataTypeSchemaHeader
    val (schemaHeader, pos1) = DataTypeSchemaHeaderParser(byteVector)(parsePosition)

    // 2. Parse the name and description
    val (name, pos2) = ParserUtils.parseString(byteVector, pos1)
    val (description, pos3) = ParserUtils.parseLocalizedText(byteVector, pos2)

    // 3. Parse the size if the Fields array (Array length is encoded as Int32 or 4 bytes)
    val (fieldMetaDataSize, pos4) = ParserUtils.parseUInt32(byteVector, pos3)
    val (fieldMetaData, pos5) = FieldMetaDataParser(byteVector)(fieldMetaDataSize)(pos4)

    val (dataSetClassId, pos6) = ParserUtils.parseGuid(byteVector, pos5) // TODO: GUID is wrong Fix it!
    val (configVersion, pos7) = ParserUtils.parseConfigVersion(byteVector, pos6)

    (DataSetMetaData(
      schemaHeader,
      name,
      description,
      fieldMetaData,
      dataSetClassId,
      configVersion
    ), pos7)
  }
}
