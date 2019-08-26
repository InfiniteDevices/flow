package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMetaData
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector


object DataSetMetaDataParser extends (ByteVector => ParsePosition => (DataSetMetaData, ParsePosition)) {

  override def apply(byteVector: ByteVector): ParsePosition => (DataSetMetaData, ParsePosition) =
    parsePosition => parseDataSetMetaData(byteVector, parsePosition)

  def parseDataSetMetaData(byteVector: ByteVector, parsePosition: ParsePosition): (DataSetMetaData, ParsePosition) = {
    // 1. Parse the DataSetWriterId
    val (dataSetWriterId, pos1) = ParserUtils.parseUInt16(byteVector, parsePosition) // TODO: Somehow this need to be returned to the caller!

    // 2. Parse the DataTypeSchemaHeader
    val (schemaHeader, pos2) = DataTypeSchemaHeaderParser(byteVector)(pos1)

    // 3. Parse the name and description
    val (name, pos3) = ParserUtils.parseString(byteVector, pos2)
    val (description, pos4) = ParserUtils.parseLocalizedText(byteVector, pos3)

    // 4. Parse the size if the Fields array (Array length is encoded as Int32 or 4 bytes)
    val (fieldMetaDataSize, pos5) = ParserUtils.parseUInt32(byteVector, pos4)
    val (fieldMetaData, pos6) = FieldMetaDataParser(byteVector)(fieldMetaDataSize)(pos5)

    val (dataSetClassId, pos7) = ParserUtils.parseGuid(byteVector, pos6) // TODO: GUID is wrong Fix it!
    val (configVersion, pos8) = ParserUtils.parseConfigVersion(byteVector, pos7)
    val (status, pos9) = ParserUtils.parseStatusCode(byteVector, pos8)

    (DataSetMetaData(
      dataSetWriterId,
      schemaHeader,
      name,
      description,
      fieldMetaData,
      dataSetClassId,
      configVersion,
      status
    ), pos9)
  }
}
