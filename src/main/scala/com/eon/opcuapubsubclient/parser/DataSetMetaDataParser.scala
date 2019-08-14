package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient._
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.DataSetMetaData
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector


object DataSetMetaDataParser extends (ByteVector => ParsePosition => V[(Int, ParsePosition)]) {

  override def apply(byteVector: ByteVector): ParsePosition => V[(Int, ParsePosition)] =
    parsePosition => validated { parseDataSetMetaData(byteVector, parsePosition) }

  def parseDataSetMetaData(byteVector: ByteVector, parsePosition: ParsePosition): (ParsePosition, ParsePosition) = {
    // 1. Parse the DataSetWriterId
    val (dataSetWriterId, pos1) =
      (ParserUtils.sliceToUInt(byteVector, parsePosition, parsePosition + 2), parsePosition + 2)

    // 2. Parse the DataTypeSchemaHeader
    val (schemaHeader, pos2) = DataTypeSchemaHeaderParser(byteVector)(pos1)

    // 3. Parse the name and description
    val (name, pos3) = ParserUtils.parseStringWithPosition(byteVector, pos2)
    val (description, pos4) = ParserUtils.parseLocalizedText(byteVector, pos3)

    // 4. Parse the size if the Fields array (Array length is encoded as Int32 or 4 bytes)
    val (fieldMetaDataSize, pos5) = (ParserUtils.sliceToInt(byteVector, pos4, pos4 + 4), pos4 + 4)

    val (fieldMetaDataVector, pos6) = FieldMetaDataParser(byteVector)(fieldMetaDataSize)(pos5)

    // TODO: Bug free until here!

    //val (fields, pos5) = FieldMetaDataParser(byteVector)(fieldMetaDataSize)(pos5)
    //val (dataSetClassId, pos6) = (ParserUtils.parseGuid(byteVector, pos5), pos5+ 16)
    //val (configVersion, pos7) = ParseConfigVersion(.... TODO ....)
    /*
    (DataSetMetaData(
      schemaHeader,
      name,
      description,
      fields,
      dataSetClassId,
      configVersion
    ), pos7) */

    // TODO
    (pos1, pos1)
  }
}
