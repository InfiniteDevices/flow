package com.eon.opcuapubsubclient.parser.datasetmetadata

import com.eon.opcuapubsubclient.domain.PayloadTypes.FieldMetaData
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import com.eon.opcuapubsubclient.parser.{ NodeIdParser, OptionSetParser, ParserUtils }
import scodec.bits.ByteVector

import scala.annotation.tailrec

// TODO: Testing pending
object FieldMetaDataParser extends (ByteVector => Int => ParsePosition => (Vector[FieldMetaData], ParsePosition)) {

  override def apply(byteVector: ByteVector): Int => ParsePosition => (Vector[FieldMetaData], ParsePosition) =
    size => parsePosition => parseFieldMetaData(byteVector, size, parsePosition)

  def parseFieldMetaData(byteVector: ByteVector, size: Int, from: ParsePosition): (Vector[FieldMetaData], ParsePosition) = {

    @tailrec
    def fieldMetaData(size: Int, pos: ParsePosition, acc: Vector[FieldMetaData]): (Vector[FieldMetaData], ParsePosition) = {
      if (size > 0) {
        val (name, pos1) = ParserUtils.parseString(byteVector, pos)
        val (description, pos2) = ParserUtils.parseLocalizedText(byteVector, pos1)

        // TODO: Check if this is this correct for the FieldFlags OptionSet!
        val (optionSet, pos3) = OptionSetParser(byteVector)(pos2)
        val (builtInType, pos4) = ParserUtils.parseUByte(byteVector, pos3)
        val (dataType, pos5) = NodeIdParser(byteVector)(pos4)
        val (valueRank, pos6) = ParserUtils.parseUInt32(byteVector, pos5)
        val (arrayDimensions, pos7) = ParserUtils.parseUInt32(byteVector, pos6)
        val (maxStringLength, pos8) = ParserUtils.parseUInt32(byteVector, pos7)
        val (dataSetFieldId, pos9) = ParserUtils.parseGuid(byteVector, pos8) // TODO: This is wrong! Fix this!
        val (kvProperties, pos10) = ParserUtils.parseKeyValueProperties(byteVector, pos9)

        val data = FieldMetaData(
          name,
          description,
          optionSet,
          builtInType,
          dataType,
          valueRank,
          arrayDimensions,
          maxStringLength,
          dataSetFieldId,
          kvProperties)

        fieldMetaData(size - 1, pos10, acc :+ data)
      } else (acc, pos)
    }
    fieldMetaData(size, from, Vector.empty)
  }
}
