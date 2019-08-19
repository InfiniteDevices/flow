package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.{FieldMetaData, QualifiedName}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

import scala.annotation.tailrec

// TODO: Implementation pending
object FieldMetaDataParser extends (ByteVector => Int => ParsePosition => (Vector[FieldMetaData], ParsePosition)) {

  override def apply(byteVector: ByteVector): Int => ParsePosition => (Vector[FieldMetaData], ParsePosition) =
    size => parsePosition => parseFieldMetaData(byteVector, size, parsePosition)

  def parseFieldMetaData(byteVector: ByteVector, size: Int, from: ParsePosition): (Vector[FieldMetaData], ParsePosition) = {

    @tailrec
    def fieldMetaData(size: Int, pos: ParsePosition, acc: Vector[FieldMetaData]): (Vector[FieldMetaData], ParsePosition) = {
      if (size > 0) {
        val (name, pos1) = ParserUtils.parseString(byteVector, pos)

        val (description, pos2) = ParserUtils.parseLocalizedText(byteVector, pos1)

        // FieldFlags
        val (optionSet, pos3) = OptionSetParser(byteVector)(pos2)

        val (builtInType, pos4) = ParserUtils.parseUByte(byteVector, pos3)
        val (dataType, pos5) = NodeIdParser(byteVector)(pos4)
        val (valueRank, pos6) = ParserUtils.parseUInt32(byteVector, pos5)
        val (arrayDimensions, pos7) = ParserUtils.parseUInt32(byteVector, pos6)
        val (maxStringLength, pos8) = ParserUtils.parseUInt32(byteVector, pos7)
        val (dataSetFieldId, pos9) = ParserUtils.parseGuid(byteVector, pos8) // TODO: This is wrong! Fix this!

        val (kvProperties, pos10) = ParserUtils.parseKeyValueProperties(byteVector, pos9)

        // TODO: Parser is error free until here!

        // TODO... rest of the fields!

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
          kvProperties
        )

        fieldMetaData(size - 1, pos10, acc :+ data)
      } else (acc, pos)
    }
/*
    def qualifiedNames(size: Int, pos: ParsePosition, acc: Vector[QualifiedName]): (Vector[QualifiedName], ParsePosition) = {
      if (size > 0) {
        val (qName, nPos) = ParserUtils.parseQualifiedName(byteVector, pos)
        qualifiedNames(size - 1, nPos, acc :+ qName)
      } else (acc, pos)
    } */

    fieldMetaData(size, from, Vector.empty)
  }
}
