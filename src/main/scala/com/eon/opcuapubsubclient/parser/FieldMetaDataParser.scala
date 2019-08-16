package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.{FieldMetaData, QualifiedName}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

import scala.annotation.tailrec

// TODO: Implementation pending
object FieldMetaDataParser extends (ByteVector => Int => ParsePosition => (Vector[FieldMetaData], ParsePosition)) {

  override def apply(byteVector: ByteVector): Int => ParsePosition => (Vector[FieldMetaData], ParsePosition) =
    size => parsePosition => parseFieldMetaData(byteVector, size, parsePosition)

  def parseFieldMetaData(byteVector: ByteVector, size: Int, parsePosition: ParsePosition): (Vector[FieldMetaData], ParsePosition) = {

    @tailrec
    def fieldMetaData(size: Int, pos: ParsePosition, acc: Vector[FieldMetaData]): (Vector[FieldMetaData], ParsePosition) = {
      if (size > 0) {
        val (name, pos1) = ParserUtils.parseString(byteVector, parsePosition)

        val (description, pos2) = ParserUtils.parseLocalizedText(byteVector, pos1)

        // FieldFlags
        val (optionSet, pos3) = OptionSetParser(byteVector)(pos2)

        val (builtInType, pos4) = (ParserUtils.sliceToUInt(byteVector, pos3, pos3 + 1), pos3 + 1)
        val (dataType, pos5) = NodeIdParser(byteVector)(pos4)
        val (valueRank, pos6) = (ParserUtils.sliceToUInt(byteVector, pos5, pos5 + 4), pos5 + 4)
        // TODO: ArrayDimensions is of type Array[UInt32]... what we do here is wrong!!!
        val (arrayDimensions, pos7) = (ParserUtils.sliceToUInt(byteVector, pos6, pos6 + 4), pos6 + 4)
        val (maxStringLength, pos8) = (ParserUtils.sliceToUInt(byteVector, pos7, pos7 + 4), pos7 + 4)
        val (dataSetFieldId, pos9) = (ParserUtils.parseGuid(byteVector, pos8), pos8 + 16)

        // TODO... rest of the fields!
        val (keyValuePairLength, pos10) = (ParserUtils.sliceToUInt(byteVector, pos9, pos9 + 4), pos9 + 4)


        println(keyValuePairLength)

        fieldMetaData(size - 1, pos, acc)
      } else (acc, pos)
    }

    def qualifiedNames(size: Int, pos: ParsePosition, acc: Vector[QualifiedName]): (Vector[QualifiedName], ParsePosition) = {
      if (size > 0) {
        val (qName, nPos) = ParserUtils.parseQualifiedName(byteVector, pos)
        qualifiedNames(size - 1, nPos, acc :+ qName)
      } else (acc, pos)
    }

    fieldMetaData(size, parsePosition, Vector.empty)
  }
}
