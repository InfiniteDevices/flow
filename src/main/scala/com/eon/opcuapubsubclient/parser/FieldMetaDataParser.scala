package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.FieldMetaData
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
        val (name, pos1) = ParserUtils.parseStringWithPosition(byteVector, parsePosition)

        val (description, pos2) = ParserUtils.parseLocalizedText(byteVector, pos1)

        // TODO: FieldFlags

        val (builtInType, pos4) = (ParserUtils.sliceToUInt(byteVector, pos2, pos2 + 1), pos2 + 1)
        val (valueRank, pos5) = (ParserUtils.sliceToInt(byteVector, pos4, pos4 + 4), pos4 + 4)
        val (arrayDimensions, pos6) = (ParserUtils.sliceToUInt(byteVector, pos2, pos2 + 1), pos2 + 1)

        //val (maxStringLength, pos7) = (ParserUtils.sliceToUInt(byteVector, pos6, pos6 + 4), pos6 + 4)
        //val (dataSetFieldId, pos8) = (ParserUtils.parseGuid(byteVector, pos7), pos7 + 16)

        // TODO... rest of the fields!

        fieldMetaData(size - 1, pos, acc)
      } else (acc, pos)
    }

    fieldMetaData(size, parsePosition, Vector.empty)
  }
}
