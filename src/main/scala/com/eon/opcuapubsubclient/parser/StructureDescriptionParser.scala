package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.{QualifiedName, StructureDefinition, StructureDescription, StructureField, StructureType}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteOrdering.LittleEndian
import scodec.bits.ByteVector

import scala.annotation.tailrec


object StructureDescriptionParser extends (ByteVector => Int => ParsePosition => (Vector[StructureDescription], ParsePosition)) {

  override def apply(byteVector: ByteVector): Int => ParsePosition => (Vector[StructureDescription], ParsePosition) = arraySize => parsePosition => {
    parseStructureDescriptions(byteVector, arraySize, parsePosition, Vector.empty)
  }

  @tailrec
  def parseStructureDescriptions(v: ByteVector, size: Int, pos: ParsePosition, acc: Vector[StructureDescription]): (Vector[StructureDescription], ParsePosition) = {
    if (size > 0) {
      val (structureDescription, nPos) = parseStructureDescription(v, pos)
      parseStructureDescriptions(v, size - 1, nPos, acc :+ structureDescription)
    } else (acc, pos)
  }

  def parseStructureDescription(byteVector: ByteVector, pos: ParsePosition): (StructureDescription, ParsePosition) = {
    // 1. Populate the DataTypeId which is of type NodeId
    val (dataTypeId, pos1) = NodeIdParser(byteVector)(pos)

    // 2. Populate the QualifiedName
    val (qName, pos2) = ParserUtils.parseQualifiedName(byteVector, pos1)

    // 3. Populate the DefaultEncodingId which is of type NodeId
    val (defaultEncodingId, pos3) = NodeIdParser(byteVector)(pos2)

    // 4. Populate the BaseDataType which is of type NodeId
    val (baseDataType, pos4) = NodeIdParser(byteVector)(pos3)

    // 5. Identify the StructureType enum (OPC UA Spec., version 1.04, Part 6, Page 17 under Enumerations)
    // Enums are of type Int32 which is 4 bytes long
    // TODO: What happens when nothing matches!!!!
    val (structureType, pos5) = ParserUtils.sliceToInt(byteVector, pos4, pos4 + 4) match {
      case 0 => (StructureType.Simple, pos4 + 4)
      case 1 => (StructureType.OptionalFields, pos4 + 4)
      case 2 => (StructureType.Union, pos4 + 4)
    }

    @tailrec
    def parseStructureFields(byteVector: ByteVector, size: Int, pos: ParsePosition, acc: Vector[StructureField]): (Vector[StructureField], ParsePosition) = {

      // OPC UA Spec version 1.04, Part 3, Page 69, Table 36 StructureField Structure
      def parseStructureField(byteVector: ByteVector, pos: ParsePosition): (StructureField, ParsePosition) = {
        val (name, pos1) = ParserUtils.toString(byteVector, pos)
        val (description, pos2) = ParserUtils.parseLocalizedText(byteVector, pos1)
        val (dataType, pos3) = NodeIdParser(byteVector)(pos2)
        // ValueRank is of type Int32 or 4 bytes long
        val (valueRank, pos4) = (ParserUtils.sliceToInt(byteVector, pos3, pos3 + 4), pos3 + 4)
        // TODO: Check this with the OPC UA Discussion forum - Is this correct to assume that the arrayDimensions is -1 in case of a -1 ValueRank?
        // https://opcfoundation.org/forum/opc-ua-standard/opc-ua-valuerank-and-arraydimensions-decoding/#p1909
        val (arrayDimensions, pos5) = (ParserUtils.sliceToInt(byteVector, pos4, pos4 + 4), pos4 + 4)
        val (maxStringLength, pos6) = (ParserUtils.sliceToInt(byteVector, pos5, pos5 + 4), pos5 + 4)
        val (isOptional, pos7) = (ParserUtils.sliceToInt(byteVector, pos6, pos6 + 1) == 1, pos6 + 1)
        (StructureField(
          name,
          description,
          dataType,
          valueRank,
          arrayDimensions,
          maxStringLength,
          isOptional
        ), pos7)
      }

      if (size > 0) {
        val (structureField, nPos) = parseStructureField(byteVector, pos)
        parseStructureFields(byteVector, size - 1, nPos, acc :+ structureField)
      } else (acc, pos)
    }

    // 6. Identify the StructureField sequence length and use it to populate the StructureField sequence
    val (structFieldLen, pos6) = (byteVector.slice(from = pos5, until = pos5 + 4).toInt(signed = false, ordering = LittleEndian), pos5 + 4)
    val (structureFieldsParsed, pos7) = parseStructureFields(byteVector, structFieldLen, pos6, Vector.empty)

    // TODO: It is bug free until here....

    val structureDefinition = StructureDefinition(
      defaultEncodingId,
      baseDataType,
      structureType,
      structureFieldsParsed
    )

    (StructureDescription(
      dataTypeId,
      qName,
      structureDefinition
    ), pos7)
  }
}
