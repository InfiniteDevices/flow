package com.eon.opcuapubsubclient.parser.datasetmetadata

import com.eon.opcuapubsubclient.domain.PayloadTypes.{ DataTypeSchemaHeader, EnumDescription, SimpleTypeDescription }
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import com.eon.opcuapubsubclient.parser.{ ParserUtils, SimpleTypeDescriptionParser }
import scodec.bits.ByteVector

import scala.annotation.tailrec

object DataTypeSchemaHeaderParser extends (ByteVector => ParsePosition => (DataTypeSchemaHeader, ParsePosition)) {

  override def apply(byteVector: ByteVector): ParsePosition => (DataTypeSchemaHeader, ParsePosition) =
    parsePosition => parseDataTypeSchemaHeader(byteVector, parsePosition)

  def parseDataTypeSchemaHeader(byteVector: ByteVector, from: ParsePosition): (DataTypeSchemaHeader, ParsePosition) = {
    // 1. Get the size of the namespaces array which is of type Int32 (OPC UA Spec Part 6, Page 17, Chapter 5.2.5)
    val (nsArraySize, pos1) = ParserUtils.parseUInt32(byteVector, from)

    @tailrec
    def namespaces(size: Int, pos: ParsePosition, acc: Vector[String] = Vector.empty): (Vector[String], ParsePosition) = {
      if (size > 0) {
        val (ns, nPos) = ParserUtils.parseString(byteVector, pos)
        namespaces(size - 1, nPos, acc :+ ns)
      } else (acc, pos)
    }

    // 2. Populate the namespaces array
    val (namespaceSeq, pos2) = namespaces(nsArraySize, pos1)

    // 3. Get the size of the StructureDescription array which is of type Int32
    // OPC UA Spec version 1.04, Part 3, Page 17, Chapter 5.2.5, Array size is of type Int32 or 4 bytes
    // Using the size, populate the StructureDescription sequence
    val (structDescSize, pos3) = ParserUtils.parseUInt32(byteVector, pos2)
    val (structureDataTypes, pos4) = StructureDescriptionParser(byteVector)(structDescSize)(pos3)

    // 4. Get the size of the EnumDescription array which is of type Int32
    // OPC UA Spec version 1.04, Part 3, Page 17, Chapter 5.2.5, Array size is of type Int32 or 4 bytes
    // Using the size, populate the EnumDescription sequence
    val (enumDataTypeSize, pos5) = ParserUtils.parseUInt32(byteVector, pos4)
    val (enumDataTypes, pos6) =
      if (enumDataTypeSize != -1) EnumDescriptionParser(byteVector)(pos5)
      else (Vector.empty[EnumDescription], pos5)

    // 5. Get the size of the SimpleTypeDescription array which is of type Int32
    // OPC UA Spec version 1.04, Part 3, Page 17, Chapter 5.2.5, Array size is of type Int32 or 4 bytes
    // Using the size, populate the SimpleTypeDescription sequence
    val (simpleDataTypeSize, pos7) = ParserUtils.parseUInt32(byteVector, pos6)
    val (simpleDataTypes, pos8) =
      if (simpleDataTypeSize != -1) SimpleTypeDescriptionParser(byteVector)(pos7)
      else (Vector.empty[SimpleTypeDescription], pos7)

    (DataTypeSchemaHeader(
      namespaceSeq,
      structureDataTypes,
      enumDataTypes,
      simpleDataTypes), pos8)
  }
}
