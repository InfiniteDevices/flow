package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.{DataTypeSchemaHeader, EnumDescription, SimpleTypeDescription}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

import scala.annotation.tailrec


object DataTypeSchemaHeaderParser extends (ByteVector => ParsePosition => (DataTypeSchemaHeader, ParsePosition)) {

  override def apply(byteVector: ByteVector): ParsePosition => (DataTypeSchemaHeader, ParsePosition) =
    parsePosition => parseDataTypeSchemaHeader(byteVector, parsePosition)

  def parseDataTypeSchemaHeader(byteVector: ByteVector, pos: ParsePosition): (DataTypeSchemaHeader, ParsePosition) = {
    // 1. Get the size of the namespaces array which is of type Int32 (OPC UA Spec Part 6, Page 17, Chapter 5.2.5)
    val (nsArraySize, pos1) = (ParserUtils.sliceToUInt(byteVector, pos, pos + 4), pos + 4)

    @tailrec
    def namespaces(size: Int, pos: ParsePosition, acc: Vector[String] = Vector.empty): (Vector[String], ParsePosition) = {
      // TODO: This parsing recursively can be refactored as a common abstraction! I'm using the same logic in StructureField parsing
      if (size > 0) {
        // Get the length of the String element and get the String element based on the length
        val (namespaceStrSize, pos1) = (ParserUtils.sliceToUInt(byteVector, pos, pos + 4), pos + 4)
        if (namespaceStrSize > 0) {
          val namespaceStr = ParserUtils.parseString(byteVector, pos1, size = namespaceStrSize)
          namespaces(size - 1, pos1 + namespaceStrSize, acc :+ namespaceStr)
        }
        // There is nothing to parse, we add an empty namespace and proceed with the next
        else namespaces(size - 1, pos1, acc :+ "")
      }
      else (acc, pos)
    }

    // 2. Populate the namespaces array
    val (namespaceSeq, pos2) = namespaces(nsArraySize, pos1)

    // 3. Get the size of the StructureDescription array which is of type Int32
    // OPC UA Spec version 1.04, Part 3, Page 17, Chapter 5.2.5, Array size is of type Int32 or 4 bytes
    // Using the size, populate the StructureDescription sequence
    val (structDescSize, pos3) = (ParserUtils.sliceToInt(byteVector, pos2, pos2 + 4), pos2 + 4)
    val (structureDataTypes, pos4) = StructureDescriptionParser(byteVector)(structDescSize)(pos3)

    // 4. Get the size of the EnumDescription array which is of type Int32
    // OPC UA Spec version 1.04, Part 3, Page 17, Chapter 5.2.5, Array size is of type Int32 or 4 bytes
    // Using the size, populate the EnumDescription sequence
    val (enumDataTypeSize, pos5) = (ParserUtils.sliceToInt(byteVector, pos4, pos4 + 4), pos4 + 4)
    val (enumDataTypes, pos6) =
      if(enumDataTypeSize != -1) EnumDescriptionParser(byteVector)(pos5)
      else (Vector.empty[EnumDescription], pos5)

    // 5. Get the size of the SimpleTypeDescription array which is of type Int32
    // OPC UA Spec version 1.04, Part 3, Page 17, Chapter 5.2.5, Array size is of type Int32 or 4 bytes
    // Using the size, populate the SimpleTypeDescription sequence
    val (simpleDataTypeSize, pos7) = (ParserUtils.sliceToInt(byteVector, pos6, pos6 + 4), pos6 + 4)
    val (simpleDataTypes, pos8) =
      if(simpleDataTypeSize != -1) SimpleTypeDescriptionParser(byteVector)(pos7)
      else (Vector.empty[SimpleTypeDescription], pos7)

    (DataTypeSchemaHeader(
      namespaceSeq,
      structureDataTypes,
      enumDataTypes,
      simpleDataTypes
    ), pos8)
  }
}
