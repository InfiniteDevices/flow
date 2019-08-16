package com.eon.opcuapubsubclient.parser

import java.nio.charset.StandardCharsets
import java.util.UUID

import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.BuiltInType._
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.{BuiltInType, LocalizedText, NodeId, QualifiedName, Variant}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteOrdering.{BigEndian, LittleEndian}
import scodec.bits.ByteVector

import scala.annotation.tailrec


object ParserUtils {

  def parseBoolean(byteVector: ByteVector, from: ParsePosition): (Boolean, ParsePosition) = {
    (slice(byteVector, from, from + 1).toByte() != 0, from + 1)
  }

  def parseByte(byteVector: ByteVector, from: ParsePosition): (Byte, ParsePosition) = {
    (slice(byteVector, from, from + 1).toByte(), from + 1)
  }

  def parseUByte(byteVector: ByteVector, from: ParsePosition): (Byte, ParsePosition) = {
    (slice(byteVector, from, from + 1).toByte(signed = false), from + 1)
  }

  def parseInt16(byteVector: ByteVector, from: ParsePosition): (Int, ParsePosition) = {
    (slice(byteVector, from, from + 2).toInt(ordering = LittleEndian), from + 2)
  }

  def parseUInt16(byteVector: ByteVector, from: ParsePosition): (Int, ParsePosition) = {
    (slice(byteVector, from, from + 2).toInt(signed = false, ordering = LittleEndian), from + 2)
  }

  def parseInt32(byteVector: ByteVector, from: ParsePosition): (Int, ParsePosition) = {
    (slice(byteVector, from, from + 4).toInt(ordering = LittleEndian), from + 4)
  }

  def parseUInt32(byteVector: ByteVector, from: ParsePosition): (Int, ParsePosition) = {
    (slice(byteVector, from, from + 4).toInt(signed = false, ordering = LittleEndian), from + 4)
  }

  def parseInt64(byteVector: ByteVector, from: ParsePosition): (Long, ParsePosition) = {
    (slice(byteVector, from, from + 8).toLong(ordering = LittleEndian), from + 8)
  }

  def parseUInt64(byteVector: ByteVector, from: ParsePosition): (Long, ParsePosition) = {
    (slice(byteVector, from, from + 8).toInt(signed = false, ordering = LittleEndian), from + 8)
  }

  def parseFloat(byteVector: ByteVector, from: ParsePosition): (Float, ParsePosition) = {
    val (int32, pos) = parseInt32(byteVector, from)
    (java.lang.Float.intBitsToFloat(int32), pos)
  }

  def parseDouble(byteVector: ByteVector, from: ParsePosition): (Double, ParsePosition) = {
    val (int64, pos) = parseInt64(byteVector, from)
    (java.lang.Double.longBitsToDouble(int64), pos)
  }

  def parseString(byteVector: ByteVector, from: ParsePosition): (String, ParsePosition) = {
    val (strLength, pos) = (sliceToUInt(byteVector, from, from + 4), from + 4)
    (parseString(byteVector, pos, strLength), pos + strLength)
  }

  def parseString(byteVector: ByteVector, from: ParsePosition, size: Int = 4): String = {
    byteVector.slice(from, from + size).foldLeft("")((a, b) => {
      a + b.toChar
    })
  }

  // FIXME: Use a DateTime type rather than an Long
  def parseDateTime(byteVector: ByteVector, from: ParsePosition): (Long, ParsePosition) = {
    parseInt64(byteVector, from)
  }

  // TODO: Test if this works correctly!
  def parseGuid(byteVector: ByteVector, pos: ParsePosition): UUID = {
    val (part1, pos1) = sliceToUIntWithPosition(byteVector, pos, pos + 4) // 4 bytes
    println(part1)
    val (part2, pos2) = (byteVector.slice(from = pos1, until = pos1 + 2).toShort(signed = false, ordering = LittleEndian), pos1 + 2) // 2 bytes
    val (part3, pos3) = (byteVector.slice(from = pos2, until = pos2 + 2).toShort(signed = false, ordering = LittleEndian), pos2 + 2) // 2 bytes
    val (part4, _) = (byteVector.slice(from = pos3, until = pos3 + 8).toLong(signed = false, ordering = BigEndian), pos3 + 8) // 8 bytes intentionally Big Endian
    val msb = (part1 << 32) | (part2 << 16) | part3
    new UUID(msb, part4)
  }

  def parseByteString(byteVector: ByteVector, from: ParsePosition): (Vector[Byte], ParsePosition) = {
    val (length, pos1) = parseInt32(byteVector, from)
    (slice(byteVector, pos1, pos1 + length).toSeq.toVector, pos1 + length)
  }

  def parseXmlElement(byteVector: ByteVector, from: ParsePosition): (String, ParsePosition) = {
    val (byteString, pos1) = parseByteString(byteVector, from)
    (new String(byteString.toArray, StandardCharsets.UTF_8), pos1)
  }

  // TODO: Move the parser in here, in this utility class
  def parseNodeId(byteVector: ByteVector, from: ParsePosition): (NodeId, ParsePosition) = {
    NodeIdParser(byteVector)(from)
  }

  // TODO: Implement, FIXME: Wrong implementation
  def parseExpandedNodeId(byteVector: ByteVector, from: ParsePosition): (NodeId, ParsePosition) = {
    parseNodeId(byteVector, from)
  }

  def parseStatusCode(byteVector: ByteVector, from: ParsePosition): (Int, ParsePosition) = {
    parseUInt32(byteVector, from)
  }

  def parseQualifiedName(byteVector: ByteVector, from: ParsePosition): (QualifiedName, ParsePosition) = {
    val (nsIndex, pos1) = parseUInt16(byteVector, from)
    val (qNameStr, pos2) = parseString(byteVector, pos1)
    (QualifiedName(nsIndex, qNameStr), pos2)
  }

  def parseLocalizedText(byteVector: ByteVector, pos: ParsePosition): (LocalizedText, ParsePosition) = {
    // See OPC UA Spec Part 6, Version 1.04, page number 14 Chapter 5.2.2.14 LocalizedText
    val (mask, pos1) = (sliceUByte(byteVector, pos), pos + 1)
    if ((mask & 1) == 1) { // Contains just the Locale
      val (locale, nPos) = parseString(byteVector, pos1)
      (LocalizedText(locale = Some(locale)), nPos)
    }
    else if ((mask & 2) == 2) { // Contains just the Text
      val (text, nPos) = parseString(byteVector, pos1)
      (LocalizedText(text = Some(text)), nPos)
    }
    else if ((mask & 3) == 3) { // Contains both Locale and Text
      val (locale, nPos1) = parseString(byteVector, pos1)
      val (text, nPos2) = parseString(byteVector, nPos1)
      (LocalizedText(locale = Some(locale), text = Some(text)), nPos2)
    }
    else (LocalizedText(), pos1)
  }

  // TODO: Implement
  def parseExtensionObject(byteVector: ByteVector, from: ParsePosition): (String, ParsePosition) = ("", 0)

  // TODO: Implement
  def parseDataValue(byteVector: ByteVector, from: ParsePosition): (String, ParsePosition) = ("", 0)

  /**
    * Implemented according to the OPC UA Spec version 1.04,
    * Part 6, Page number 16, Chapter 5.2.2.16 Table 15 - Variant Binary DataEncoding
    *
    * FIXME: Ids 26 to 31 are not yet supported, but still the spec says that Decoders should
    * support these Ids and should assume that the Value is a ByteString
    *
    * TODO: Refactor without nested if else statements
    *
    * @param byteVector The incoming bytes
    * @param parsePosition The starting position in the ByteVector from where the parsing should happen
    * @return
    */
 /* def parseVariant(byteVector: ByteVector, parsePosition: ParsePosition): (Variant, ParsePosition) = {
    val (encodingMask, pos1) = (ParserUtils.sliceToUInt(byteVector, from = parsePosition, until = parsePosition + 1), parsePosition + 1)
    val buildInTypeId = encodingMask & 0x3F
    val dimensionsEncoded = (encodingMask & 0x40) == 0x40
    val arrayEncoded = (encodingMask & 0x80) == 0x80

    if (buildInTypeId == 0) {
      Variant(ZombieType(""))
    } else {
      if (arrayEncoded) {
        val (arrLength, pos2) = parseInt32(byteVector, pos1)
        if (arrLength == -1) {
          Variant(ZombieType(""))
        } else { // TODO: This does not work., we need a recursive solution to capture the position variable

          @tailrec
          def builtInTypes(size: Int, pos: ParsePosition, acc: Vector[BuiltInType] = Vector.empty): (Vector[BuiltInType], ParsePosition) = {
            if (size < 0) (acc, pos)
            else {
              val (builtInType, newPos) = parseBuiltInType(byteVector, buildInTypeId, pos2)
              builtInTypes(size - 1, newPos, acc :+ builtInType)
            }
          }

          val (builtInTypeSeq, pos3) = builtInTypes(arrLength, pos2)
          val (dimensions, pos4) = {
            if (dimensionsEncoded) parseArrayDimensions(byteVector, pos3)
            else (Vector.empty, pos3)
          }
          if(dimensions.length > 1) ArrayUtil.unflatten(flatArray, dimensions)
          else builtInTypeSeq
        }
      }
    }
  } */

  // TODO: Implement
  def parseDiagnosticInfo(byteVector: ByteVector, from: ParsePosition): (String, ParsePosition) = ("", 0)

  // ****************

  // TODO: Implement pending ones
  def parseBuiltInType(byteVector: ByteVector, builtInTypeId: Int, from: ParsePosition): (BuiltInType, ParsePosition) = builtInTypeId match {
    case 0 =>
      (ZombieType(""), from)
    case 1 =>
      val (bool, pos) = parseBoolean(byteVector, from)
      (BooleanType(bool, builtInTypeId), pos)
    case 2 =>
      val (byte, pos) = parseByte(byteVector, from)
      (ByteType(byte, builtInTypeId), pos)
    case 3 =>
      val (ubyte, pos) = parseUByte(byteVector, from)
      (UByteType(ubyte, builtInTypeId), pos)
    case 4 =>
      val (int16, pos) = parseInt16(byteVector, from)
      (Int16Type(int16, builtInTypeId), pos)
    case 5 =>
      val (uint16, pos) = parseUInt16(byteVector, from)
      (UInt16Type(uint16, builtInTypeId), pos)
    case 6 =>
      val (int32, pos) = parseInt32(byteVector, from)
      (Int32Type(int32, builtInTypeId), pos)
    case 7 =>
      val (uint32, pos) = parseUInt32(byteVector, from)
      (UInt32Type(uint32, builtInTypeId), pos)
    case 8 =>
      val (int64, pos) = parseInt64(byteVector, from)
      (Int64Type(int64, builtInTypeId), pos)
    case 9 =>
      val (uint64, pos) = parseUInt64(byteVector, from)
      (UInt64Type(uint64, builtInTypeId), pos)
    case 10 =>
      val (float, pos) = parseFloat(byteVector, from)
      (FloatType(float, builtInTypeId), pos)
    case 11 =>
      val (double, pos) = parseDouble(byteVector, from)
      (DoubleType(double, builtInTypeId), pos)
    case 12 =>
      val (string, pos) = parseString(byteVector, from)
      (StringType(string, builtInTypeId), pos)
    case 13 =>
      val (dateTime, pos) = parseDateTime(byteVector, from)
      (DateTimeType(dateTime, builtInTypeId), pos)
    case 14 =>
      val (uuid, pos) = (parseGuid(byteVector, from), from + 16) // FIXME: Fix the parseGuid to return the position as well!
      (GuidType(uuid, builtInTypeId), pos)
    case 15 =>
      val (byteString, pos) = parseByteString(byteVector, from)
      (ByteStringType(byteString, builtInTypeId), pos)
    case 16 =>
      val (xmlElem, pos) = parseXmlElement(byteVector, from)
      (XmlElementType(xmlElem, builtInTypeId), pos)
    case 17 =>
      val (nodeId, pos) = parseNodeId(byteVector, from)
      (NodeIdType(nodeId, builtInTypeId), pos)
    case 18 =>
      val (expNodeId, pos) = parseExpandedNodeId(byteVector, from)
      (ExpandedNodeIdType(expNodeId, builtInTypeId), pos)
    case 19 =>
      val (statusCode, pos) = parseStatusCode(byteVector, from)
      (StatusCodeType(statusCode, builtInTypeId), pos)
    case 20 =>
      val (qName, pos) = parseQualifiedName(byteVector, from)
      (QualifiedNameType(qName, builtInTypeId), pos)
    case 21 =>
      val (locText, pos) = parseLocalizedText(byteVector, from)
      (LocalizedTextType(locText, builtInTypeId), pos)
    case 22 =>
      val (extObj, pos) = parseExtensionObject(byteVector, from)
      (ExtensionObjectType(extObj, builtInTypeId), pos)
    case 23 =>
      val (dataValue, pos) = parseDataValue(byteVector, from)
      (DataValueType(dataValue, builtInTypeId), pos)
    /*case 25 =>
      val (variant, pos) = parseVariant(byteVector, from)
      (VariantType(variant, builtInTypeId), pos) */
    case 25 =>
      val (diagInfo, pos) = parseDiagnosticInfo(byteVector, from)
      (DiagnosticInfoType(diagInfo, builtInTypeId), pos)
    case _ =>
      (ZombieType(""), from)
  }

  def parseArrayDimensions(byteVector: ByteVector, from: ParsePosition): (Vector[Int], ParsePosition) = {
    val (size, pos1) = parseInt32(byteVector, from)

    @tailrec
    def arrayDimensions(size: Int, pos: ParsePosition, acc: Vector[Int]): (Vector[Int], ParsePosition) = {
      if (size < 0) (acc, pos)
      else {
        val (elem, newPos) = parseInt32(byteVector, pos)
        arrayDimensions(size - 1, newPos, acc :+ elem)
      }
    }
    arrayDimensions(size, pos1, Vector.empty)
  }

  def slice(byteVector: ByteVector, from: ParsePosition, until: ParsePosition): ByteVector =
    byteVector.slice(from, until)

  def sliceUByte(byteVector: ByteVector, from: ParsePosition): Byte = {
    byteVector.slice(from, from + 1).toByte(signed = false)
  }

  def sliceToUByte(byteVector: ByteVector, from: ParsePosition, until: ParsePosition): Vector[Byte] = {
    byteVector.slice(from, until).toSeq.toVector
  }

  def sliceToUInt(byteVector: ByteVector, from: ParsePosition, until: ParsePosition): Int =
    slice(byteVector, from, until).toInt(signed = false, ordering = LittleEndian)

  def sliceToInt(byteVector: ByteVector, from: ParsePosition, until: ParsePosition): Int =
    slice(byteVector, from, until).toInt(ordering = LittleEndian)

  // TODO: This is buggy, fix this later, do not use this now!
  def sliceToUIntWithPosition(byteVector: ByteVector, from: ParsePosition, until: ParsePosition): (Int, ParsePosition) =
    (sliceToUInt(byteVector, from, until), until - from)

  def parseKPropertiesKeyValuePairs(byteVector: ByteVector, pos: ParsePosition): (Vector[(QualifiedName, String)], ParsePosition) = ???
}
