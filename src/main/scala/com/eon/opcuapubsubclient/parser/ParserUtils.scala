package com.eon.opcuapubsubclient.parser

import java.nio.charset.StandardCharsets
import java.util.UUID

import com.eon.opcuapubsubclient.domain.CommonTypes._
import com.eon.opcuapubsubclient.domain.HeaderTypes._
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteOrdering.{ BigEndian, LittleEndian }
import scodec.bits.{ BitVector, ByteVector }

import scala.annotation.tailrec

object ParserUtils {

  def slice(byteVector: ByteVector, from: ParsePosition, until: ParsePosition): ByteVector =
    byteVector.slice(from, until)

  def parseBoolean(byteVector: ByteVector, from: ParsePosition): (Boolean, ParsePosition) = {
    (slice(byteVector, from, from + 1).toByte() != 0, from + 1)
  }

  def parseByte(byteVector: ByteVector, from: ParsePosition): (Byte, ParsePosition) = {
    (slice(byteVector, from, from + 1).toByte(), from + 1)
  }

  def parseUByte(byteVector: ByteVector, from: ParsePosition): (Byte, ParsePosition) = {
    (slice(byteVector, from, from + 1).toByte(signed = false), from + 1)
  }

  def parseByteAsInt(byteVector: ByteVector, from: ParsePosition): (Int, ParsePosition) = {
    (BitVector(byteVector(from)).toInt(signed = false), from + 1)
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
    (slice(byteVector, from, from + 8).toLong(signed = false, ordering = LittleEndian), from + 8)
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

    def parseString(byteVector: ByteVector, from: ParsePosition, strLength: Int): String = {
      byteVector.slice(from, from + strLength).foldLeft("")((a, b) => {
        a + b.toChar
      })
    }

    val (strLength, pos) = parseUInt32(byteVector, from)
    if (strLength > 0) (parseString(byteVector, pos, strLength), pos + strLength)
    else ("", pos)
  }

  // FIXME: Use a DateTime type as UTC rather than an Long
  def parseDateTime(byteVector: ByteVector, from: ParsePosition): (Long, ParsePosition) = {
    parseInt64(byteVector, from)
  }

  /**
   * OPC UA Spec., version 1.04, Part 4, Page 160, Chapter 7.38
   * @param byteVector
   * @param from
   * @return
   * // FIXME: Use a DateTime type as UTC rather than an Long
   */
  def parseVersionTime(byteVector: ByteVector, from: ParsePosition): (Int, ParsePosition) = {
    parseUInt32(byteVector, from)
  }

  // TODO: Test if this works correctly!
  def parseGuid(byteVector: ByteVector, pos: ParsePosition): (UUID, ParsePosition) = {
    val (part1, pos1) = parseUInt32(byteVector, pos) // 4 bytes
    val (part2, pos2) = (byteVector.slice(from = pos1, until = pos1 + 2).toShort(signed = false, ordering = LittleEndian), pos1 + 2) // 2 bytes
    val (part3, pos3) = (byteVector.slice(from = pos2, until = pos2 + 2).toShort(signed = false, ordering = LittleEndian), pos2 + 2) // 2 bytes
    val (part4, _) = (byteVector.slice(from = pos3, until = pos3 + 8).toLong(signed = false, ordering = BigEndian), pos3 + 8) // 8 bytes intentionally Big Endian
    val msb = (part1 << 32) | (part2 << 16) | part3
    (new UUID(msb, part4), pos + 16) // GUID is always 16 bytes long as per the Spec
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

  def parseStatusCode(byteVector: ByteVector, from: ParsePosition): (StatusCode, ParsePosition) = {
    val (status, pos) = parseUInt32(byteVector, from)
    (StatusCode(status), pos)
  }

  def parseQualifiedName(byteVector: ByteVector, from: ParsePosition): (QualifiedName, ParsePosition) = {
    val (nsIndex, pos1) = parseUInt16(byteVector, from)
    val (qNameStr, pos2) = parseString(byteVector, pos1)
    (QualifiedName(nsIndex, qNameStr), pos2)
  }

  def parseLocalizedText(byteVector: ByteVector, pos: ParsePosition): (LocalizedText, ParsePosition) = {
    // See OPC UA Spec Part 6, Version 1.04, page number 14 Chapter 5.2.2.14 LocalizedText
    val (mask, pos1) = parseUByte(byteVector, pos)
    if ((mask & 1) == 1) { // Contains just the Locale
      val (locale, nPos) = parseString(byteVector, pos1)
      (LocalizedText(locale = Some(locale)), nPos)
    } else if ((mask & 2) == 2) { // Contains just the Text
      val (text, nPos) = parseString(byteVector, pos1)
      (LocalizedText(text = Some(text)), nPos)
    } else if ((mask & 3) == 3) { // Contains both Locale and Text
      val (locale, nPos1) = parseString(byteVector, pos1)
      val (text, nPos2) = parseString(byteVector, nPos1)
      (LocalizedText(locale = Some(locale), text = Some(text)), nPos2)
    } else (LocalizedText(), pos1)
  }

  /**
   * OPC UA Spec Part 6, version 1.04, Page 15, Chapter 5.2.2.15, Table 14
   * @param byteVector
   * @param from
   * @return
   */
  def parseExtensionObject(byteVector: ByteVector, from: ParsePosition): (ExtensionObject, ParsePosition) = {
    val (encodingTypeId, pos1) = parseNodeId(byteVector, from)
    val (encoding, pos2) = parseByte(byteVector, pos1)
    encoding match {
      case 0 =>
        (
          ExtensionObject(encodingTypeId, ByteStringEncoding(Vector.empty)),
          pos2)
      case 1 =>
        val (byteStr, pos3) = parseByteString(byteVector, pos2)
        (
          ExtensionObject(encodingTypeId, ByteStringEncoding(byteStr)),
          pos3)
      case 2 =>
        val (xmlElement, pos3) = parseXmlElement(byteVector, pos2)
        (
          ExtensionObject(encodingTypeId, XmlElementEncoding(xmlElement)),
          pos3)
    }
  }

  /**
   * OPC UA Spec Part 6, version 1.04, Page 17, Chapter 5.2.2.17, Table 16
   * @param byteVector
   * @param from
   * @return
   */
  def parseDataValue(byteVector: ByteVector, from: ParsePosition): (DataValue, ParsePosition) = {
    val (byte, pos1) = parseByte(byteVector, from)
    val mask = byte & 0xFF

    val (variant, pos2) = if ((mask & 0x01) != 0) parseVariant(byteVector, pos1)
    else (Variant(SimpleOrder(Vector.empty)), pos1)
    val (status, pos3) = if ((mask & 0x02) != 0) parseStatusCode(byteVector, pos2)
    else (StatusCode(0), pos2) // TODO: Is this OKAY for the default status code?
    val (sourceTime, pos4) = if ((mask & 0x04) != 0) parseDateTime(byteVector, pos3)
    else (0L, pos3) // TODO: DateTime need to be fixed?
    val (sourcePicoseconds, pos5) = if ((mask & 0x10) != 0) parseUInt16(byteVector, pos4)
    else (0, pos4)
    val (serverTime, pos6) = if ((mask & 0x08) != 0) parseDateTime(byteVector, pos5)
    else (0L, pos5)
    val (serverPicoseconds, pos7) = if ((mask & 0x20) != 0) parseUInt16(byteVector, pos6)
    else (0, pos6)

    (
      DataValue(variant, status, sourceTime, sourcePicoseconds, serverTime, serverPicoseconds),
      pos7)
  }

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
  def parseVariant(byteVector: ByteVector, parsePosition: ParsePosition): (Variant, ParsePosition) = {
    val (encodingMask, pos1) = parseByte(byteVector, parsePosition)
    val buildInTypeId = encodingMask & 0x3F

    def unflatten(flat: Vector[BuiltInType], dims: Vector[Int]): VariantData = {
      if (dims.length <= 1) {
        SimpleOrder(flat)
      } else {
        val (Vector(dim), rest) = dims.splitAt(1)
        val subs = flat.grouped(flat.length / dim).map(a => unflatten(a, rest)).toVector
        HigherOrder(subs)
      }
    }

    @tailrec
    def builtInTypes(size: Int, pos: ParsePosition, acc: Vector[BuiltInType] = Vector.empty): (Vector[BuiltInType], ParsePosition) = {
      if (size < 0) (acc, pos)
      else {
        val (builtInType, newPos) = parseBuiltInType(byteVector, buildInTypeId, pos)
        builtInTypes(size - 1, newPos, acc :+ builtInType)
      }
    }

    if (encodingMask == 0) {
      (Variant(SimpleOrder(Vector(ZombieType("")))), pos1)
    } else {
      val dimensionsEncoded = (encodingMask & 0x40) == 0x40
      val arrayEncoded = (encodingMask & 0x80) == 0x80
      if (arrayEncoded) {
        val (arrLength, pos2) = parseInt32(byteVector, pos1)
        val (builtInTypeVector, pos3) = builtInTypes(arrLength, pos2)

        // Now check if the array has dimensions. Array dimensions are themselves encoded as an array of Int32
        val (dimensions, pos4) = {
          if (dimensionsEncoded) parseArrayDimensions(byteVector, pos3)
          else (Vector.empty, pos3)
        }

        if (dimensions.length > 1) {
          (Variant(unflatten(builtInTypeVector, dimensions)), pos4)
        } else {
          (Variant(SimpleOrder(builtInTypeVector)), pos4)
        }
      } else {
        val (builtInType, nPos) = parseBuiltInType(byteVector, buildInTypeId, pos1)
        (Variant(SimpleOrder(Vector(builtInType))), nPos)
      }
    }
  }

  // TODO: Implement
  def parseDiagnosticInfo(byteVector: ByteVector, from: ParsePosition): (String, ParsePosition) = ("", 0)

  // ****************

  // TODO: Implement pending ones
  def parseBuiltInType(byteVector: ByteVector, builtInTypeId: Int, from: ParsePosition): (BuiltInType, ParsePosition) = builtInTypeId match {
    case 0 => (ZombieType(""), from)
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
      val (uuid, pos) = parseGuid(byteVector, from)
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
    case 24 =>
      val (variant, pos) = parseVariant(byteVector, from)
      (VariantType(variant, builtInTypeId), pos)
    case 25 =>
      val (diagnosticInfo, pos) = parseDiagnosticInfo(byteVector, from)
      (DiagnosticInfoType(diagnosticInfo, builtInTypeId), pos)
    // If nothing fits then a Zombie appears!
    case _ => (ZombieType(""), from)
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

  def parseKeyValueProperties(byteVector: ByteVector, from: ParsePosition): (Vector[KeyValueProperty], ParsePosition) = {

    @tailrec
    def parseKeyValueProperty(size: Int, pos: ParsePosition, acc: Vector[KeyValueProperty]): (Vector[KeyValueProperty], ParsePosition) = {
      if (size < 1) (acc, pos)
      else {
        val (qName, pos1) = parseQualifiedName(byteVector, pos)
        val (variant, pos2) = parseVariant(byteVector, pos1)
        val kvProp = KeyValueProperty(qName, variant)
        parseKeyValueProperty(size - 1, pos2, acc :+ kvProp)
      }
    }

    val (keyValuePropSize, pos1) = ParserUtils.parseUInt32(byteVector, from)
    parseKeyValueProperty(keyValuePropSize, pos1, Vector.empty)
  }

  def parseConfigVersion(byteVector: ByteVector, from: ParsePosition): (ConfigVersion, ParsePosition) = {
    val (majorVersion, pos1) = parseUInt32(byteVector, from)
    val (minorVersion, pos2) = parseUInt32(byteVector, pos1)
    (ConfigVersion(majorVersion, minorVersion), pos2)
  }
}
