package com.eon.opcuapubsubclient.parser

import java.nio.charset.StandardCharsets
import java.util.UUID

import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.BuiltInType.ZombieType
import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.{BuiltInType, LocalizedText, QualifiedName, Variant}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteOrdering.{BigEndian, LittleEndian}
import scodec.bits.ByteVector


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
    val (byteString, pos1) = parseByteString(byteString, from)
    (new String(byteString.toArray, StandardCharsets.UTF_8), pos1)
  }

  // TODO: Implement
  def parseExpandedNodeId(byteVector: ByteVector, from: ParsePosition): Unit = ???

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
  def parseExtensionObject(byteVector: ByteVector, from: ParsePosition): Unit = ???

  // TODO: Implement
  def parseDataValue(byteVector: ByteVector, from: ParsePosition): Unit = ???

  /**
    * Implemented according to the OPC UA Spec version 1.04,
    * Part 6, Page number 16, Chapter 5.2.2.16 Table 15 - Variant Binary DataEncoding
    *
    * FIXME: Ids 26 to 31 are not yet supported, but still the spec says that Decoders should
    * support these Ids and should assume that the Value is a ByteString
    *
    * @param byteVector The incoming bytes
    * @param parsePosition The starting position in the ByteVector from where the parsing should happen
    * @return
    */
  def parseVariant(byteVector: ByteVector, parsePosition: ParsePosition): (Variant, ParsePosition) = {
    val (encodingMask, pos1) = (ParserUtils.sliceToUInt(byteVector, from = parsePosition, until = parsePosition + 1), parsePosition + 1)
    val buildInType = BuiltInType.builtInTypes(encodingMask & 0x3F)
    val dimensionsEncoded = (encodingMask & 0x40) == 0x40
    val arrayEncoded = (encodingMask & 0x80) == 0x80

    if (buildInType == ZombieType) {
      Variant(ZombieType)
    } else {
      if (arrayEncoded) {
        val (arrLength, pos2) = (ParserUtils.sliceToInt(byteVector, from = pos1, until = pos1 + 4), pos1 + 4)
      }
    }
  }

  // TODO: Implement
  def parseDiagnosticInfo(byteVector: ByteVector, from: ParsePosition): Unit = ???

  // ****************

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
