package com.eon.opcuapubsubclient.parser

import java.util.UUID

import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.{LocalizedText, QualifiedName, Variant}
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteOrdering.{BigEndian, LittleEndian}
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}


object ParserUtils {

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

  /* def parseStringUntil(byteVector: ByteVector, from: ParsePosition, until: ParsePosition): (String, ParsePosition) = {
    Try { sliceToUIntWithPosition(byteVector, from, until) } match { // Here is the error!
      case Success(suck) => println("suck")
      case Failure(fail) => fail.printStackTrace()
    }
    val (strSize, pos1) = sliceToUIntWithPosition(byteVector, from, until)

    (byteVector.slice(from, until).foldLeft("")((a, b) => {
      a + b.toChar
    }), pos1 + strSize)
  } */

  def parseString(byteVector: ByteVector, from: ParsePosition, size: Int = 4): String = {
    byteVector.slice(from, from + size).foldLeft("")((a, b) => {
      a + b.toChar
    })
  }

  def parseStringWithPosition(byteVector: ByteVector, from: ParsePosition): (String, ParsePosition) = {
    val (strLength, pos) = (sliceToUInt(byteVector, from, from + 4), from + 4)
    (parseString(byteVector, pos, strLength), pos + strLength)
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

  def parseLocalizedText(byteVector: ByteVector, pos: ParsePosition): (LocalizedText, ParsePosition) = {
    // See OPC UA Spec Part 6, Version 1.04, page number 14 Chapter 5.2.2.14 LocalizedText
    val (mask, pos1) = (sliceUByte(byteVector, pos), pos + 1)
    if ((mask & 1) == 1) { // Contains just the Locale
      val (locale, nPos) = parseStringWithPosition(byteVector, pos1)
      (LocalizedText(locale = Some(locale)), nPos)
    }
    else if ((mask & 2) == 2) { // Contains just the Text
      val (text, nPos) = parseStringWithPosition(byteVector, pos1)
      (LocalizedText(text = Some(text)), nPos)
    }
    else if ((mask & 3) == 3) { // Contains both Locale and Text
      val (locale, nPos1) = parseStringWithPosition(byteVector, pos1)
      val (text, nPos2) = parseStringWithPosition(byteVector, nPos1)
      (LocalizedText(locale = Some(locale), text = Some(text)), nPos2)
    }
    else (LocalizedText(), pos1)
  }

  def parseQualifiedName(byteVector: ByteVector, pos: ParsePosition): (QualifiedName, ParsePosition) = {
    val (nsIndex, nPos1) = (byteVector.slice(from = pos, until = pos + 2).toInt(signed = false, ordering = LittleEndian), pos + 2)
    val (strLength, nPos2) = (ParserUtils.sliceToUInt(byteVector, nPos1, nPos1 + 4), nPos1 + 4)
    val (qNameStr, nPos3) = (ParserUtils.parseString(byteVector, nPos2, size = strLength), nPos2 + strLength)
    (QualifiedName(nsIndex, qNameStr), nPos3)
  }

  def parseKPropertiesKeyValuePairs(byteVector: ByteVector, pos: ParsePosition): (Vector[(QualifiedName, String)], ParsePosition) = ???

  /**
    * Implemented according to the OPC UA Spec version 1.04,
    * Part 6, Page number 16, Chapter 5.2.2.16 Table 15 - Variant Binary DataEncoding
    *
    * @param byteVector The incoming bytes
    * @param parsePosition The starting position in the ByteVector from where the parsing should happen
    * @return
    */
  def parseVariant(byteVector: ByteVector, parsePosition: ParsePosition): (Variant, ParsePosition) = ???
}
