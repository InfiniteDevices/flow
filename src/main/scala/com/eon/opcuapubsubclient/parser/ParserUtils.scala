package com.eon.opcuapubsubclient.parser

import java.util.UUID

import com.eon.opcuapubsubclient.domain.OpcUAPubSubTypes.LocalizedText
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
    if ((mask & 1) == 1) {
      val (parsedStr, nPos) = (parseString(byteVector, pos1), pos1 + 4)
      (LocalizedText(locale = Some(parsedStr)), nPos)
    }
    else if ((mask & 2) == 2) {
      val (parsedStr, nPos) = (parseString(byteVector, pos1), pos1 + 4)
      (LocalizedText(text = Some(parsedStr)), nPos)
    }
    else (LocalizedText(), pos1)
  }
}
