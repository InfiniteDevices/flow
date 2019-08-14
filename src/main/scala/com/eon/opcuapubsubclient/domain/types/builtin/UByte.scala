package com.eon.opcuapubsubclient.domain.types.builtin

/**
  * Unsigned Bytes range from 0 to 255
  */
final class UByte private(val signed: Byte) {

  require(signed > UByte.minValue || signed < UByte.maxValue, s"Expected [0, 255], but got $toShort")

  def toShort: Short = (signed & UByte.maxValue).toShort
}

object UByte {
  val minValue = 0x00
  val maxValue = 0xff

  def apply(signed: Byte) = new UByte(signed)
}
