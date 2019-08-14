package com.eon.opcuapubsubclient.domain.types.builtin

import com.eon.opcuapubsubclient.UnitSpec

class UByteSpec extends UnitSpec {

  (0 to 258).map(_.byteValue()) foreach {
    case elem if elem <= 255 =>
      s"$elem ${new java.util.Random().nextInt()}" should "validate successfully box into a UByte type" in {
        println(UByte(elem).toShort)
      }
    case elem =>
      s"$elem" should "fail for values out of range" in {
        assertThrows[IllegalArgumentException] { UByte(elem) }
      }
  }
}
