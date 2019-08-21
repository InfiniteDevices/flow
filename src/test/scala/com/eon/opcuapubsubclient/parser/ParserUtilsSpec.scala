package com.eon.opcuapubsubclient.parser



import com.eon.opcuapubsubclient.UnitSpec


// TODO: Rewrite this class with tests! This is just to see how we can parse Variant data
class ParserUtilsSpec extends UnitSpec {

  def unflatten(arr: Vector[Int], dimensions: Vector[Int], offset: Int): Object = {
    if (dimensions.length == 1) {
      (0 until dimensions(0)).zipWithIndex.map { case (_, i) =>
        Vector(arr(offset + i))
      }
    } else {
      val tail = dimensions.tail
      (0 until dimensions(0)).zipWithIndex.map { case (_, i) =>
        val resss = unflatten(arr, tail, offset + i * tail.length)
        Vector(resss)
      }
    }
  }/*

  def unflatten(`type`: Class[_],
                        array: Object,
                        dimensions: Array[Int],
                        offset: Int): Object =
    if (dimensions.length == 1) {
      val a: Object = java.lang.reflect.Array.newInstance(`type`, dimensions(0))
      for (i <- 0 until dimensions(0)) {
        java.lang.reflect.Array.set(a, i, java.lang.reflect.Array.get(array, offset + i))
      }
      a
    } else {
      val a: Object = java.lang.reflect.Array.newInstance(`type`, dimensions.toList:_*)
      val tail: Array[Int] = util.Arrays.copyOfRange(dimensions, 1, dimensions.length)
      for (i <- 0 until dimensions(0)) {
        val element: AnyRef =
          unflatten(`type`, array, tail, offset + i * tail.length)
        java.lang.reflect.Array.set(a, i, element)
      }
      a
    } */

  trait Matrix
  case class SimpleMatrix(rows: Vector[Int]) extends Matrix
  case class HigherMatrix(matrices: Vector[Matrix]) extends Matrix

  def unflatten(flat: Vector[Int], dims: Vector[Int]): Matrix = {
    if (dims.length <= 1) {
      SimpleMatrix(flat)
    } else {
      val (Vector(dim), rest) = dims.splitAt(1)

      val subs = flat.grouped(flat.length/dim).map(a => unflatten(a, rest)).toVector

      HigherMatrix(subs)
    }
  }


  "dummy test" should "do dummy things" in {

    case class Elem(elems: Vector[Elem])
    val dimensions = Array(2,2,2,3)
    val elems = Array(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)

    val result = unflatten(elems.toVector, dimensions.toVector)
    println(result)
  }
}
