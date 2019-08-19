package com.eon

import com.eon.opcuapubsubclient.domain.errors._


package object opcuapubsubclient {

  type V[+R] = Either[ParseError, R]

  def validated[R](r: => R): V[R] = try Right(r) catch {
    case ex: Throwable => Left(ValidationError(ex.getMessage))
  }

  implicit class VSuccess[R](r: R) { val successV: V[R] = Right(r) }

  implicit class VFailure(e: ParseError) { def failureV[R]: V[R] = Left(e) }

  def tryFailure[B](block: => B)(error: String => ParseError): V[B] = try block.successV catch {

    case e: Exception => error(e.getMessage).failureV
  }

  implicit class SequenceV[R](s: List[V[R]]) {
    val sequenceV: V[List[R]] = s.foldRight(Right(Nil): V[List[R]]) { (e, acc) =>
      for {
        xs <- acc.right
        x <- e.right
      } yield x :: xs
    }
  }

  implicit class BooleanToOption(val self: Boolean) extends AnyVal {
    def toOption[A](value: => A): Option[A] =
      if (self) Some(value) else None
  }
}
