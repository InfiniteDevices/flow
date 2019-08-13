package com.eon.opcuapubsubclient.domain.errors

sealed trait ParseError

case class ValidationError(msg: String) extends ParseError

object ErrorCode extends (ParseError => (Int, String)) {

  def apply(error: ParseError): (Int, String) = error match {
    case ValidationError(msg) => (-1, msg)
  }
}