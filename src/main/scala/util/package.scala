import scala.compiletime.{error, constValue}

package object util:
  trait Assert[P, M <: String] {}

  implicit inline def isTrue[M <: String]: Assert[true, M] = new Assert[true, M] {}
  implicit inline def isFalse[M <: String]: Assert[false, M] = error(constValue[M])
