package util

import scala.compiletime.ops.int._
import scala.compiletime.ops.boolean._
import scala.compiletime.{constValue, error}

object Vec:
  def apply[A](x: A): Vec[A, 1] = new Vec(Vector(x))

  def apply[A](x: A, y: A): Vec[A, 2] = new Vec(Vector(x, y))

  def apply[A](x: A, y: A, z: A): Vec[A, 3] = new Vec(Vector(x, y, z))

  def apply[A](x: A, y: A, z: A, a: A): Vec[A, 4] = new Vec(Vector(x, y, z, a))

  inline def fromVector[A, N <: Int](v: Vector[A]): Option[Vec[A, N]] =
    inline if (v.length != constValue[N]) {
      None
    } else {
      Some(new Vec[A, N](v))
    }

  inline def vectorSlice[A, From <: Int, Until <: Int](v: Vector[A])(using
                                                                     Assert[(Until - From) >= 0, "Slice is negative"],
                                                                     Assert[From >= 0, "From must be positive"],
                                                                     Assert[Until >= 0, "Until must be positive"]): Option[Vec[A, Until - From]] =
    if (v.length < constValue[Until - From]) {
      None
    } else {
      Some(new Vec[A, Until - From](v.slice(constValue[From], constValue[Until])))
    }


class Vec[+A, N <: Int](private val inner: Vector[A]):
  inline def head: A = inline if (constValue[N] == 0) error("empty vectors dont have heads") else inner.head

  inline def tail: Vec[A, N - 1] =
    inline if (constValue[N] == 0) error("empty vectors dont have tails")
    else new Vec[A, N - 1](inner.tail)

  def ++[O <: Int, B >: A](other: Vec[B, O]): Vec[B, N + O] = new Vec[B, N + O](inner ++ other.inner)

  def +:[B >: A](other: B): Vec[B, N + 1] = new Vec[B, N + 1](inner.+:(other))

  inline def slice[From <: Int, Until <: Int](using
                                              Assert[(Until - From) < N, "Slice is larger than the collection"],
                                              Assert[(Until - From) >= 0, "Slice is negative"],
                                              Assert[From >= 0, "From must be positive"],
                                              Assert[Until >= 0, "Until must be positive"]
                                             ): Vec[A, Until - From] =
    new Vec[A, Until - From](inner.slice(constValue[From], constValue[Until]))

  inline def apply[I <: Int](using Assert[I >= 0 && I < N, "Index out of bounds"]): A = inner(constValue[I])

  override def toString() = inner.mkString("Vec(", ", ", ")")
