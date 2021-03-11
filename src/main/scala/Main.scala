import scodec.codecs.uint8

import scala.compiletime.S
import scala.compiletime.ops.int._
import scala.compiletime.ops.boolean._
import scala.compiletime.ops.any._
import scala.compiletime.{constValue, error, summonInline}

object Vec:
  def apply[A](x: A): Vec[A, 1] = new Vec(Vector(x))
  def apply[A](x: A, y: A): Vec[A, 2] = new Vec(Vector(x, y))
  def apply[A](x: A, y: A, z: A): Vec[A, 3] = new Vec(Vector(x, y, z))
  def apply[A](x: A, y: A, z: A, a: A): Vec[A, 4] = new Vec(Vector(x, y, z, a))

  inline def lift[A, N <: Int](v: Vector[A]): Option[Vec[A, N]] =
    inline if (v.length != constValue[N]) {
      None 
    } else {
      Some(new Vec[A, N](v))
    }
    
trait Assert[P, M <: String] {}
implicit inline def isTrue[M <: String]: Assert[true, M] = new Assert[true, M] {}
implicit inline def isFalse[M <: String]: Assert[false, M] = error(constValue[M])


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

object SizedSlice:
  // A slice of a vector with the size known at compile time
  inline final def fromVector[A, S <: Int, E <: Int](n: Vector[A]): SizedSlice[A, E - S] = {
    
    new SizedSlice[A, E - S](n.slice(constValue[S], constValue[E]))
  }

class SizedSlice[+A, N <: Int](inner: Vector[A]):
  // Will blow up at compile time if vector is empty
  inline def head: A = inline if (constValue[N] == 0) error("empty vectors dont have heads") else inner.head

  inline def tail: SizedSlice[A, N - 1] = 
    inline if (constValue[N] == 0) error("empty vectors dont have tails")
    else SizedSlice.fromVector[A, 1, N](inner)
    
  def toVector: Vector[A] = inner
  override def toString = inner.toString()

object UInt8:
  @inline final def apply(n: Byte): UInt8 = new UInt8(n)

  @inline final def apply(n: Int): UInt8 = new UInt8(n.toByte)

class UInt8(val signed: Byte):
  def toByte: Byte = signed

  def toChar: Char = (signed & 0xFF).toChar

  def toShort: Short = (signed & 0xFF).toShort

  def toInt: Int = signed & 0xFF

  def ==(other: UInt8): Boolean = this.signed == other.signed

  def !=(other: UInt8): Boolean = this.signed != other.signed

  def <=(other: UInt8): Boolean = this.toInt <= other.toInt

  def <(other: UInt8): Boolean = this.toInt < other.toInt

  def >=(other: UInt8): Boolean = this.toInt >= other.toInt

  def >(other: UInt8): Boolean = this.toInt > other.toInt

  def +(other: UInt8): UInt8 = UInt8(this.signed + other.signed)

  def -(other: UInt8): UInt8 = UInt8(this.signed - other.signed)

  def *(other: UInt8): UInt8 = UInt8(this.signed * other.signed)

  def /(other: UInt8): UInt8 = UInt8(this.toInt / other.toInt)

  def %(other: UInt8): UInt8 = UInt8(this.toInt % other.toInt)

  def <<(shift: Int): UInt8 = UInt8((signed & 0xff) << (shift & 7))

  def >>(shift: Int): UInt8 = UInt8((signed & 0xff) >>> (shift & 7))

  def >>>(shift: Int): UInt8 = UInt8((signed & 0xff) >>> (shift & 7))

  def &(that: UInt8): UInt8 = UInt8((this.signed & 0xff) & (that.signed & 0xff))

  def |(that: UInt8): UInt8 = UInt8((this.signed & 0xff) | (that.signed & 0xff))

  def ^(that: UInt8): UInt8 = UInt8((this.signed & 0xff) ^ (that.signed & 0xff))

  override def toString: String = toInt.toString

class UInt16(val signed: Char):
  def toByte = signed.toByte

class UInt32(val signed: Int):
  def toByte = signed.toByte


case class CartridgeHeader(logo: Vec[UInt8, 30])

object Main:
  def main(args: Array[String]): Unit =
    println("Hello world!")
    println(msg)

  def msg = "I was compiled by dotty :)"

