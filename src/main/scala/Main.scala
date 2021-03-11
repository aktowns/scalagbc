import scodec.codecs.uint8

import scala.compiletime.S
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, error, summonInline}

object SizedSlice:
  inline final def fromVector[A, S <: Int, E <: Int](n: Vector[A]): SizedSlice[A, E - S] =
    new SizedSlice[A, E - S](n.slice(constValue[S], constValue[E]))

class SizedSlice[+A, N <: Int](inner: Vector[A]):
  inline def head: A = inline if (constValue[N] == 0) error("empty vectors dont have heads") else inner.head

  inline def tail: SizedSlice[A, N - 1] = 
    inline if (constValue[N] == 0) error("empty vectors dont have tails")
    else SizedSlice.fromVector[A, 1, N](inner)
    
  def toVector: Vector[A] = inner
  override def toString = inner.toString()

def testing(): Unit =
  val x: SizedSlice[UInt8, 30] = ???
  ()

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


case class CartridgeHeader(logo: UInt8)

object Main:
  def main(args: Array[String]): Unit =
    println("Hello world!")
    println(msg)

  def msg = "I was compiled by dotty :)"

