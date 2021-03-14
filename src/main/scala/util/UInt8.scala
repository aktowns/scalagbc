package util

import scala.language.experimental.genericNumberLiterals
import scala.util.FromDigits

extension (c: Int)
  def u8: UInt8 = UInt8(c)

object UInt8:
  @inline final def apply(n: Byte): UInt8 = new UInt8(n)

  @inline final def apply(n: Int): UInt8 = new UInt8(n.toByte)

  def unapply(i: UInt8): Option[Int] = Some(i.toInt)

class UInt8(val signed: Byte):
  def toByte: Byte = signed

  def toChar: Char = (signed & 0xFF).toChar

  def toShort: Short = (signed & 0xFF).toShort

  def toInt: Int = signed & 0xFF

  def canEqual(a: Any) = a.isInstanceOf[UInt8]

  override def equals(that: Any): Boolean =
    that match
      case that: UInt8 => that.canEqual(this) && that == this
      case _ => false

  override def hashCode: Int = this.signed.hashCode 

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

  override def toString: String = f"0x${toInt}%02x"
