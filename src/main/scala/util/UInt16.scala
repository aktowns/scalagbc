package util

extension (c: Int)
  def u16: UInt16 = UInt16(c)

object UInt16:
  final def apply(n: Char): UInt16 = new UInt16(n)
  final def apply(n: Short): UInt16 = new UInt16(n.toChar)
  final def apply(n: Int): UInt16 = new UInt16(n.toChar)
  final def apply(b1: UInt8, b2: UInt8) = 
    new UInt16( ((b1.toByte << 8) | (b2.toByte & 0xFF)).toChar )

class UInt16(val signed: Char):
  def toByte = signed.toByte
  def toChar: Char = signed
  def toShort: Short = signed.toShort
  def toInt: Int = signed.toInt

  def canEqual(a: Any) = a.isInstanceOf[UInt16]

  override def equals(that: Any): Boolean =
    that match
      case that: UInt16 => that.canEqual(this) && that == this
      case _ => false

  override def hashCode: Int = this.signed.hashCode 

  def ==(that: UInt16): Boolean = this.signed == that.signed
  def !=(that: UInt16): Boolean = this.signed != that.signed

  def <=(that: UInt16): Boolean = this.signed <= that.signed
  def <(that: UInt16): Boolean = this.signed < that.signed
  def >=(that: UInt16): Boolean = this.signed >= that.signed
  def >(that: UInt16): Boolean = this.signed > that.signed

  def +(that: UInt16): UInt16 = UInt16(this.signed + that.signed)
  def -(that: UInt16): UInt16 = UInt16(this.signed - that.signed)
  def *(that: UInt16): UInt16 = UInt16(this.signed * that.signed)
  def /(that: UInt16): UInt16 = UInt16(this.signed / that.signed)
  def %(that: UInt16): UInt16 = UInt16(this.signed % that.signed)

  def <<(shift: Int): UInt16 = UInt16((signed & 0xffff) << (shift & 15))
  def >>(shift: Int): UInt16 = UInt16((signed & 0xffff) >>> (shift & 15))
  def >>>(shift: Int): UInt16 = UInt16((signed & 0xffff) >>> (shift & 15))
  def &(that: UInt16): UInt16 = UInt16(this.signed & that.signed)
  def |(that: UInt16): UInt16 = UInt16(this.signed | that.signed)
  def ^(that: UInt16): UInt16 = UInt16(this.signed ^ that.signed)


  override def toString: String = f"0x${toInt}%04x"
