package gameboy

import cats.free.*
import cats.free.Free.liftF
import cats.{given, *}
import cats.data.*
import cats.effect.IO
import cats.implicits.{given, *}
import monocle.syntax.all._
import monocle.Lens
import util.*

enum CpuA[A]:
  case ReadRegister8(r: R[8])               extends CpuA[UInt8]
  case ReadRegister16(r: R[16])             extends CpuA[UInt16]
  case WriteRegister8(r: R[8], v: UInt8)    extends CpuA[Unit]
  case WriteRegister16(r: R[16], v: UInt16) extends CpuA[Unit]

type Cpu[A] = Free[CpuA, A]
type CpuState[A] = State[Registers, A]

inline def readRegister8(r: R[8]): Cpu[UInt8] = liftF[CpuA, UInt8](CpuA.ReadRegister8(r))
inline def readRegister16(r: R[16]): Cpu[UInt16] = liftF[CpuA, UInt16](CpuA.ReadRegister16(r))
inline def writeRegister8(r: R[8], v: UInt8): Cpu[Unit] = liftF[CpuA, Unit](CpuA.WriteRegister8(r, v))
inline def writeRegister16(r: R[16], v: UInt16): Cpu[Unit] = liftF[CpuA, Unit](CpuA.WriteRegister16(r, v))

// def opADDrr(r1: R[8], r2: R[8]): Cpu[Unit] =
//   for 
//     res1 <- readRegister8(r1)
//     res2 <- readRegister8(r2)
//     _  <- writeRegister8(r1, res1 + res2)
//   yield  ()

//type CpuStateT[M[_], A] = StateT[M, CpuRegisters, A]

//type CpuStack[A] = CpuT[CpuStateT[IO, *], A]

// TODO: Move me to each unsigned type
object LittleEndian: 
  inline def combineU8(b1: UInt8, b2: UInt8): UInt16 = UInt16(((b1.toByte << 8) | (b2.toByte & 0xFF)).toChar)
  inline def splitU16(s1: UInt16): (UInt8, UInt8)    = (UInt8(s1.toShort & 0xff), (UInt8((s1.toShort >> 8) & 0xFF)))

extension (s: UInt16)
  def leastSignificant: UInt8 = UInt8(s.toShort & 0xFF)
  def mostSignificant: UInt8  = UInt8((s.toShort >> 8) & 0xFF)

case class Registers(af: UInt16, bc: UInt16, de: UInt16, hl: UInt16, sp: UInt16, pc: UInt16)

object Registers:
  val empty: Registers = Registers(0.u16, 0.u16,  0.u16,  0.u16, 0.u16, 0.u16)

  val a: Lens[Registers, UInt8]   = Lens[Registers, UInt8](_.af.leastSignificant)(n => a => a.copy(af = UInt16(n, a.af.mostSignificant)))
  val f: Lens[Registers, UInt8]   = Lens[Registers, UInt8](_.af.mostSignificant)(n => a => a.copy(af = UInt16(a.af.leastSignificant, n)))
  val af: Lens[Registers, UInt16] = Lens[Registers, UInt16](_.af)(n => a => a.copy(af = n))

  val b: Lens[Registers, UInt8] = Lens[Registers, UInt8](_.bc.leastSignificant)(n => a => a.copy(bc = UInt16(n, a.bc.mostSignificant)))
  val c: Lens[Registers, UInt8] = Lens[Registers, UInt8](_.bc.mostSignificant)(n => a => a.copy(bc = UInt16(a.bc.leastSignificant, n)))
  val bc: Lens[Registers, UInt16] = Lens[Registers, UInt16](_.bc)(n => a => a.copy(bc = n))

  val d: Lens[Registers, UInt8] = Lens[Registers, UInt8](_.de.leastSignificant)(n => a => a.copy(de = UInt16(n, a.de.mostSignificant)))
  val e: Lens[Registers, UInt8] = Lens[Registers, UInt8](_.de.mostSignificant)(n => a => a.copy(de = UInt16(a.de.leastSignificant, n)))
  val de: Lens[Registers, UInt16] = Lens[Registers, UInt16](_.de)(n => a => a.copy(de = n))

  val h: Lens[Registers, UInt8] = Lens[Registers, UInt8](_.hl.leastSignificant)(n => a => a.copy(hl = UInt16(n, a.hl.mostSignificant)))
  val l: Lens[Registers, UInt8] = Lens[Registers, UInt8](_.hl.mostSignificant)(n => a => a.copy(hl = UInt16(a.hl.leastSignificant, n)))
  val hl: Lens[Registers, UInt16] = Lens[Registers, UInt16](_.hl)(n => a => a.copy(hl = n))

  val sp: Lens[Registers, UInt16] = Lens[Registers, UInt16](_.sp)(n => a => a.copy(sp = n))
  val pc: Lens[Registers, UInt16] = Lens[Registers, UInt16](_.pc)(n => a => a.copy(pc = n))

  def fromR8(r: R[8]): Lens[Registers, UInt8] = 
    r match
      case R.A => Registers.a
      case R.F => Registers.f
      case R.B => Registers.b
      case R.C => Registers.c
      case R.D => Registers.d
      case R.E => Registers.e
      case R.L => Registers.l
      case R.H => Registers.h

  def fromR16(r: R[16]): Lens[Registers, UInt16] = 
    r match
      case R.AF => Registers.af
      case R.BC => Registers.bc
      case R.DE => Registers.de
      case R.HL => Registers.hl
      case R.SP => Registers.sp
      case R.PC => Registers.pc

def evaluator: CpuA ~> CpuState = 
  new (CpuA ~> CpuState) {
    def apply[A](fa: CpuA[A]): CpuState[A] =
      fa match
        case CpuA.ReadRegister8(r)      => State.inspect(Registers.fromR8(r).get)
        case CpuA.ReadRegister16(r)     => State.inspect(Registers.fromR16(r).get)
        case CpuA.WriteRegister8(r, v)  => State.modify(Registers.fromR8(r).replace(v))
        case CpuA.WriteRegister16(r, v) => State.modify(Registers.fromR16(r).replace(v))
  }

