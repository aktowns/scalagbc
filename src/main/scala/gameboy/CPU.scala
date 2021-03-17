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


case class CpuRegisters()

enum CpuA[A]:
  case NextInstruction                  extends CpuA[Instruction]
  case ReadRegister8(r: R[8])              extends CpuA[UInt8]
  case ReadRegister16(r: R[16])             extends CpuA[UInt16]
  case WriteRegister8(r: R[8], v: UInt8)   extends CpuA[Unit]
  case WriteRegister16(r: R[16], v: UInt16) extends CpuA[Unit]

type Cpu[A] = Free[CpuA, A]


def opADDrr(r1: R[8], r2: R[8]): Cpu[Unit] =
  for 
    r1 <- readRegister8(r1)
    r2 <- readRegister8(r2)
    _  <- writeRegister(r1, r1 + r2)
  yield  ()

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

// def x = 
//   val r = Registers(0.u16, 0.u16, 0.u16, 0.u16, 0.u16, 0.u16)
//   r.focus(af).replace(1.u16) 

// type stack = Rom[Ram[Registers[Id, *], *], A]

def evaluator: CpuA ~> Id = 
  new (CpuA ~> Id) {
    def apply[A](fa: CpuA[A]): Id[A] =
      fa match
        case CpuA.NextInstruction       => Instruction.NOP
//        case CpuA.OpJPv16(v: UInt16) => setRegister(R.SP, v)
  }

/*
def evaluator: Cpu ~> CpuStateT[IO, *] = new (Cpu ~> CpuStateT[IO, *]) {
  def apply[A](fa: Cpu[A]): CpuStateT[IO, A] =
    fa match
      case Cpu.OpNop => StateT.pure[IO, CpuRegisters, A](println("NOP"))
}
*/
