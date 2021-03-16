package gameboy

import cats.free.*
import cats.free.Free.liftF
import cats.{given, *}
import cats.data.*
import cats.effect.IO
import cats.implicits.{given, *}
import monocle.syntax.all._
import util.{UInt16, UInt8}


case class CpuRegisters()

enum CpuA[A]:
  case NextInstruction                  extends CpuA[Instruction]
  case ReadRegister8(r: R)              extends CpuA[UInt8]
  case ReadRegister16(r: R)             extends CpuA[UInt16]
  case WriteRegister8(r: R, v: UInt8)   extends CpuA[Unit]
  case WriteRegister16(r: R, v: Uint16) extends CpuA[Unit]

type Cpu[A] = Free[CpuA, A]

//type CpuStateT[M[_], A] = StateT[M, CpuRegisters, A]

//type CpuStack[A] = CpuT[CpuStateT[IO, *], A]

// def opNOP: Cpu[Unit]                            = liftF[CpuA, Unit](CpuA.OpNOP)
// def opRLCA: Cpu[Unit]                           = liftF[CpuA, Unit](CpuA.OpRLCA)
// def opRRCA: Cpu[Unit]                           = liftF[CpuA, Unit](CpuA.OpRRCA)
// def opRLA: Cpu[Unit]                            = liftF[CpuA, Unit](CpuA.OpRLA)
// def opRRA: Cpu[Unit]                            = liftF[CpuA, Unit](CpuA.OpRRA)
// def opDAA: Cpu[Unit]                            = liftF[CpuA, Unit](CpuA.OpDAA)
// def opCPL: Cpu[Unit]                            = liftF[CpuA, Unit](CpuA.OpCPL)
// def opSCF: Cpu[Unit]                            = liftF[CpuA, Unit](CpuA.OpSCF)
// def opCCF: Cpu[Unit]                            = liftF[CpuA, Unit](CpuA.OpCCF)
// def opHALT: Cpu[Unit]                           = liftF[CpuA, Unit](CpuA.OpHALT)
// def opSTOP: Cpu[Unit]                           = liftF[CpuA, Unit](CpuA.OpSTOP)
// def opRET: Cpu[Unit]                            = liftF[CpuA, Unit](CpuA.OpRET)
// def opRETI: Cpu[Unit]                           = liftF[CpuA, Unit](CpuA.OpRETI)
// def opDI: Cpu[Unit]                             = liftF[CpuA, Unit](CpuA.OpDI)
// def opEI: Cpu[Unit]                             = liftF[CpuA, Unit](CpuA.OpEI)
// def opDECr(r1: R): Cpu[Unit]                    = liftF[CpuA, Unit](CpuA.OpDECr(r1))
// def opINCr(r1: R): Cpu[Unit]                    = liftF[CpuA, Unit](CpuA.OpINCr(r1))
// def opRETr(r1: R): Cpu[Unit]                    = liftF[CpuA, Unit](CpuA.OpRETr(r1))
// def opPUSHr(r1: R): Cpu[Unit]                   = liftF[CpuA, Unit](CpuA.OpPUSHr(r1))
// def opPOPr(r1: R): Cpu[Unit]                    = liftF[CpuA, Unit](CpuA.OpPOPr(r1))
// def opJPr(r1: R): Cpu[Unit]                     = liftF[CpuA, Unit](CpuA.OpJPr(r1))
// def opJPv16(v: UInt16)                          = liftF[CpuA, Unit](CpuA.OpJPv16(v))
// def opJPrv16(r1: R, v: UInt16)                  = liftF[CpuA, Unit](CpuA.OpJPrv16(r1, v))
// def opJPfv16(f: F, v: UInt16)                   = liftF[CpuA, Unit](CpuA.OpJPfv16(f, v))
// def opCALLv16(v: UInt16)                        = liftF[CpuA, Unit](CpuA.OpCALLv16(v))
// def opCALLrv16(r1: R, v: UInt16)                = liftF[CpuA, Unit](CpuA.OpCALLrv16(r1, v))
// def opCALLfv16(f: F, v: UInt16)                 = liftF[CpuA, Unit](CpuA.OpCALLfv16(f, v))
// def opLDrr(r1: R, r2: R): Cpu[Unit]             = liftF[CpuA, Unit](CpuA.OpLDrr(r1, r2))
// def opLDra(r1: R, a: Addr): Cpu[Unit]           = liftF[CpuA, Unit](CpuA.OpLDra(r1, a))
// def opLDar(a: Addr, r1: R): Cpu[Unit]           = liftF[CpuA, Unit](CpuA.OpLDar(a, r1))
// def opLDrv8(r1: R, v: UInt8 | Add): Cpu[Unit]   = liftF[CpuA, Unit](CpuA.OpLDrv8(r1, v))
// def opLDa8r(a: Addr, r1: R): Cpu[Unit]          = liftF[CpuA, Unit](CpuA.OpLDa8r(a, r1))
// def opLDra8(r1: R, a: Addr): Cpu[Unit]          = liftF[CpuA, Unit](CpuA.OpLDra8(r1, a))
// def opLDav8(a: Addr, v: UInt8): Cpu[Unit]       = liftF[CpuA, Unit](CpuA.OpLDav8(a, v))
// def opLDrv16(r1: R, v: UInt16): Cpu[Unit]       = liftF[CpuA, Unit](CpuA.OpLDrv16(r1, v))
// def opLDa16r(a: Addr, r1: R): Cpu[Unit]         = liftF[CpuA, Unit](CpuA.OpLDa16r(a, r1))
// def opLDra16(r1: R, a: Addr): Cpu[Unit]         = liftF[CpuA, Unit](CpuA.OpLDra16(r1, a))
// def opADDrr(r1: R, r2: R): Cpu[Unit]            = liftF[CpuA, Unit](CpuA.OpADDrr(r1, r2))
// def opADDra(r1: R, a: Addr): Cpu[Unit]          = liftF[CpuA, Unit](CpuA.OpADDra(r1, a))
// def opADDrv8(r1: R, v: UInt8 | Byte): Cpu[Unit] = liftF[CpuA, Unit](CpuA.OpADDrv8(r1, v))
// def opADCrr(r1: R, r2: R): Cpu[Unit]            = liftF[CpuA, Unit](CpuA.OpADCrr(r1, r2))
// def opADCra(r1: R, a: Addr): Cpu[Unit]          = liftF[CpuA, Unit](CpuA.OpADCra(r1, a))
// def opADCrv8(r1: R, v: UInt8): Cpu[Unit]        = liftF[CpuA, Unit](CpuA.OpADCrv8(r1, v))
// def opSUBrr(r1: R, r2: R): Cpu[Unit]            = liftF[CpuA, Unit](CpuA.OpSUBrr(r1, r2))
// def opSUBra(r1: R, a: Addr): Cpu[Unit]          = liftF[CpuA, Unit](CpuA.OpSUBra(r1, a))
// def opSUBrv8(r1: R, v: UInt8): Cpu[Unit]        = liftF[CpuA, Unit](CpuA.OpSUBrv8(r1, v))
// def opSBCrr(r1: R, r2: R): Cpu[Unit]            = liftF[CpuA, Unit](CpuA.OpSBCrr(r1, r2))
// def opSBCra(r1: R, a: Addr): Cpu[Unit]          = liftF[CpuA, Unit](CpuA.OpSBCra(r1, a))
// def opSBCrv8(r1: R, v: UInt8): Cpu[Unit]        = liftF[CpuA, Unit](CpuA.OpSBCrv8(r1, v))
// def opORrr(r1: R, r2: R): Cpu[Unit]             = liftF[CpuA, Unit](CpuA.OpORrr(r1, r2))
// def opORra(r1: R, a: Addr): Cpu[Unit]           = liftF[CpuA, Unit](CpuA.OpORra(r1, a))
// def opORrv8(r1: R, v: UInt8): Cpu[Unit]         = liftF[CpuA, Unit](CpuA.OpORrv8(r1, v))
// def opXORrr(r1: R, r2: R): Cpu[Unit]            = liftF[CpuA, Unit](CpuA.OpXORrr(r1, r2))
// def opXORra(r1: R, a: Addr): Cpu[Unit]          = liftF[CpuA, Unit](CpuA.OpXORra(r1, a))
// def opXORrv8(r1: R, v: UInt8): Cpu[Unit]        = liftF[CpuA, Unit](CpuA.OpXORrv8(r1, v))
// def opANDrr(r1: R, r2: R): Cpu[Unit]            = liftF[CpuA, Unit](CpuA.OpANDrr(r1, r2))
// def opANDra(r1: R, a: Addr): Cpu[Unit]          = liftF[CpuA, Unit](CpuA.OpANDra(r1, a))
// def opANDrv8(r1: R, v: UInt8): Cpu[Unit]        = liftF[CpuA, Unit](CpuA.OpANDrv8(r1, v))
// def opCPrr(r1: R, r2: R): Cpu[Unit]             = liftF[CpuA, Unit](CpuA.OpCPrr(r1, r2))
// def opCPra(r1: R, a: Addr): Cpu[Unit]           = liftF[CpuA, Unit](CpuA.OpCPra(r1, a))
// def opCPrv8(r1: R, v: UInt8): Cpu[Unit]         = liftF[CpuA, Unit](CpuA.OpCPrv8(r1, v))
// def opJRv8(v: Byte)                             = liftF[CpuA, Unit](CpuA.OpJRv8(v))
// def opJRrv8(r1: R, v: Byte)                     = liftF[CpuA, Unit](CpuA.OpJRrv8(r1, v))
// def opJRfv8(f: F, v: Byte)                      = liftF[CpuA, Unit](CpuA.OpJRfv8(f, v))
// def opRSTl(v: UInt8)                            = liftF[CpuA, Unit](CpuA.OpRSTl(v))
// def opRETf(f: F)                                = liftF[CpuA, Unit](CpuA.OpRETf(f))
// def opINCa(a: Addr)                             = liftF[CpuA, Unit](CpuA.OpINCa(a))
// def opDECa(a: Addr)                             = liftF[CpuA, Unit](CpuA.OpDECa(a))

// def opJpA(addr: UInt16): Cpu[Unit] = liftF[CpuA, Unit](CpuA.OpJpA(addr))

case class Registers(af: UInt16, bc: UInt16, de: UInt16, hl: UInt16, sp: UInt16, pc: UInt16):
  val a = ???
  val f = ???

// Game disconnected: you were kicked by PunkBuster Stated Reason: PunkBuster kicked player RetroIslandGaming (for 0 minutes)
// You were kicked off the game. (BattlEye: Admin kick for cheating)
// [CAUTION] YOU HAVE BEEN PENALIZED Invalid game data remains present on your account; as such your account has now been penalized to restrict your online experience. The use of invalid data during gameplay is a violation of the online service end user license agreement. Thank you for supporting Dark Souls III
// You were removed from the match due to your IP, VPN, machine, or cheating. We recommend not utilizing VPN or proxy servers while attempting to play fortnite
// def x = 
//   val r = Registers(0.u16, 0.u16, 0.u16, 0.u16, 0.u16, 0.u16)
//   r.focus(af).replace(1.u16) 

// type stack = Rom[Ram[Registers[Id, *], *], A]

def evaluator: CpuA ~> Id = 
  new (CpuA ~> Id) {
    def apply[A](fa: CpuA[A]): Id[A] =
      fa match
        case CpuA.ReadInstruction       => Instruction.NOP
//        case CpuA.OpJPv16(v: UInt16) => setRegister(R.SP, v)
  }

/*
def evaluator: Cpu ~> CpuStateT[IO, *] = new (Cpu ~> CpuStateT[IO, *]) {
  def apply[A](fa: Cpu[A]): CpuStateT[IO, A] =
    fa match
      case Cpu.OpNop => StateT.pure[IO, CpuRegisters, A](println("NOP"))
}
*/
