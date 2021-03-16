package gameboy

import util.{UInt16, UInt8}

enum R:
  case F
  case A
  case AF
  case C
  case B
  case BC
  case E
  case D
  case DE
  case L
  case H
  case HL
  case SP
  case PC

enum F:
  case Z
  case NZ
  case C
  case NC

case class Addr(v: R | UInt16 | UInt8, increment: Boolean = false, decrement: Boolean = false, offset: Int = 0)
case class Add(v: Byte, r: R)

enum Op:
  case U8(str: () => String, op: Instruction)
  case S16(str: Byte => String, op: (Byte) => Instruction)
  case U16(str: UInt8 => String, op: (UInt8) => Instruction)
  case U24(str: UInt16 => String, op: (UInt16) => Instruction)

def decompile(data: Vector[UInt8]): Vector[String] =
  val op = data.headOption.map(_.toInt).map(OpcodeLookup.apply)
  op match
    case None => Vector()
    case Some(Op.U8(f, _))  => f() +: decompile(data.tail)
    case Some(Op.U16(f, _)) => f(data(1)) +: decompile(data.drop(2))
    case Some(Op.S16(f, _)) => f(data(1).toByte) +: decompile(data.drop(2))
    case Some(Op.U24(f, _)) => f(UInt16(data(1), data(2))) +: decompile(data.drop(3))

enum Instruction:
  case NOP                             extends Instruction
  case RLCA                            extends Instruction
  case RRCA                            extends Instruction
  case RLA                             extends Instruction
  case RRA                             extends Instruction
  case DAA                             extends Instruction
  case CPL                             extends Instruction
  case SCF                             extends Instruction
  case CCF                             extends Instruction
  case HALT                            extends Instruction
  case STOP                            extends Instruction
  case RET                             extends Instruction
  case RETI                            extends Instruction
  case DI                              extends Instruction
  case EI                              extends Instruction
  case DECr(r1: R)                     extends Instruction
  case INCr(r1: R)                     extends Instruction
  case RETr(r1: R)                     extends Instruction
  case PUSHr(r1: R)                    extends Instruction
  case POPr(r1: R)                     extends Instruction
  case JPr(r1: R)                      extends Instruction
  case JPv16(v: UInt16)                extends Instruction
  case JPfv16(f: F, v: UInt16)         extends Instruction
  case JPrv16(r1: R, v: UInt16)        extends Instruction
  case CALLv16(v: UInt16)              extends Instruction
  case CALLfv16(f: F, v: UInt16)       extends Instruction
  case CALLrv16(r1: R, v: UInt16)      extends Instruction
  case LDrr(r1: R, r2: R)              extends Instruction
  case LDra(r1: R, a: Addr)            extends Instruction
  case LDar(a: Addr, r1: R)            extends Instruction
  case LDrv8(r1: R, v: UInt8 | Add)    extends Instruction
  case LDa8r(a: Addr, r1: R)           extends Instruction
  case LDra8(r1: R, a: Addr)           extends Instruction
  case LDav8(a: Addr, v: UInt8)        extends Instruction
  case LDrv16(r1: R, v: UInt16)        extends Instruction
  case LDa16r(a: Addr, r1: R)          extends Instruction
  case LDra16(r1: R, a: Addr)          extends Instruction
  case ADDrr(r1: R, r2: R)             extends Instruction
  case ADDra(r1: R, a: Addr)           extends Instruction
  case ADDrv8(r1: R, v: UInt8 | Byte)  extends Instruction
  case ADCrr(r1: R, r2: R)             extends Instruction
  case ADCra(r1: R, a: Addr)           extends Instruction
  case ADCrv8(r1: R, v: UInt8)         extends Instruction
  case SUBrr(r1: R, r2: R)             extends Instruction
  case SUBra(r1: R, a: Addr)           extends Instruction
  case SUBrv8(r1: R, v: UInt8)         extends Instruction
  case SBCrr(r1: R, r2: R)             extends Instruction
  case SBCra(r1: R, a: Addr)           extends Instruction
  case SBCrv8(r1: R, v: UInt8)         extends Instruction
  case ORrr(r1: R, r2: R)              extends Instruction
  case ORra(r1: R, a: Addr)            extends Instruction
  case ORrv8(r1: R, v: UInt8)          extends Instruction
  case XORrr(r1: R, r2: R)             extends Instruction
  case XORra(r1: R, a: Addr)           extends Instruction
  case XORrv8(r1: R, v: UInt8)         extends Instruction
  case ANDrr(r1: R, r2: R)             extends Instruction
  case ANDra(r1: R, a: Addr)           extends Instruction
  case ANDrv8(r1: R, v: UInt8)         extends Instruction
  case CPrr(r1: R, r2: R)              extends Instruction
  case CPra(r1: R, a: Addr)            extends Instruction
  case CPrv8(r1: R, v: UInt8)          extends Instruction
  case JRv8(v: Byte)                   extends Instruction
  case JRrv8(r1: R, v: Byte)           extends Instruction
  case JRfv8(f: F, v: Byte)            extends Instruction
  case RSTl(v: UInt8)                  extends Instruction
  case RETf(f: F)                      extends Instruction
  case INCa(a: Addr)                   extends Instruction
  case DECa(a: Addr)                   extends Instruction

val OpcodeLookup: Map[Int, Op] = Map(
  0x00 -> Op.U8(() => "NOP", Instruction.NOP),
  0x01 -> Op.U24((x: UInt16) => f"LD BC,${x}", (x: UInt16) => Instruction.LDrv16(R.BC, x : UInt16)),
  0x02 -> Op.U8(() => "LD (BC),A", Instruction.LDar(Addr(R.BC), R.A)),
  0x03 -> Op.U8(() => "INC BC", Instruction.INCr(R.BC)),
  0x04 -> Op.U8(() => "INC B", Instruction.INCr(R.B)),
  0x05 -> Op.U8(() => "DEC B", Instruction.DECr(R.B)),
  0x06 -> Op.U16((x: UInt8) => f"LD B,${x}", (x: UInt8) => Instruction.LDrv8(R.B, x : UInt8)),
  0x07 -> Op.U8(() => "RLCA", Instruction.RLCA),
  0x08 -> Op.U24((x: UInt16) => f"LD (${x}),SP", (x: UInt16) => Instruction.LDa16r(Addr(x : UInt16), R.SP)),
  0x09 -> Op.U8(() => "ADD HL,BC", Instruction.ADDrr(R.HL, R.BC)),
  0x0a -> Op.U8(() => "LD A,(BC)", Instruction.LDra(R.A, Addr(R.BC))),
  0x0b -> Op.U8(() => "DEC BC", Instruction.DECr(R.BC)),
  0x0c -> Op.U8(() => "INC C", Instruction.INCr(R.C)),
  0x0d -> Op.U8(() => "DEC C", Instruction.DECr(R.C)),
  0x0e -> Op.U16((x: UInt8) => f"LD C,${x}", (x: UInt8) => Instruction.LDrv8(R.C, x : UInt8)),
  0x0f -> Op.U8(() => "RRCA", Instruction.RRCA),
  0x10 -> Op.U16((x: UInt8) => "STOP", (x: UInt8) => Instruction.STOP),
  0x11 -> Op.U24((x: UInt16) => f"LD DE,${x}", (x: UInt16) => Instruction.LDrv16(R.DE, x : UInt16)),
  0x12 -> Op.U8(() => "LD (DE),A", Instruction.LDar(Addr(R.DE), R.A)),
  0x13 -> Op.U8(() => "INC DE", Instruction.INCr(R.DE)),
  0x14 -> Op.U8(() => "INC D", Instruction.INCr(R.D)),
  0x15 -> Op.U8(() => "DEC D", Instruction.DECr(R.D)),
  0x16 -> Op.U16((x: UInt8) => f"LD D,${x}", (x: UInt8) => Instruction.LDrv8(R.D, x : UInt8)),
  0x17 -> Op.U8(() => "RLA", Instruction.RLA),
  0x18 -> Op.S16((x: Byte) => f"JR $x%2x", (x: Byte) => Instruction.JRv8(x : Byte)),
  0x19 -> Op.U8(() => "ADD HL,DE", Instruction.ADDrr(R.HL, R.DE)),
  0x1a -> Op.U8(() => "LD A,(DE)", Instruction.LDra(R.A, Addr(R.DE))),
  0x1b -> Op.U8(() => "DEC DE", Instruction.DECr(R.DE)),
  0x1c -> Op.U8(() => "INC E", Instruction.INCr(R.E)),
  0x1d -> Op.U8(() => "DEC E", Instruction.DECr(R.E)),
  0x1e -> Op.U16((x: UInt8) => f"LD E,${x}", (x: UInt8) => Instruction.LDrv8(R.E, x : UInt8)),
  0x1f -> Op.U8(() => "RRA", Instruction.RRA),
  0x20 -> Op.S16((x: Byte) => f"JR NZ,$x%2x", (x: Byte) => Instruction.JRfv8(F.NZ, x : Byte)),
  0x21 -> Op.U24((x: UInt16) => f"LD HL,${x}", (x: UInt16) => Instruction.LDrv16(R.HL, x : UInt16)),
  0x22 -> Op.U8(() => "LD (HL+),A", Instruction.LDar(Addr(R.HL, increment = true), R.A)),
  0x23 -> Op.U8(() => "INC HL", Instruction.INCr(R.HL)),
  0x24 -> Op.U8(() => "INC H", Instruction.INCr(R.H)),
  0x25 -> Op.U8(() => "DEC H", Instruction.DECr(R.H)),
  0x26 -> Op.U16((x: UInt8) => f"LD H,${x}", (x: UInt8) => Instruction.LDrv8(R.H, x : UInt8)),
  0x27 -> Op.U8(() => "DAA", Instruction.DAA),
  0x28 -> Op.S16((x: Byte) => f"JR Z,$x%2x", (x: Byte) => Instruction.JRfv8(F.Z, x : Byte)),
  0x29 -> Op.U8(() => "ADD HL,HL", Instruction.ADDrr(R.HL, R.HL)),
  0x2a -> Op.U8(() => "LD A,(HL+)", Instruction.LDra(R.A, Addr(R.HL, increment = true))),
  0x2b -> Op.U8(() => "DEC HL", Instruction.DECr(R.HL)),
  0x2c -> Op.U8(() => "INC L", Instruction.INCr(R.L)),
  0x2d -> Op.U8(() => "DEC L", Instruction.DECr(R.L)),
  0x2e -> Op.U16((x: UInt8) => f"LD L,${x}", (x: UInt8) => Instruction.LDrv8(R.L, x : UInt8)),
  0x2f -> Op.U8(() => "CPL", Instruction.CPL),
  0x30 -> Op.S16((x: Byte) => f"JR NC,$x%2x", (x: Byte) => Instruction.JRfv8(F.NC, x : Byte)),
  0x31 -> Op.U24((x: UInt16) => f"LD SP,${x}", (x: UInt16) => Instruction.LDrv16(R.SP, x : UInt16)),
  0x32 -> Op.U8(() => "LD (HL-),A", Instruction.LDar(Addr(R.HL, decrement = true), R.A)),
  0x33 -> Op.U8(() => "INC SP", Instruction.INCr(R.SP)),
  0x34 -> Op.U8(() => "INC (HL)", Instruction.INCa(Addr(R.HL))),
  0x35 -> Op.U8(() => "DEC (HL)", Instruction.DECa(Addr(R.HL))),
  0x36 -> Op.U16((x: UInt8) => f"LD (HL),${x}", (x: UInt8) => Instruction.LDav8(Addr(R.HL), x : UInt8)),
  0x37 -> Op.U8(() => "SCF", Instruction.SCF),
  0x38 -> Op.S16((x: Byte) => f"JR C,$x%2x", (x: Byte) => Instruction.JRrv8(R.C, x : Byte)),
  0x39 -> Op.U8(() => "ADD HL,SP", Instruction.ADDrr(R.HL, R.SP)),
  0x3a -> Op.U8(() => "LD A,(HL-)", Instruction.LDra(R.A, Addr(R.HL, decrement = true))),
  0x3b -> Op.U8(() => "DEC SP", Instruction.DECr(R.SP)),
  0x3c -> Op.U8(() => "INC A", Instruction.INCr(R.A)),
  0x3d -> Op.U8(() => "DEC A", Instruction.DECr(R.A)),
  0x3e -> Op.U16((x: UInt8) => f"LD A,${x}", (x: UInt8) => Instruction.LDrv8(R.A, x : UInt8)),
  0x3f -> Op.U8(() => "CCF", Instruction.CCF),
  0x40 -> Op.U8(() => "LD B,B", Instruction.LDrr(R.B, R.B)),
  0x41 -> Op.U8(() => "LD B,C", Instruction.LDrr(R.B, R.C)),
  0x42 -> Op.U8(() => "LD B,D", Instruction.LDrr(R.B, R.D)),
  0x43 -> Op.U8(() => "LD B,E", Instruction.LDrr(R.B, R.E)),
  0x44 -> Op.U8(() => "LD B,H", Instruction.LDrr(R.B, R.H)),
  0x45 -> Op.U8(() => "LD B,L", Instruction.LDrr(R.B, R.L)),
  0x46 -> Op.U8(() => "LD B,(HL)", Instruction.LDra(R.B, Addr(R.HL))),
  0x47 -> Op.U8(() => "LD B,A", Instruction.LDrr(R.B, R.A)),
  0x48 -> Op.U8(() => "LD C,B", Instruction.LDrr(R.C, R.B)),
  0x49 -> Op.U8(() => "LD C,C", Instruction.LDrr(R.C, R.C)),
  0x4a -> Op.U8(() => "LD C,D", Instruction.LDrr(R.C, R.D)),
  0x4b -> Op.U8(() => "LD C,E", Instruction.LDrr(R.C, R.E)),
  0x4c -> Op.U8(() => "LD C,H", Instruction.LDrr(R.C, R.H)),
  0x4d -> Op.U8(() => "LD C,L", Instruction.LDrr(R.C, R.L)),
  0x4e -> Op.U8(() => "LD C,(HL)", Instruction.LDra(R.C, Addr(R.HL))),
  0x4f -> Op.U8(() => "LD C,A", Instruction.LDrr(R.C, R.A)),
  0x50 -> Op.U8(() => "LD D,B", Instruction.LDrr(R.D, R.B)),
  0x51 -> Op.U8(() => "LD D,C", Instruction.LDrr(R.D, R.C)),
  0x52 -> Op.U8(() => "LD D,D", Instruction.LDrr(R.D, R.D)),
  0x53 -> Op.U8(() => "LD D,E", Instruction.LDrr(R.D, R.E)),
  0x54 -> Op.U8(() => "LD D,H", Instruction.LDrr(R.D, R.H)),
  0x55 -> Op.U8(() => "LD D,L", Instruction.LDrr(R.D, R.L)),
  0x56 -> Op.U8(() => "LD D,(HL)", Instruction.LDra(R.D, Addr(R.HL))),
  0x57 -> Op.U8(() => "LD D,A", Instruction.LDrr(R.D, R.A)),
  0x58 -> Op.U8(() => "LD E,B", Instruction.LDrr(R.E, R.B)),
  0x59 -> Op.U8(() => "LD E,C", Instruction.LDrr(R.E, R.C)),
  0x5a -> Op.U8(() => "LD E,D", Instruction.LDrr(R.E, R.D)),
  0x5b -> Op.U8(() => "LD E,E", Instruction.LDrr(R.E, R.E)),
  0x5c -> Op.U8(() => "LD E,H", Instruction.LDrr(R.E, R.H)),
  0x5d -> Op.U8(() => "LD E,L", Instruction.LDrr(R.E, R.L)),
  0x5e -> Op.U8(() => "LD E,(HL)", Instruction.LDra(R.E, Addr(R.HL))),
  0x5f -> Op.U8(() => "LD E,A", Instruction.LDrr(R.E, R.A)),
  0x60 -> Op.U8(() => "LD H,B", Instruction.LDrr(R.H, R.B)),
  0x61 -> Op.U8(() => "LD H,C", Instruction.LDrr(R.H, R.C)),
  0x62 -> Op.U8(() => "LD H,D", Instruction.LDrr(R.H, R.D)),
  0x63 -> Op.U8(() => "LD H,E", Instruction.LDrr(R.H, R.E)),
  0x64 -> Op.U8(() => "LD H,H", Instruction.LDrr(R.H, R.H)),
  0x65 -> Op.U8(() => "LD H,L", Instruction.LDrr(R.H, R.L)),
  0x66 -> Op.U8(() => "LD H,(HL)", Instruction.LDra(R.H, Addr(R.HL))),
  0x67 -> Op.U8(() => "LD H,A", Instruction.LDrr(R.H, R.A)),
  0x68 -> Op.U8(() => "LD L,B", Instruction.LDrr(R.L, R.B)),
  0x69 -> Op.U8(() => "LD L,C", Instruction.LDrr(R.L, R.C)),
  0x6a -> Op.U8(() => "LD L,D", Instruction.LDrr(R.L, R.D)),
  0x6b -> Op.U8(() => "LD L,E", Instruction.LDrr(R.L, R.E)),
  0x6c -> Op.U8(() => "LD L,H", Instruction.LDrr(R.L, R.H)),
  0x6d -> Op.U8(() => "LD L,L", Instruction.LDrr(R.L, R.L)),
  0x6e -> Op.U8(() => "LD L,(HL)", Instruction.LDra(R.L, Addr(R.HL))),
  0x6f -> Op.U8(() => "LD L,A", Instruction.LDrr(R.L, R.A)),
  0x70 -> Op.U8(() => "LD (HL),B", Instruction.LDar(Addr(R.HL), R.B)),
  0x71 -> Op.U8(() => "LD (HL),C", Instruction.LDar(Addr(R.HL), R.C)),
  0x72 -> Op.U8(() => "LD (HL),D", Instruction.LDar(Addr(R.HL), R.D)),
  0x73 -> Op.U8(() => "LD (HL),E", Instruction.LDar(Addr(R.HL), R.E)),
  0x74 -> Op.U8(() => "LD (HL),H", Instruction.LDar(Addr(R.HL), R.H)),
  0x75 -> Op.U8(() => "LD (HL),L", Instruction.LDar(Addr(R.HL), R.L)),
  0x76 -> Op.U8(() => "HALT", Instruction.HALT),
  0x77 -> Op.U8(() => "LD (HL),A", Instruction.LDar(Addr(R.HL), R.A)),
  0x78 -> Op.U8(() => "LD A,B", Instruction.LDrr(R.A, R.B)),
  0x79 -> Op.U8(() => "LD A,C", Instruction.LDrr(R.A, R.C)),
  0x7a -> Op.U8(() => "LD A,D", Instruction.LDrr(R.A, R.D)),
  0x7b -> Op.U8(() => "LD A,E", Instruction.LDrr(R.A, R.E)),
  0x7c -> Op.U8(() => "LD A,H", Instruction.LDrr(R.A, R.H)),
  0x7d -> Op.U8(() => "LD A,L", Instruction.LDrr(R.A, R.L)),
  0x7e -> Op.U8(() => "LD A,(HL)", Instruction.LDra(R.A, Addr(R.HL))),
  0x7f -> Op.U8(() => "LD A,A", Instruction.LDrr(R.A, R.A)),
  0x80 -> Op.U8(() => "ADD A,B", Instruction.ADDrr(R.A, R.B)),
  0x81 -> Op.U8(() => "ADD A,C", Instruction.ADDrr(R.A, R.C)),
  0x82 -> Op.U8(() => "ADD A,D", Instruction.ADDrr(R.A, R.D)),
  0x83 -> Op.U8(() => "ADD A,E", Instruction.ADDrr(R.A, R.E)),
  0x84 -> Op.U8(() => "ADD A,H", Instruction.ADDrr(R.A, R.H)),
  0x85 -> Op.U8(() => "ADD A,L", Instruction.ADDrr(R.A, R.L)),
  0x86 -> Op.U8(() => "ADD A,(HL)", Instruction.ADDra(R.A, Addr(R.HL))),
  0x87 -> Op.U8(() => "ADD A,A", Instruction.ADDrr(R.A, R.A)),
  0x88 -> Op.U8(() => "ADC A,B", Instruction.ADCrr(R.A, R.B)),
  0x89 -> Op.U8(() => "ADC A,C", Instruction.ADCrr(R.A, R.C)),
  0x8a -> Op.U8(() => "ADC A,D", Instruction.ADCrr(R.A, R.D)),
  0x8b -> Op.U8(() => "ADC A,E", Instruction.ADCrr(R.A, R.E)),
  0x8c -> Op.U8(() => "ADC A,H", Instruction.ADCrr(R.A, R.H)),
  0x8d -> Op.U8(() => "ADC A,L", Instruction.ADCrr(R.A, R.L)),
  0x8e -> Op.U8(() => "ADC A,(HL)", Instruction.ADCra(R.A, Addr(R.HL))),
  0x8f -> Op.U8(() => "ADC A,A", Instruction.ADCrr(R.A, R.A)),
  0x90 -> Op.U8(() => "SUB A,B", Instruction.SUBrr(R.A, R.B)),
  0x91 -> Op.U8(() => "SUB A,C", Instruction.SUBrr(R.A, R.C)),
  0x92 -> Op.U8(() => "SUB A,D", Instruction.SUBrr(R.A, R.D)),
  0x93 -> Op.U8(() => "SUB A,E", Instruction.SUBrr(R.A, R.E)),
  0x94 -> Op.U8(() => "SUB A,H", Instruction.SUBrr(R.A, R.H)),
  0x95 -> Op.U8(() => "SUB A,L", Instruction.SUBrr(R.A, R.L)),
  0x96 -> Op.U8(() => "SUB A,(HL)", Instruction.SUBra(R.A, Addr(R.HL))),
  0x97 -> Op.U8(() => "SUB A,A", Instruction.SUBrr(R.A, R.A)),
  0x98 -> Op.U8(() => "SBC A,B", Instruction.SBCrr(R.A, R.B)),
  0x99 -> Op.U8(() => "SBC A,C", Instruction.SBCrr(R.A, R.C)),
  0x9a -> Op.U8(() => "SBC A,D", Instruction.SBCrr(R.A, R.D)),
  0x9b -> Op.U8(() => "SBC A,E", Instruction.SBCrr(R.A, R.E)),
  0x9c -> Op.U8(() => "SBC A,H", Instruction.SBCrr(R.A, R.H)),
  0x9d -> Op.U8(() => "SBC A,L", Instruction.SBCrr(R.A, R.L)),
  0x9e -> Op.U8(() => "SBC A,(HL)", Instruction.SBCra(R.A, Addr(R.HL))),
  0x9f -> Op.U8(() => "SBC A,A", Instruction.SBCrr(R.A, R.A)),
  0xa0 -> Op.U8(() => "AND A,B", Instruction.ANDrr(R.A, R.B)),
  0xa1 -> Op.U8(() => "AND A,C", Instruction.ANDrr(R.A, R.C)),
  0xa2 -> Op.U8(() => "AND A,D", Instruction.ANDrr(R.A, R.D)),
  0xa3 -> Op.U8(() => "AND A,E", Instruction.ANDrr(R.A, R.E)),
  0xa4 -> Op.U8(() => "AND A,H", Instruction.ANDrr(R.A, R.H)),
  0xa5 -> Op.U8(() => "AND A,L", Instruction.ANDrr(R.A, R.L)),
  0xa6 -> Op.U8(() => "AND A,(HL)", Instruction.ANDra(R.A, Addr(R.HL))),
  0xa7 -> Op.U8(() => "AND A,A", Instruction.ANDrr(R.A, R.A)),
  0xa8 -> Op.U8(() => "XOR A,B", Instruction.XORrr(R.A, R.B)),
  0xa9 -> Op.U8(() => "XOR A,C", Instruction.XORrr(R.A, R.C)),
  0xaa -> Op.U8(() => "XOR A,D", Instruction.XORrr(R.A, R.D)),
  0xab -> Op.U8(() => "XOR A,E", Instruction.XORrr(R.A, R.E)),
  0xac -> Op.U8(() => "XOR A,H", Instruction.XORrr(R.A, R.H)),
  0xad -> Op.U8(() => "XOR A,L", Instruction.XORrr(R.A, R.L)),
  0xae -> Op.U8(() => "XOR A,(HL)", Instruction.XORra(R.A, Addr(R.HL))),
  0xaf -> Op.U8(() => "XOR A,A", Instruction.XORrr(R.A, R.A)),
  0xb0 -> Op.U8(() => "OR A,B", Instruction.ORrr(R.A, R.B)),
  0xb1 -> Op.U8(() => "OR A,C", Instruction.ORrr(R.A, R.C)),
  0xb2 -> Op.U8(() => "OR A,D", Instruction.ORrr(R.A, R.D)),
  0xb3 -> Op.U8(() => "OR A,E", Instruction.ORrr(R.A, R.E)),
  0xb4 -> Op.U8(() => "OR A,H", Instruction.ORrr(R.A, R.H)),
  0xb5 -> Op.U8(() => "OR A,L", Instruction.ORrr(R.A, R.L)),
  0xb6 -> Op.U8(() => "OR A,(HL)", Instruction.ORra(R.A, Addr(R.HL))),
  0xb7 -> Op.U8(() => "OR A,A", Instruction.ORrr(R.A, R.A)),
  0xb8 -> Op.U8(() => "CP A,B", Instruction.CPrr(R.A, R.B)),
  0xb9 -> Op.U8(() => "CP A,C", Instruction.CPrr(R.A, R.C)),
  0xba -> Op.U8(() => "CP A,D", Instruction.CPrr(R.A, R.D)),
  0xbb -> Op.U8(() => "CP A,E", Instruction.CPrr(R.A, R.E)),
  0xbc -> Op.U8(() => "CP A,H", Instruction.CPrr(R.A, R.H)),
  0xbd -> Op.U8(() => "CP A,L", Instruction.CPrr(R.A, R.L)),
  0xbe -> Op.U8(() => "CP A,(HL)", Instruction.CPra(R.A, Addr(R.HL))),
  0xbf -> Op.U8(() => "CP A,A", Instruction.CPrr(R.A, R.A)),
  0xc0 -> Op.U8(() => "RET NZ", Instruction.RETf(F.NZ)),
  0xc1 -> Op.U8(() => "POP BC", Instruction.POPr(R.BC)),
  0xc2 -> Op.U24((x: UInt16) => f"JP NZ,${x}", (x: UInt16) => Instruction.JPfv16(F.NZ, x : UInt16)),
  0xc3 -> Op.U24((x: UInt16) => f"JP ${x}", (x: UInt16) => Instruction.JPv16(x : UInt16)),
  0xc4 -> Op.U24((x: UInt16) => f"CALL NZ,${x}", (x: UInt16) => Instruction.CALLfv16(F.NZ, x : UInt16)),
  0xc5 -> Op.U8(() => "PUSH BC", Instruction.PUSHr(R.BC)),
  0xc6 -> Op.U16((x: UInt8) => f"ADD A,${x}", (x: UInt8) => Instruction.ADDrv8(R.A, x : UInt8)),
  0xc7 -> Op.U8(() => "RST 00h", Instruction.RSTl(UInt8(0x00))),
  0xc8 -> Op.U8(() => "RET Z", Instruction.RETf(F.Z)),
  0xc9 -> Op.U8(() => "RET", Instruction.RET),
  0xca -> Op.U24((x: UInt16) => f"JP Z,${x}", (x: UInt16) => Instruction.JPfv16(F.Z, x : UInt16)),
  // 0xcb PREFIX
  0xcc -> Op.U24((x: UInt16) => f"CALL Z,${x}", (x: UInt16) => Instruction.CALLfv16(F.Z, x : UInt16)),
  0xcd -> Op.U24((x: UInt16) => f"CALL ${x}", (x: UInt16) => Instruction.CALLv16(x : UInt16)),
  0xce -> Op.U16((x: UInt8) => f"ADC A,${x}", (x: UInt8) => Instruction.ADCrv8(R.A, x : UInt8)),
  0xcf -> Op.U8(() => "RST 08h", Instruction.RSTl(UInt8(0x08))),
  0xd0 -> Op.U8(() => "RET NC", Instruction.RETf(F.NC)),
  0xd1 -> Op.U8(() => "POP DE", Instruction.POPr(R.DE)),
  0xd2 -> Op.U24((x: UInt16) => f"JP NC,${x}", (x: UInt16) => Instruction.JPfv16(F.NC, x : UInt16)),
  // 0xd3 is unused
  0xd4 -> Op.U24((x: UInt16) => f"CALL NC,${x}", (x: UInt16) => Instruction.CALLfv16(F.NC, x : UInt16)),
  0xd5 -> Op.U8(() => "PUSH DE", Instruction.PUSHr(R.DE)),
  0xd6 -> Op.U16((x: UInt8) => f"SUB A,${x}", (x: UInt8) => Instruction.SUBrv8(R.A, x : UInt8)),
  0xd7 -> Op.U8(() => "RST 10h", Instruction.RSTl(UInt8(0x10))),
  0xd8 -> Op.U8(() => "RET C", Instruction.RETr(R.C)),
  0xd9 -> Op.U8(() => "RETI", Instruction.RETI),
  0xda -> Op.U24((x: UInt16) => f"JP C,${x}", (x: UInt16) => Instruction.JPrv16(R.C, x : UInt16)),
  // 0xdb is unused
  0xdc -> Op.U24((x: UInt16) => f"CALL C,${x}", (x: UInt16) => Instruction.CALLrv16(R.C, x : UInt16)),
  // 0xdd is unused
  0xde -> Op.U16((x: UInt8) => f"SBC A,${x}", (x: UInt8) => Instruction.SBCrv8(R.A, x : UInt8)),
  0xdf -> Op.U8(() => "RST 18h", Instruction.RSTl(UInt8(0x18))),
  0xe0 -> Op.U16((x: UInt8) => f"LD (FF00+${x}),A", (x: UInt8) => Instruction.LDa8r(Addr(x : UInt8, offset = 0xFF00), R.A)),
  0xe1 -> Op.U8(() => "POP HL", Instruction.POPr(R.HL)),
  0xe2 -> Op.U8(() => "LD (FF00+C),A", Instruction.LDar(Addr(R.C, offset = 0xFF00), R.A)),
  // 0xe3 is unused
  // 0xe4 is unused
  0xe5 -> Op.U8(() => "PUSH HL", Instruction.PUSHr(R.HL)),
  0xe6 -> Op.U16((x: UInt8) => f"AND A,${x}", (x: UInt8) => Instruction.ANDrv8(R.A, x : UInt8)),
  0xe7 -> Op.U8(() => "RST 20h", Instruction.RSTl(UInt8(0x20))),
  0xe8 -> Op.S16((x: Byte) => f"ADD SP,$x%2x", (x: Byte) => Instruction.ADDrv8(R.SP, x : Byte)),
  0xe9 -> Op.U8(() => "JP HL", Instruction.JPr(R.HL)),
  0xea -> Op.U24((x: UInt16) => f"LD (${x}),A", (x: UInt16) => Instruction.LDa16r(Addr(x : UInt16), R.A)),
  // 0xeb is unused
  // 0xec is unused
  // 0xed is unused
  0xee -> Op.U16((x: UInt8) => f"XOR A,${x}", (x: UInt8) => Instruction.XORrv8(R.A, x : UInt8)),
  0xef -> Op.U8(() => "RST 28h", Instruction.RSTl(UInt8(0x28))),
  0xf0 -> Op.U16((x: UInt8) => f"LD A,(FF00+${x})", (x: UInt8) => Instruction.LDra8(R.A, Addr(x : UInt8, offset = 0xFF00))),
  0xf1 -> Op.U8(() => "POP AF", Instruction.POPr(R.AF)),
  0xf2 -> Op.U8(() => "LD A,(FF00+C)", Instruction.LDra(R.A, Addr(R.C, offset = 0xFF00))),
  0xf3 -> Op.U8(() => "DI", Instruction.DI),
  // 0xf4 is unused
  0xf5 -> Op.U8(() => "PUSH AF", Instruction.PUSHr(R.AF)),
  0xf6 -> Op.U16((x: UInt8) => f"OR A,${x}", (x: UInt8) => Instruction.ORrv8(R.A, x : UInt8)),
  0xf7 -> Op.U8(() => "RST 30h", Instruction.RSTl(UInt8(0x30))),
  0xf8 -> Op.S16((x: Byte) => f"LD HL,SP+$x%2x", (x: Byte) => Instruction.LDrv8(R.HL, Add(x : Byte, R.SP))),
  0xf9 -> Op.U8(() => "LD SP,HL", Instruction.LDrr(R.SP, R.HL)),
  0xfa -> Op.U24((x: UInt16) => f"LD A,(${x})", (x: UInt16) => Instruction.LDra16(R.A, Addr(x : UInt16))),
  0xfb -> Op.U8(() => "EI", Instruction.EI),
  // 0xfc is unused
  // 0xfd is unused
  0xfe -> Op.U16((x: UInt8) => f"CP A,${x}", (x: UInt8) => Instruction.CPrv8(R.A, x : UInt8)),
  0xff -> Op.U8(() => "RST 38h", Instruction.RSTl(UInt8(0x38))),
)
