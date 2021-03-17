package gameboy

import util.{UInt16, UInt8}

enum R[SZ <: Int]:
  case F   extends R[8]
  case A   extends R[8]
  case AF  extends R[16]
  case C   extends R[8]
  case B   extends R[8]
  case BC  extends R[16]
  case E   extends R[8]
  case D   extends R[8]
  case DE  extends R[16]
  case L   extends R[8]
  case H   extends R[8]
  case HL  extends R[16]
  case SP  extends R[16]
  case PC  extends R[16]

// R but i don't care about the size, pronounced ARRRRRR 
// like ARRRRRR why im spending all my time on stupid shit like sized registers
type RR = R[8 | 16]

enum F:
  case Z
  case NZ
  case C
  case NC

case class Addr(v: R[8] | R[16] | UInt16 | UInt8, increment: Boolean = false, decrement: Boolean = false, offset: Int = 0)
case class Add(v: Byte, r: R[8] | R[16])

enum Op:
  case U8(str: String, op: Instruction)
  case S16(str: Byte => String, op: (Byte) => Instruction)
  case U16(str: UInt8 => String, op: (UInt8) => Instruction)
  case U24(str: UInt16 => String, op: (UInt16) => Instruction)

def decompile(data: Vector[UInt8]): Vector[String] =
  val op = data.headOption.map(_.toInt).map(OpcodeLookup.apply)
  op match
    case None => Vector()
    case Some(Op.U8(f, _))  => f +: decompile(data.tail)
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
  case DECr(r1: R[8] | R[16])          extends Instruction
  case INCr(r1: R[8] | R[16])          extends Instruction
  case RETr(r1: R[8])                  extends Instruction
  case PUSHr(r1: R[16])                extends Instruction
  case POPr(r1: R[16])                 extends Instruction
  case JPr(r1: R[16])                  extends Instruction
  case JPv16(v: UInt16)                extends Instruction
  case JPfv16(f: F, v: UInt16)         extends Instruction
  case JPrv16(r1: R[8], v: UInt16)     extends Instruction
  case CALLv16(v: UInt16)              extends Instruction
  case CALLfv16(f: F, v: UInt16)       extends Instruction
  case CALLrv16(r1: R[8], v: UInt16)   extends Instruction
  case LDrr[N <: 8 | 16](r1: R[N], r2: R[N])        extends Instruction
  case LDra(r1: R[8], a: Addr)         extends Instruction
  case LDar(a: Addr, r1: R[8])         extends Instruction
  case LDrv8(r1: R[8] | R[16], v: UInt8 | Add)    extends Instruction
  case LDa8r(a: Addr, r1: R[8])        extends Instruction
  case LDra8(r1: R[8], a: Addr)        extends Instruction
  case LDav8(a: Addr, v: UInt8)        extends Instruction
  case LDrv16(r1: R[16], v: UInt16)    extends Instruction
  case LDa16r(a: Addr, r1: R[8] | R[16]) extends Instruction
  case LDra16(r1: R[8], a: Addr)       extends Instruction
  case ADDrr(r1: R[8] | R[16], r2: R[8] | R[16])             extends Instruction
  case ADDra(r1: R[8], a: Addr)           extends Instruction
  case ADDrv8(r1: R[8] | R[16], v: UInt8 | Byte) extends Instruction
  case ADCrr(r1: R[8], r2: R[8])       extends Instruction
  case ADCra(r1: R[8], a: Addr)           extends Instruction
  case ADCrv8(r1: R[8], v: UInt8)      extends Instruction
  case SUBrr(r1: R[8], r2: R[8])       extends Instruction
  case SUBra(r1: R[8], a: Addr)           extends Instruction
  case SUBrv8(r1: R[8], v: UInt8)      extends Instruction
  case SBCrr(r1: R[8], r2: R[8])       extends Instruction
  case SBCra(r1: R[8], a: Addr)           extends Instruction
  case SBCrv8(r1: R[8], v: UInt8)      extends Instruction
  case ORrr(r1: R[8], r2: R[8])        extends Instruction
  case ORra(r1: R[8], a: Addr)            extends Instruction
  case ORrv8(r1: R[8], v: UInt8)       extends Instruction
  case XORrr(r1: R[8], r2: R[8])       extends Instruction
  case XORra(r1: R[8], a: Addr)           extends Instruction
  case XORrv8(r1: R[8], v: UInt8)      extends Instruction
  case ANDrr(r1: R[8], r2: R[8])             extends Instruction
  case ANDra(r1: R[8], a: Addr)           extends Instruction
  case ANDrv8(r1: R[8], v: UInt8)      extends Instruction
  case CPrr(r1: R[8], r2: R[8])              extends Instruction
  case CPra(r1: R[8], a: Addr)            extends Instruction
  case CPrv8(r1: R[8], v: UInt8)       extends Instruction
  case JRv8(v: Byte)                   extends Instruction
  case JRrv8(r1: R[8], v: Byte)        extends Instruction
  case JRfv8(f: F, v: Byte)            extends Instruction
  case RSTl(v: UInt8)                  extends Instruction
  case RETf(f: F)                      extends Instruction
  case INCa(a: Addr)                   extends Instruction
  case DECa(a: Addr)                   extends Instruction

val OpcodeLookup: Map[Int, Op] = Map(
  0x00 -> Op.U8("NOP", Instruction.NOP),
  0x01 -> Op.U24((x: UInt16) => f"LD BC,${x}", (x: UInt16) => Instruction.LDrv16(R.BC, x : UInt16)),
  0x02 -> Op.U8("LD (BC),A", Instruction.LDar(Addr(R.BC), R.A)),
  0x03 -> Op.U8("INC BC", Instruction.INCr(R.BC)),
  0x04 -> Op.U8("INC B", Instruction.INCr(R.B)),
  0x05 -> Op.U8("DEC B", Instruction.DECr(R.B)),
  0x06 -> Op.U16((x: UInt8) => f"LD B,${x}", (x: UInt8) => Instruction.LDrv8(R.B, x : UInt8)),
  0x07 -> Op.U8("RLCA", Instruction.RLCA),
  0x08 -> Op.U24((x: UInt16) => f"LD (${x}),SP", (x: UInt16) => Instruction.LDa16r(Addr(x : UInt16), R.SP)),
  0x09 -> Op.U8("ADD HL,BC", Instruction.ADDrr(R.HL, R.BC)),
  0x0a -> Op.U8("LD A,(BC)", Instruction.LDra(R.A, Addr(R.BC))),
  0x0b -> Op.U8("DEC BC", Instruction.DECr(R.BC)),
  0x0c -> Op.U8("INC C", Instruction.INCr(R.C)),
  0x0d -> Op.U8("DEC C", Instruction.DECr(R.C)),
  0x0e -> Op.U16((x: UInt8) => f"LD C,${x}", (x: UInt8) => Instruction.LDrv8(R.C, x : UInt8)),
  0x0f -> Op.U8("RRCA", Instruction.RRCA),
  0x10 -> Op.U16((x: UInt8) => "STOP", (x: UInt8) => Instruction.STOP),
  0x11 -> Op.U24((x: UInt16) => f"LD DE,${x}", (x: UInt16) => Instruction.LDrv16(R.DE, x : UInt16)),
  0x12 -> Op.U8("LD (DE),A", Instruction.LDar(Addr(R.DE), R.A)),
  0x13 -> Op.U8("INC DE", Instruction.INCr(R.DE)),
  0x14 -> Op.U8("INC D", Instruction.INCr(R.D)),
  0x15 -> Op.U8("DEC D", Instruction.DECr(R.D)),
  0x16 -> Op.U16((x: UInt8) => f"LD D,${x}", (x: UInt8) => Instruction.LDrv8(R.D, x : UInt8)),
  0x17 -> Op.U8("RLA", Instruction.RLA),
  0x18 -> Op.S16((x: Byte) => f"JR $x%2x", (x: Byte) => Instruction.JRv8(x : Byte)),
  0x19 -> Op.U8("ADD HL,DE", Instruction.ADDrr(R.HL, R.DE)),
  0x1a -> Op.U8("LD A,(DE)", Instruction.LDra(R.A, Addr(R.DE))),
  0x1b -> Op.U8("DEC DE", Instruction.DECr(R.DE)),
  0x1c -> Op.U8("INC E", Instruction.INCr(R.E)),
  0x1d -> Op.U8("DEC E", Instruction.DECr(R.E)),
  0x1e -> Op.U16((x: UInt8) => f"LD E,${x}", (x: UInt8) => Instruction.LDrv8(R.E, x : UInt8)),
  0x1f -> Op.U8("RRA", Instruction.RRA),
  0x20 -> Op.S16((x: Byte) => f"JR NZ,$x%2x", (x: Byte) => Instruction.JRfv8(F.NZ, x : Byte)),
  0x21 -> Op.U24((x: UInt16) => f"LD HL,${x}", (x: UInt16) => Instruction.LDrv16(R.HL, x : UInt16)),
  0x22 -> Op.U8("LD (HL+),A", Instruction.LDar(Addr(R.HL, increment = true), R.A)),
  0x23 -> Op.U8("INC HL", Instruction.INCr(R.HL)),
  0x24 -> Op.U8("INC H", Instruction.INCr(R.H)),
  0x25 -> Op.U8("DEC H", Instruction.DECr(R.H)),
  0x26 -> Op.U16((x: UInt8) => f"LD H,${x}", (x: UInt8) => Instruction.LDrv8(R.H, x : UInt8)),
  0x27 -> Op.U8("DAA", Instruction.DAA),
  0x28 -> Op.S16((x: Byte) => f"JR Z,$x%2x", (x: Byte) => Instruction.JRfv8(F.Z, x : Byte)),
  0x29 -> Op.U8("ADD HL,HL", Instruction.ADDrr(R.HL, R.HL)),
  0x2a -> Op.U8("LD A,(HL+)", Instruction.LDra(R.A, Addr(R.HL, increment = true))),
  0x2b -> Op.U8("DEC HL", Instruction.DECr(R.HL)),
  0x2c -> Op.U8("INC L", Instruction.INCr(R.L)),
  0x2d -> Op.U8("DEC L", Instruction.DECr(R.L)),
  0x2e -> Op.U16((x: UInt8) => f"LD L,${x}", (x: UInt8) => Instruction.LDrv8(R.L, x : UInt8)),
  0x2f -> Op.U8("CPL", Instruction.CPL),
  0x30 -> Op.S16((x: Byte) => f"JR NC,$x%2x", (x: Byte) => Instruction.JRfv8(F.NC, x : Byte)),
  0x31 -> Op.U24((x: UInt16) => f"LD SP,${x}", (x: UInt16) => Instruction.LDrv16(R.SP, x : UInt16)),
  0x32 -> Op.U8("LD (HL-),A", Instruction.LDar(Addr(R.HL, decrement = true), R.A)),
  0x33 -> Op.U8("INC SP", Instruction.INCr(R.SP)),
  0x34 -> Op.U8("INC (HL)", Instruction.INCa(Addr(R.HL))),
  0x35 -> Op.U8("DEC (HL)", Instruction.DECa(Addr(R.HL))),
  0x36 -> Op.U16((x: UInt8) => f"LD (HL),${x}", (x: UInt8) => Instruction.LDav8(Addr(R.HL), x : UInt8)),
  0x37 -> Op.U8("SCF", Instruction.SCF),
  0x38 -> Op.S16((x: Byte) => f"JR C,$x%2x", (x: Byte) => Instruction.JRrv8(R.C, x : Byte)),
  0x39 -> Op.U8("ADD HL,SP", Instruction.ADDrr(R.HL, R.SP)),
  0x3a -> Op.U8("LD A,(HL-)", Instruction.LDra(R.A, Addr(R.HL, decrement = true))),
  0x3b -> Op.U8("DEC SP", Instruction.DECr(R.SP)),
  0x3c -> Op.U8("INC A", Instruction.INCr(R.A)),
  0x3d -> Op.U8("DEC A", Instruction.DECr(R.A)),
  0x3e -> Op.U16((x: UInt8) => f"LD A,${x}", (x: UInt8) => Instruction.LDrv8(R.A, x : UInt8)),
  0x3f -> Op.U8("CCF", Instruction.CCF),
  0x40 -> Op.U8("LD B,B", Instruction.LDrr(R.B, R.B)),
  0x41 -> Op.U8("LD B,C", Instruction.LDrr(R.B, R.C)),
  0x42 -> Op.U8("LD B,D", Instruction.LDrr(R.B, R.D)),
  0x43 -> Op.U8("LD B,E", Instruction.LDrr(R.B, R.E)),
  0x44 -> Op.U8("LD B,H", Instruction.LDrr(R.B, R.H)),
  0x45 -> Op.U8("LD B,L", Instruction.LDrr(R.B, R.L)),
  0x46 -> Op.U8("LD B,(HL)", Instruction.LDra(R.B, Addr(R.HL))),
  0x47 -> Op.U8("LD B,A", Instruction.LDrr(R.B, R.A)),
  0x48 -> Op.U8("LD C,B", Instruction.LDrr(R.C, R.B)),
  0x49 -> Op.U8("LD C,C", Instruction.LDrr(R.C, R.C)),
  0x4a -> Op.U8("LD C,D", Instruction.LDrr(R.C, R.D)),
  0x4b -> Op.U8("LD C,E", Instruction.LDrr(R.C, R.E)),
  0x4c -> Op.U8("LD C,H", Instruction.LDrr(R.C, R.H)),
  0x4d -> Op.U8("LD C,L", Instruction.LDrr(R.C, R.L)),
  0x4e -> Op.U8("LD C,(HL)", Instruction.LDra(R.C, Addr(R.HL))),
  0x4f -> Op.U8("LD C,A", Instruction.LDrr(R.C, R.A)),
  0x50 -> Op.U8("LD D,B", Instruction.LDrr(R.D, R.B)),
  0x51 -> Op.U8("LD D,C", Instruction.LDrr(R.D, R.C)),
  0x52 -> Op.U8("LD D,D", Instruction.LDrr(R.D, R.D)),
  0x53 -> Op.U8("LD D,E", Instruction.LDrr(R.D, R.E)),
  0x54 -> Op.U8("LD D,H", Instruction.LDrr(R.D, R.H)),
  0x55 -> Op.U8("LD D,L", Instruction.LDrr(R.D, R.L)),
  0x56 -> Op.U8("LD D,(HL)", Instruction.LDra(R.D, Addr(R.HL))),
  0x57 -> Op.U8("LD D,A", Instruction.LDrr(R.D, R.A)),
  0x58 -> Op.U8("LD E,B", Instruction.LDrr(R.E, R.B)),
  0x59 -> Op.U8("LD E,C", Instruction.LDrr(R.E, R.C)),
  0x5a -> Op.U8("LD E,D", Instruction.LDrr(R.E, R.D)),
  0x5b -> Op.U8("LD E,E", Instruction.LDrr(R.E, R.E)),
  0x5c -> Op.U8("LD E,H", Instruction.LDrr(R.E, R.H)),
  0x5d -> Op.U8("LD E,L", Instruction.LDrr(R.E, R.L)),
  0x5e -> Op.U8("LD E,(HL)", Instruction.LDra(R.E, Addr(R.HL))),
  0x5f -> Op.U8("LD E,A", Instruction.LDrr(R.E, R.A)),
  0x60 -> Op.U8("LD H,B", Instruction.LDrr(R.H, R.B)),
  0x61 -> Op.U8("LD H,C", Instruction.LDrr(R.H, R.C)),
  0x62 -> Op.U8("LD H,D", Instruction.LDrr(R.H, R.D)),
  0x63 -> Op.U8("LD H,E", Instruction.LDrr(R.H, R.E)),
  0x64 -> Op.U8("LD H,H", Instruction.LDrr(R.H, R.H)),
  0x65 -> Op.U8("LD H,L", Instruction.LDrr(R.H, R.L)),
  0x66 -> Op.U8("LD H,(HL)", Instruction.LDra(R.H, Addr(R.HL))),
  0x67 -> Op.U8("LD H,A", Instruction.LDrr(R.H, R.A)),
  0x68 -> Op.U8("LD L,B", Instruction.LDrr(R.L, R.B)),
  0x69 -> Op.U8("LD L,C", Instruction.LDrr(R.L, R.C)),
  0x6a -> Op.U8("LD L,D", Instruction.LDrr(R.L, R.D)),
  0x6b -> Op.U8("LD L,E", Instruction.LDrr(R.L, R.E)),
  0x6c -> Op.U8("LD L,H", Instruction.LDrr(R.L, R.H)),
  0x6d -> Op.U8("LD L,L", Instruction.LDrr(R.L, R.L)),
  0x6e -> Op.U8("LD L,(HL)", Instruction.LDra(R.L, Addr(R.HL))),
  0x6f -> Op.U8("LD L,A", Instruction.LDrr(R.L, R.A)),
  0x70 -> Op.U8("LD (HL),B", Instruction.LDar(Addr(R.HL), R.B)),
  0x71 -> Op.U8("LD (HL),C", Instruction.LDar(Addr(R.HL), R.C)),
  0x72 -> Op.U8("LD (HL),D", Instruction.LDar(Addr(R.HL), R.D)),
  0x73 -> Op.U8("LD (HL),E", Instruction.LDar(Addr(R.HL), R.E)),
  0x74 -> Op.U8("LD (HL),H", Instruction.LDar(Addr(R.HL), R.H)),
  0x75 -> Op.U8("LD (HL),L", Instruction.LDar(Addr(R.HL), R.L)),
  0x76 -> Op.U8("HALT", Instruction.HALT),
  0x77 -> Op.U8("LD (HL),A", Instruction.LDar(Addr(R.HL), R.A)),
  0x78 -> Op.U8("LD A,B", Instruction.LDrr(R.A, R.B)),
  0x79 -> Op.U8("LD A,C", Instruction.LDrr(R.A, R.C)),
  0x7a -> Op.U8("LD A,D", Instruction.LDrr(R.A, R.D)),
  0x7b -> Op.U8("LD A,E", Instruction.LDrr(R.A, R.E)),
  0x7c -> Op.U8("LD A,H", Instruction.LDrr(R.A, R.H)),
  0x7d -> Op.U8("LD A,L", Instruction.LDrr(R.A, R.L)),
  0x7e -> Op.U8("LD A,(HL)", Instruction.LDra(R.A, Addr(R.HL))),
  0x7f -> Op.U8("LD A,A", Instruction.LDrr(R.A, R.A)),
  0x80 -> Op.U8("ADD A,B", Instruction.ADDrr(R.A, R.B)),
  0x81 -> Op.U8("ADD A,C", Instruction.ADDrr(R.A, R.C)),
  0x82 -> Op.U8("ADD A,D", Instruction.ADDrr(R.A, R.D)),
  0x83 -> Op.U8("ADD A,E", Instruction.ADDrr(R.A, R.E)),
  0x84 -> Op.U8("ADD A,H", Instruction.ADDrr(R.A, R.H)),
  0x85 -> Op.U8("ADD A,L", Instruction.ADDrr(R.A, R.L)),
  0x86 -> Op.U8("ADD A,(HL)", Instruction.ADDra(R.A, Addr(R.HL))),
  0x87 -> Op.U8("ADD A,A", Instruction.ADDrr(R.A, R.A)),
  0x88 -> Op.U8("ADC A,B", Instruction.ADCrr(R.A, R.B)),
  0x89 -> Op.U8("ADC A,C", Instruction.ADCrr(R.A, R.C)),
  0x8a -> Op.U8("ADC A,D", Instruction.ADCrr(R.A, R.D)),
  0x8b -> Op.U8("ADC A,E", Instruction.ADCrr(R.A, R.E)),
  0x8c -> Op.U8("ADC A,H", Instruction.ADCrr(R.A, R.H)),
  0x8d -> Op.U8("ADC A,L", Instruction.ADCrr(R.A, R.L)),
  0x8e -> Op.U8("ADC A,(HL)", Instruction.ADCra(R.A, Addr(R.HL))),
  0x8f -> Op.U8("ADC A,A", Instruction.ADCrr(R.A, R.A)),
  0x90 -> Op.U8("SUB A,B", Instruction.SUBrr(R.A, R.B)),
  0x91 -> Op.U8("SUB A,C", Instruction.SUBrr(R.A, R.C)),
  0x92 -> Op.U8("SUB A,D", Instruction.SUBrr(R.A, R.D)),
  0x93 -> Op.U8("SUB A,E", Instruction.SUBrr(R.A, R.E)),
  0x94 -> Op.U8("SUB A,H", Instruction.SUBrr(R.A, R.H)),
  0x95 -> Op.U8("SUB A,L", Instruction.SUBrr(R.A, R.L)),
  0x96 -> Op.U8("SUB A,(HL)", Instruction.SUBra(R.A, Addr(R.HL))),
  0x97 -> Op.U8("SUB A,A", Instruction.SUBrr(R.A, R.A)),
  0x98 -> Op.U8("SBC A,B", Instruction.SBCrr(R.A, R.B)),
  0x99 -> Op.U8("SBC A,C", Instruction.SBCrr(R.A, R.C)),
  0x9a -> Op.U8("SBC A,D", Instruction.SBCrr(R.A, R.D)),
  0x9b -> Op.U8("SBC A,E", Instruction.SBCrr(R.A, R.E)),
  0x9c -> Op.U8("SBC A,H", Instruction.SBCrr(R.A, R.H)),
  0x9d -> Op.U8("SBC A,L", Instruction.SBCrr(R.A, R.L)),
  0x9e -> Op.U8("SBC A,(HL)", Instruction.SBCra(R.A, Addr(R.HL))),
  0x9f -> Op.U8("SBC A,A", Instruction.SBCrr(R.A, R.A)),
  0xa0 -> Op.U8("AND A,B", Instruction.ANDrr(R.A, R.B)),
  0xa1 -> Op.U8("AND A,C", Instruction.ANDrr(R.A, R.C)),
  0xa2 -> Op.U8("AND A,D", Instruction.ANDrr(R.A, R.D)),
  0xa3 -> Op.U8("AND A,E", Instruction.ANDrr(R.A, R.E)),
  0xa4 -> Op.U8("AND A,H", Instruction.ANDrr(R.A, R.H)),
  0xa5 -> Op.U8("AND A,L", Instruction.ANDrr(R.A, R.L)),
  0xa6 -> Op.U8("AND A,(HL)", Instruction.ANDra(R.A, Addr(R.HL))),
  0xa7 -> Op.U8("AND A,A", Instruction.ANDrr(R.A, R.A)),
  0xa8 -> Op.U8("XOR A,B", Instruction.XORrr(R.A, R.B)),
  0xa9 -> Op.U8("XOR A,C", Instruction.XORrr(R.A, R.C)),
  0xaa -> Op.U8("XOR A,D", Instruction.XORrr(R.A, R.D)),
  0xab -> Op.U8("XOR A,E", Instruction.XORrr(R.A, R.E)),
  0xac -> Op.U8("XOR A,H", Instruction.XORrr(R.A, R.H)),
  0xad -> Op.U8("XOR A,L", Instruction.XORrr(R.A, R.L)),
  0xae -> Op.U8("XOR A,(HL)", Instruction.XORra(R.A, Addr(R.HL))),
  0xaf -> Op.U8("XOR A,A", Instruction.XORrr(R.A, R.A)),
  0xb0 -> Op.U8("OR A,B", Instruction.ORrr(R.A, R.B)),
  0xb1 -> Op.U8("OR A,C", Instruction.ORrr(R.A, R.C)),
  0xb2 -> Op.U8("OR A,D", Instruction.ORrr(R.A, R.D)),
  0xb3 -> Op.U8("OR A,E", Instruction.ORrr(R.A, R.E)),
  0xb4 -> Op.U8("OR A,H", Instruction.ORrr(R.A, R.H)),
  0xb5 -> Op.U8("OR A,L", Instruction.ORrr(R.A, R.L)),
  0xb6 -> Op.U8("OR A,(HL)", Instruction.ORra(R.A, Addr(R.HL))),
  0xb7 -> Op.U8("OR A,A", Instruction.ORrr(R.A, R.A)),
  0xb8 -> Op.U8("CP A,B", Instruction.CPrr(R.A, R.B)),
  0xb9 -> Op.U8("CP A,C", Instruction.CPrr(R.A, R.C)),
  0xba -> Op.U8("CP A,D", Instruction.CPrr(R.A, R.D)),
  0xbb -> Op.U8("CP A,E", Instruction.CPrr(R.A, R.E)),
  0xbc -> Op.U8("CP A,H", Instruction.CPrr(R.A, R.H)),
  0xbd -> Op.U8("CP A,L", Instruction.CPrr(R.A, R.L)),
  0xbe -> Op.U8("CP A,(HL)", Instruction.CPra(R.A, Addr(R.HL))),
  0xbf -> Op.U8("CP A,A", Instruction.CPrr(R.A, R.A)),
  0xc0 -> Op.U8("RET NZ", Instruction.RETf(F.NZ)),
  0xc1 -> Op.U8("POP BC", Instruction.POPr(R.BC)),
  0xc2 -> Op.U24((x: UInt16) => f"JP NZ,${x}", (x: UInt16) => Instruction.JPfv16(F.NZ, x : UInt16)),
  0xc3 -> Op.U24((x: UInt16) => f"JP ${x}", (x: UInt16) => Instruction.JPv16(x : UInt16)),
  0xc4 -> Op.U24((x: UInt16) => f"CALL NZ,${x}", (x: UInt16) => Instruction.CALLfv16(F.NZ, x : UInt16)),
  0xc5 -> Op.U8("PUSH BC", Instruction.PUSHr(R.BC)),
  0xc6 -> Op.U16((x: UInt8) => f"ADD A,${x}", (x: UInt8) => Instruction.ADDrv8(R.A, x : UInt8)),
  0xc7 -> Op.U8("RST 00h", Instruction.RSTl(UInt8(0x00))),
  0xc8 -> Op.U8("RET Z", Instruction.RETf(F.Z)),
  0xc9 -> Op.U8("RET", Instruction.RET),
  0xca -> Op.U24((x: UInt16) => f"JP Z,${x}", (x: UInt16) => Instruction.JPfv16(F.Z, x : UInt16)),
  // 0xcb PREFIX
  0xcc -> Op.U24((x: UInt16) => f"CALL Z,${x}", (x: UInt16) => Instruction.CALLfv16(F.Z, x : UInt16)),
  0xcd -> Op.U24((x: UInt16) => f"CALL ${x}", (x: UInt16) => Instruction.CALLv16(x : UInt16)),
  0xce -> Op.U16((x: UInt8) => f"ADC A,${x}", (x: UInt8) => Instruction.ADCrv8(R.A, x : UInt8)),
  0xcf -> Op.U8("RST 08h", Instruction.RSTl(UInt8(0x08))),
  0xd0 -> Op.U8("RET NC", Instruction.RETf(F.NC)),
  0xd1 -> Op.U8("POP DE", Instruction.POPr(R.DE)),
  0xd2 -> Op.U24((x: UInt16) => f"JP NC,${x}", (x: UInt16) => Instruction.JPfv16(F.NC, x : UInt16)),
  // 0xd3 is unused
  0xd4 -> Op.U24((x: UInt16) => f"CALL NC,${x}", (x: UInt16) => Instruction.CALLfv16(F.NC, x : UInt16)),
  0xd5 -> Op.U8("PUSH DE", Instruction.PUSHr(R.DE)),
  0xd6 -> Op.U16((x: UInt8) => f"SUB A,${x}", (x: UInt8) => Instruction.SUBrv8(R.A, x : UInt8)),
  0xd7 -> Op.U8("RST 10h", Instruction.RSTl(UInt8(0x10))),
  0xd8 -> Op.U8("RET C", Instruction.RETr(R.C)),
  0xd9 -> Op.U8("RETI", Instruction.RETI),
  0xda -> Op.U24((x: UInt16) => f"JP C,${x}", (x: UInt16) => Instruction.JPrv16(R.C, x : UInt16)),
  // 0xdb is unused
  0xdc -> Op.U24((x: UInt16) => f"CALL C,${x}", (x: UInt16) => Instruction.CALLrv16(R.C, x : UInt16)),
  // 0xdd is unused
  0xde -> Op.U16((x: UInt8) => f"SBC A,${x}", (x: UInt8) => Instruction.SBCrv8(R.A, x : UInt8)),
  0xdf -> Op.U8("RST 18h", Instruction.RSTl(UInt8(0x18))),
  0xe0 -> Op.U16((x: UInt8) => f"LD (FF00+${x}),A", (x: UInt8) => Instruction.LDa8r(Addr(x : UInt8, offset = 0xFF00), R.A)),
  0xe1 -> Op.U8("POP HL", Instruction.POPr(R.HL)),
  0xe2 -> Op.U8("LD (FF00+C),A", Instruction.LDar(Addr(R.C, offset = 0xFF00), R.A)),
  // 0xe3 is unused
  // 0xe4 is unused
  0xe5 -> Op.U8("PUSH HL", Instruction.PUSHr(R.HL)),
  0xe6 -> Op.U16((x: UInt8) => f"AND A,${x}", (x: UInt8) => Instruction.ANDrv8(R.A, x : UInt8)),
  0xe7 -> Op.U8("RST 20h", Instruction.RSTl(UInt8(0x20))),
  0xe8 -> Op.S16((x: Byte) => f"ADD SP,$x%2x", (x: Byte) => Instruction.ADDrv8(R.SP, x : Byte)),
  0xe9 -> Op.U8("JP HL", Instruction.JPr(R.HL)),
  0xea -> Op.U24((x: UInt16) => f"LD (${x}),A", (x: UInt16) => Instruction.LDa16r(Addr(x : UInt16), R.A)),
  // 0xeb is unused
  // 0xec is unused
  // 0xed is unused
  0xee -> Op.U16((x: UInt8) => f"XOR A,${x}", (x: UInt8) => Instruction.XORrv8(R.A, x : UInt8)),
  0xef -> Op.U8("RST 28h", Instruction.RSTl(UInt8(0x28))),
  0xf0 -> Op.U16((x: UInt8) => f"LD A,(FF00+${x})", (x: UInt8) => Instruction.LDra8(R.A, Addr(x : UInt8, offset = 0xFF00))),
  0xf1 -> Op.U8("POP AF", Instruction.POPr(R.AF)),
  0xf2 -> Op.U8("LD A,(FF00+C)", Instruction.LDra(R.A, Addr(R.C, offset = 0xFF00))),
  0xf3 -> Op.U8("DI", Instruction.DI),
  // 0xf4 is unused
  0xf5 -> Op.U8("PUSH AF", Instruction.PUSHr(R.AF)),
  0xf6 -> Op.U16((x: UInt8) => f"OR A,${x}", (x: UInt8) => Instruction.ORrv8(R.A, x : UInt8)),
  0xf7 -> Op.U8("RST 30h", Instruction.RSTl(UInt8(0x30))),
  0xf8 -> Op.S16((x: Byte) => f"LD HL,SP+$x%2x", (x: Byte) => Instruction.LDrv8(R.HL, Add(x : Byte, R.SP))),
  0xf9 -> Op.U8("LD SP,HL", Instruction.LDrr(R.SP, R.HL)),
  0xfa -> Op.U24((x: UInt16) => f"LD A,(${x})", (x: UInt16) => Instruction.LDra16(R.A, Addr(x : UInt16))),
  0xfb -> Op.U8("EI", Instruction.EI),
  // 0xfc is unused
  // 0xfd is unused
  0xfe -> Op.U16((x: UInt8) => f"CP A,${x}", (x: UInt8) => Instruction.CPrv8(R.A, x : UInt8)),
  0xff -> Op.U8("RST 38h", Instruction.RSTl(UInt8(0x38))),
)

val OpcodePrefixedLookup: Map[Int, Op] = Map(
  0x00 -> Op.U8("RLC B", Instruction.RLCr(R.B)),
  0x01 -> Op.U8("RLC C", Instruction.RLCr(R.C)),
  0x02 -> Op.U8("RLC D", Instruction.RLCr(R.D)),
  0x03 -> Op.U8("RLC E", Instruction.RLCr(R.E)),
  0x04 -> Op.U8("RLC H", Instruction.RLCr(R.H)),
  0x05 -> Op.U8("RLC L", Instruction.RLCr(R.L)),
  0x06 -> Op.U8("RLC (HL)", Instruction.RLCa(Addr(R.HL))),
  0x07 -> Op.U8("RLC A", Instruction.RLCr(R.A)),
  0x08 -> Op.U8("RRC B", Instruction.RRCr(R.B)),
  0x09 -> Op.U8("RRC C", Instruction.RRCr(R.C)),
  0x0a -> Op.U8("RRC D", Instruction.RRCr(R.D)),
  0x0b -> Op.U8("RRC E", Instruction.RRCr(R.E)),
  0x0c -> Op.U8("RRC H", Instruction.RRCr(R.H)),
  0x0d -> Op.U8("RRC L", Instruction.RRCr(R.L)),
  0x0e -> Op.U8("RRC (HL)", Instruction.RRCa(Addr(R.HL))),
  0x0f -> Op.U8("RRC A", Instruction.RRCr(R.A)),
  0x10 -> Op.U8("RL B", Instruction.RLr(R.B)),
  0x11 -> Op.U8("RL C", Instruction.RLr(R.C)),
  0x12 -> Op.U8("RL D", Instruction.RLr(R.D)),
  0x13 -> Op.U8("RL E", Instruction.RLr(R.E)),
  0x14 -> Op.U8("RL H", Instruction.RLr(R.H)),
  0x15 -> Op.U8("RL L", Instruction.RLr(R.L)),
  0x16 -> Op.U8("RL (HL)", Instruction.RLa(Addr(R.HL))),
  0x17 -> Op.U8("RL A", Instruction.RLr(R.A)),
  0x18 -> Op.U8("RR B", Instruction.RRr(R.B)),
  0x19 -> Op.U8("RR C", Instruction.RRr(R.C)),
  0x1a -> Op.U8("RR D", Instruction.RRr(R.D)),
  0x1b -> Op.U8("RR E", Instruction.RRr(R.E)),
  0x1c -> Op.U8("RR H", Instruction.RRr(R.H)),
  0x1d -> Op.U8("RR L", Instruction.RRr(R.L)),
  0x1e -> Op.U8("RR (HL)", Instruction.RRa(Addr(R.HL))),
  0x1f -> Op.U8("RR A", Instruction.RRr(R.A)),
  0x20 -> Op.U8("SLA B", Instruction.SLAr(R.B)),
  0x21 -> Op.U8("SLA C", Instruction.SLAr(R.C)),
  0x22 -> Op.U8("SLA D", Instruction.SLAr(R.D)),
  0x23 -> Op.U8("SLA E", Instruction.SLAr(R.E)),
  0x24 -> Op.U8("SLA H", Instruction.SLAr(R.H)),
  0x25 -> Op.U8("SLA L", Instruction.SLAr(R.L)),
  0x26 -> Op.U8("SLA (HL)", Instruction.SLAa(Addr(R.HL))),
  0x27 -> Op.U8("SLA A", Instruction.SLAr(R.A)),
  0x28 -> Op.U8("SRA B", Instruction.SRAr(R.B)),
  0x29 -> Op.U8("SRA C", Instruction.SRAr(R.C)),
  0x2a -> Op.U8("SRA D", Instruction.SRAr(R.D)),
  0x2b -> Op.U8("SRA E", Instruction.SRAr(R.E)),
  0x2c -> Op.U8("SRA H", Instruction.SRAr(R.H)),
  0x2d -> Op.U8("SRA L", Instruction.SRAr(R.L)),
  0x2e -> Op.U8("SRA (HL)", Instruction.SRAa(Addr(R.HL))),
  0x2f -> Op.U8("SRA A", Instruction.SRAr(R.A)),
  0x30 -> Op.U8("SWAP B", Instruction.SWAPr(R.B)),
  0x31 -> Op.U8("SWAP C", Instruction.SWAPr(R.C)),
  0x32 -> Op.U8("SWAP D", Instruction.SWAPr(R.D)),
  0x33 -> Op.U8("SWAP E", Instruction.SWAPr(R.E)),
  0x34 -> Op.U8("SWAP H", Instruction.SWAPr(R.H)),
  0x35 -> Op.U8("SWAP L", Instruction.SWAPr(R.L)),
  0x36 -> Op.U8("SWAP (HL)", Instruction.SWAPa(Addr(R.HL))),
  0x37 -> Op.U8("SWAP A", Instruction.SWAPr(R.A)),
  0x38 -> Op.U8("SRL B", Instruction.SRLr(R.B)),
  0x39 -> Op.U8("SRL C", Instruction.SRLr(R.C)),
  0x3a -> Op.U8("SRL D", Instruction.SRLr(R.D)),
  0x3b -> Op.U8("SRL E", Instruction.SRLr(R.E)),
  0x3c -> Op.U8("SRL H", Instruction.SRLr(R.H)),
  0x3d -> Op.U8("SRL L", Instruction.SRLr(R.L)),
  0x3e -> Op.U8("SRL (HL)", Instruction.SRLa(Addr(R.HL))),
  0x3f -> Op.U8("SRL A", Instruction.SRLr(R.A)),
  0x40 -> Op.U8("BIT 0,B", Instruction.BITnr(0, R.B)),
  0x41 -> Op.U8("BIT 0,C", Instruction.BITnr(0, R.C)),
  0x42 -> Op.U8("BIT 0,D", Instruction.BITnr(0, R.D)),
  0x43 -> Op.U8("BIT 0,E", Instruction.BITnr(0, R.E)),
  0x44 -> Op.U8("BIT 0,H", Instruction.BITnr(0, R.H)),
  0x45 -> Op.U8("BIT 0,L", Instruction.BITnr(0, R.L)),
  0x46 -> Op.U8("BIT 0,(HL)", Instruction.BITna(0, Addr(R.HL))),
  0x47 -> Op.U8("BIT 0,A", Instruction.BITnr(0, R.A)),
  0x48 -> Op.U8("BIT 1,B", Instruction.BITnr(1, R.B)),
  0x49 -> Op.U8("BIT 1,C", Instruction.BITnr(1, R.C)),
  0x4a -> Op.U8("BIT 1,D", Instruction.BITnr(1, R.D)),
  0x4b -> Op.U8("BIT 1,E", Instruction.BITnr(1, R.E)),
  0x4c -> Op.U8("BIT 1,H", Instruction.BITnr(1, R.H)),
  0x4d -> Op.U8("BIT 1,L", Instruction.BITnr(1, R.L)),
  0x4e -> Op.U8("BIT 1,(HL)", Instruction.BITna(1, Addr(R.HL))),
  0x4f -> Op.U8("BIT 1,A", Instruction.BITnr(1, R.A)),
  0x50 -> Op.U8("BIT 2,B", Instruction.BITnr(2, R.B)),
  0x51 -> Op.U8("BIT 2,C", Instruction.BITnr(2, R.C)),
  0x52 -> Op.U8("BIT 2,D", Instruction.BITnr(2, R.D)),
  0x53 -> Op.U8("BIT 2,E", Instruction.BITnr(2, R.E)),
  0x54 -> Op.U8("BIT 2,H", Instruction.BITnr(2, R.H)),
  0x55 -> Op.U8("BIT 2,L", Instruction.BITnr(2, R.L)),
  0x56 -> Op.U8("BIT 2,(HL)", Instruction.BITna(2, Addr(R.HL))),
  0x57 -> Op.U8("BIT 2,A", Instruction.BITnr(2, R.A)),
  0x58 -> Op.U8("BIT 3,B", Instruction.BITnr(3, R.B)),
  0x59 -> Op.U8("BIT 3,C", Instruction.BITnr(3, R.C)),
  0x5a -> Op.U8("BIT 3,D", Instruction.BITnr(3, R.D)),
  0x5b -> Op.U8("BIT 3,E", Instruction.BITnr(3, R.E)),
  0x5c -> Op.U8("BIT 3,H", Instruction.BITnr(3, R.H)),
  0x5d -> Op.U8("BIT 3,L", Instruction.BITnr(3, R.L)),
  0x5e -> Op.U8("BIT 3,(HL)", Instruction.BITna(3, Addr(R.HL))),
  0x5f -> Op.U8("BIT 3,A", Instruction.BITnr(3, R.A)),
  0x60 -> Op.U8("BIT 4,B", Instruction.BITnr(4, R.B)),
  0x61 -> Op.U8("BIT 4,C", Instruction.BITnr(4, R.C)),
  0x62 -> Op.U8("BIT 4,D", Instruction.BITnr(4, R.D)),
  0x63 -> Op.U8("BIT 4,E", Instruction.BITnr(4, R.E)),
  0x64 -> Op.U8("BIT 4,H", Instruction.BITnr(4, R.H)),
  0x65 -> Op.U8("BIT 4,L", Instruction.BITnr(4, R.L)),
  0x66 -> Op.U8("BIT 4,(HL)", Instruction.BITna(4, Addr(R.HL))),
  0x67 -> Op.U8("BIT 4,A", Instruction.BITnr(4, R.A)),
  0x68 -> Op.U8("BIT 5,B", Instruction.BITnr(5, R.B)),
  0x69 -> Op.U8("BIT 5,C", Instruction.BITnr(5, R.C)),
  0x6a -> Op.U8("BIT 5,D", Instruction.BITnr(5, R.D)),
  0x6b -> Op.U8("BIT 5,E", Instruction.BITnr(5, R.E)),
  0x6c -> Op.U8("BIT 5,H", Instruction.BITnr(5, R.H)),
  0x6d -> Op.U8("BIT 5,L", Instruction.BITnr(5, R.L)),
  0x6e -> Op.U8("BIT 5,(HL)", Instruction.BITna(5, Addr(R.HL))),
  0x6f -> Op.U8("BIT 5,A", Instruction.BITnr(5, R.A)),
  0x70 -> Op.U8("BIT 6,B", Instruction.BITnr(6, R.B)),
  0x71 -> Op.U8("BIT 6,C", Instruction.BITnr(6, R.C)),
  0x72 -> Op.U8("BIT 6,D", Instruction.BITnr(6, R.D)),
  0x73 -> Op.U8("BIT 6,E", Instruction.BITnr(6, R.E)),
  0x74 -> Op.U8("BIT 6,H", Instruction.BITnr(6, R.H)),
  0x75 -> Op.U8("BIT 6,L", Instruction.BITnr(6, R.L)),
  0x76 -> Op.U8("BIT 6,(HL)", Instruction.BITna(6, Addr(R.HL))),
  0x77 -> Op.U8("BIT 6,A", Instruction.BITnr(6, R.A)),
  0x78 -> Op.U8("BIT 7,B", Instruction.BITnr(7, R.B)),
  0x79 -> Op.U8("BIT 7,C", Instruction.BITnr(7, R.C)),
  0x7a -> Op.U8("BIT 7,D", Instruction.BITnr(7, R.D)),
  0x7b -> Op.U8("BIT 7,E", Instruction.BITnr(7, R.E)),
  0x7c -> Op.U8("BIT 7,H", Instruction.BITnr(7, R.H)),
  0x7d -> Op.U8("BIT 7,L", Instruction.BITnr(7, R.L)),
  0x7e -> Op.U8("BIT 7,(HL)", Instruction.BITna(7, Addr(R.HL))),
  0x7f -> Op.U8("BIT 7,A", Instruction.BITnr(7, R.A)),
  0x80 -> Op.U8("RES 0,B", Instruction.RESnr(0, R.B)),
  0x81 -> Op.U8("RES 0,C", Instruction.RESnr(0, R.C)),
  0x82 -> Op.U8("RES 0,D", Instruction.RESnr(0, R.D)),
  0x83 -> Op.U8("RES 0,E", Instruction.RESnr(0, R.E)),
  0x84 -> Op.U8("RES 0,H", Instruction.RESnr(0, R.H)),
  0x85 -> Op.U8("RES 0,L", Instruction.RESnr(0, R.L)),
  0x86 -> Op.U8("RES 0,(HL)", Instruction.RESna(0, Addr(R.HL))),
  0x87 -> Op.U8("RES 0,A", Instruction.RESnr(0, R.A)),
  0x88 -> Op.U8("RES 1,B", Instruction.RESnr(1, R.B)),
  0x89 -> Op.U8("RES 1,C", Instruction.RESnr(1, R.C)),
  0x8a -> Op.U8("RES 1,D", Instruction.RESnr(1, R.D)),
  0x8b -> Op.U8("RES 1,E", Instruction.RESnr(1, R.E)),
  0x8c -> Op.U8("RES 1,H", Instruction.RESnr(1, R.H)),
  0x8d -> Op.U8("RES 1,L", Instruction.RESnr(1, R.L)),
  0x8e -> Op.U8("RES 1,(HL)", Instruction.RESna(1, Addr(R.HL))),
  0x8f -> Op.U8("RES 1,A", Instruction.RESnr(1, R.A)),
  0x90 -> Op.U8("RES 2,B", Instruction.RESnr(2, R.B)),
  0x91 -> Op.U8("RES 2,C", Instruction.RESnr(2, R.C)),
  0x92 -> Op.U8("RES 2,D", Instruction.RESnr(2, R.D)),
  0x93 -> Op.U8("RES 2,E", Instruction.RESnr(2, R.E)),
  0x94 -> Op.U8("RES 2,H", Instruction.RESnr(2, R.H)),
  0x95 -> Op.U8("RES 2,L", Instruction.RESnr(2, R.L)),
  0x96 -> Op.U8("RES 2,(HL)", Instruction.RESna(2, Addr(R.HL))),
  0x97 -> Op.U8("RES 2,A", Instruction.RESnr(2, R.A)),
  0x98 -> Op.U8("RES 3,B", Instruction.RESnr(3, R.B)),
  0x99 -> Op.U8("RES 3,C", Instruction.RESnr(3, R.C)),
  0x9a -> Op.U8("RES 3,D", Instruction.RESnr(3, R.D)),
  0x9b -> Op.U8("RES 3,E", Instruction.RESnr(3, R.E)),
  0x9c -> Op.U8("RES 3,H", Instruction.RESnr(3, R.H)),
  0x9d -> Op.U8("RES 3,L", Instruction.RESnr(3, R.L)),
  0x9e -> Op.U8("RES 3,(HL)", Instruction.RESna(3, Addr(R.HL))),
  0x9f -> Op.U8("RES 3,A", Instruction.RESnr(3, R.A)),
  0xa0 -> Op.U8("RES 4,B", Instruction.RESnr(4, R.B)),
  0xa1 -> Op.U8("RES 4,C", Instruction.RESnr(4, R.C)),
  0xa2 -> Op.U8("RES 4,D", Instruction.RESnr(4, R.D)),
  0xa3 -> Op.U8("RES 4,E", Instruction.RESnr(4, R.E)),
  0xa4 -> Op.U8("RES 4,H", Instruction.RESnr(4, R.H)),
  0xa5 -> Op.U8("RES 4,L", Instruction.RESnr(4, R.L)),
  0xa6 -> Op.U8("RES 4,(HL)", Instruction.RESna(4, Addr(R.HL))),
  0xa7 -> Op.U8("RES 4,A", Instruction.RESnr(4, R.A)),
  0xa8 -> Op.U8("RES 5,B", Instruction.RESnr(5, R.B)),
  0xa9 -> Op.U8("RES 5,C", Instruction.RESnr(5, R.C)),
  0xaa -> Op.U8("RES 5,D", Instruction.RESnr(5, R.D)),
  0xab -> Op.U8("RES 5,E", Instruction.RESnr(5, R.E)),
  0xac -> Op.U8("RES 5,H", Instruction.RESnr(5, R.H)),
  0xad -> Op.U8("RES 5,L", Instruction.RESnr(5, R.L)),
  0xae -> Op.U8("RES 5,(HL)", Instruction.RESna(5, Addr(R.HL))),
  0xaf -> Op.U8("RES 5,A", Instruction.RESnr(5, R.A)),
  0xb0 -> Op.U8("RES 6,B", Instruction.RESnr(6, R.B)),
  0xb1 -> Op.U8("RES 6,C", Instruction.RESnr(6, R.C)),
  0xb2 -> Op.U8("RES 6,D", Instruction.RESnr(6, R.D)),
  0xb3 -> Op.U8("RES 6,E", Instruction.RESnr(6, R.E)),
  0xb4 -> Op.U8("RES 6,H", Instruction.RESnr(6, R.H)),
  0xb5 -> Op.U8("RES 6,L", Instruction.RESnr(6, R.L)),
  0xb6 -> Op.U8("RES 6,(HL)", Instruction.RESna(6, Addr(R.HL))),
  0xb7 -> Op.U8("RES 6,A", Instruction.RESnr(6, R.A)),
  0xb8 -> Op.U8("RES 7,B", Instruction.RESnr(7, R.B)),
  0xb9 -> Op.U8("RES 7,C", Instruction.RESnr(7, R.C)),
  0xba -> Op.U8("RES 7,D", Instruction.RESnr(7, R.D)),
  0xbb -> Op.U8("RES 7,E", Instruction.RESnr(7, R.E)),
  0xbc -> Op.U8("RES 7,H", Instruction.RESnr(7, R.H)),
  0xbd -> Op.U8("RES 7,L", Instruction.RESnr(7, R.L)),
  0xbe -> Op.U8("RES 7,(HL)", Instruction.RESna(7, Addr(R.HL))),
  0xbf -> Op.U8("RES 7,A", Instruction.RESnr(7, R.A)),
  0xc0 -> Op.U8("SET 0,B", Instruction.SETnr(0, R.B)),
  0xc1 -> Op.U8("SET 0,C", Instruction.SETnr(0, R.C)),
  0xc2 -> Op.U8("SET 0,D", Instruction.SETnr(0, R.D)),
  0xc3 -> Op.U8("SET 0,E", Instruction.SETnr(0, R.E)),
  0xc4 -> Op.U8("SET 0,H", Instruction.SETnr(0, R.H)),
  0xc5 -> Op.U8("SET 0,L", Instruction.SETnr(0, R.L)),
  0xc6 -> Op.U8("SET 0,(HL)", Instruction.SETna(0, Addr(R.HL))),
  0xc7 -> Op.U8("SET 0,A", Instruction.SETnr(0, R.A)),
  0xc8 -> Op.U8("SET 1,B", Instruction.SETnr(1, R.B)),
  0xc9 -> Op.U8("SET 1,C", Instruction.SETnr(1, R.C)),
  0xca -> Op.U8("SET 1,D", Instruction.SETnr(1, R.D)),
  0xcb -> Op.U8("SET 1,E", Instruction.SETnr(1, R.E)),
  0xcc -> Op.U8("SET 1,H", Instruction.SETnr(1, R.H)),
  0xcd -> Op.U8("SET 1,L", Instruction.SETnr(1, R.L)),
  0xce -> Op.U8("SET 1,(HL)", Instruction.SETna(1, Addr(R.HL))),
  0xcf -> Op.U8("SET 1,A", Instruction.SETnr(1, R.A)),
  0xd0 -> Op.U8("SET 2,B", Instruction.SETnr(2, R.B)),
  0xd1 -> Op.U8("SET 2,C", Instruction.SETnr(2, R.C)),
  0xd2 -> Op.U8("SET 2,D", Instruction.SETnr(2, R.D)),
  0xd3 -> Op.U8("SET 2,E", Instruction.SETnr(2, R.E)),
  0xd4 -> Op.U8("SET 2,H", Instruction.SETnr(2, R.H)),
  0xd5 -> Op.U8("SET 2,L", Instruction.SETnr(2, R.L)),
  0xd6 -> Op.U8("SET 2,(HL)", Instruction.SETna(2, Addr(R.HL))),
  0xd7 -> Op.U8("SET 2,A", Instruction.SETnr(2, R.A)),
  0xd8 -> Op.U8("SET 3,B", Instruction.SETnr(3, R.B)),
  0xd9 -> Op.U8("SET 3,C", Instruction.SETnr(3, R.C)),
  0xda -> Op.U8("SET 3,D", Instruction.SETnr(3, R.D)),
  0xdb -> Op.U8("SET 3,E", Instruction.SETnr(3, R.E)),
  0xdc -> Op.U8("SET 3,H", Instruction.SETnr(3, R.H)),
  0xdd -> Op.U8("SET 3,L", Instruction.SETnr(3, R.L)),
  0xde -> Op.U8("SET 3,(HL)", Instruction.SETna(3, Addr(R.HL))),
  0xdf -> Op.U8("SET 3,A", Instruction.SETnr(3, R.A)),
  0xe0 -> Op.U8("SET 4,B", Instruction.SETnr(4, R.B)),
  0xe1 -> Op.U8("SET 4,C", Instruction.SETnr(4, R.C)),
  0xe2 -> Op.U8("SET 4,D", Instruction.SETnr(4, R.D)),
  0xe3 -> Op.U8("SET 4,E", Instruction.SETnr(4, R.E)),
  0xe4 -> Op.U8("SET 4,H", Instruction.SETnr(4, R.H)),
  0xe5 -> Op.U8("SET 4,L", Instruction.SETnr(4, R.L)),
  0xe6 -> Op.U8("SET 4,(HL)", Instruction.SETna(4, Addr(R.HL))),
  0xe7 -> Op.U8("SET 4,A", Instruction.SETnr(4, R.A)),
  0xe8 -> Op.U8("SET 5,B", Instruction.SETnr(5, R.B)),
  0xe9 -> Op.U8("SET 5,C", Instruction.SETnr(5, R.C)),
  0xea -> Op.U8("SET 5,D", Instruction.SETnr(5, R.D)),
  0xeb -> Op.U8("SET 5,E", Instruction.SETnr(5, R.E)),
  0xec -> Op.U8("SET 5,H", Instruction.SETnr(5, R.H)),
  0xed -> Op.U8("SET 5,L", Instruction.SETnr(5, R.L)),
  0xee -> Op.U8("SET 5,(HL)", Instruction.SETna(5, Addr(R.HL))),
  0xef -> Op.U8("SET 5,A", Instruction.SETnr(5, R.A)),
  0xf0 -> Op.U8("SET 6,B", Instruction.SETnr(6, R.B)),
  0xf1 -> Op.U8("SET 6,C", Instruction.SETnr(6, R.C)),
  0xf2 -> Op.U8("SET 6,D", Instruction.SETnr(6, R.D)),
  0xf3 -> Op.U8("SET 6,E", Instruction.SETnr(6, R.E)),
  0xf4 -> Op.U8("SET 6,H", Instruction.SETnr(6, R.H)),
  0xf5 -> Op.U8("SET 6,L", Instruction.SETnr(6, R.L)),
  0xf6 -> Op.U8("SET 6,(HL)", Instruction.SETna(6, Addr(R.HL))),
  0xf7 -> Op.U8("SET 6,A", Instruction.SETnr(6, R.A)),
  0xf8 -> Op.U8("SET 7,B", Instruction.SETnr(7, R.B)),
  0xf9 -> Op.U8("SET 7,C", Instruction.SETnr(7, R.C)),
  0xfa -> Op.U8("SET 7,D", Instruction.SETnr(7, R.D)),
  0xfb -> Op.U8("SET 7,E", Instruction.SETnr(7, R.E)),
  0xfc -> Op.U8("SET 7,H", Instruction.SETnr(7, R.H)),
  0xfd -> Op.U8("SET 7,L", Instruction.SETnr(7, R.L)),
  0xfe -> Op.U8("SET 7,(HL)", Instruction.SETna(7, Addr(R.HL))),
  0xff -> Op.U8("SET 7,A", Instruction.SETnr(7, R.A)),
)