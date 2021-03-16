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

enum Op[A]:
  case U8(str: () => String, op: Cpu[A])
  case S16(str: Byte => String, op: (Byte) => Cpu[A])
  case U16(str: UInt8 => String, op: (UInt8) => Cpu[A])
  case U24(str: UInt16 => String, op: (UInt16) => Cpu[A])

def decompile(data: Vector[UInt8]): Vector[String] =
  val op = data.headOption.map(_.toInt).map(OpcodeLookup.apply)
  op match
    case None => Vector()
    case Some(Op.U8(f, _))  => f() +: decompile(data.tail)
    case Some(Op.U16(f, _)) => f(data(1)) +: decompile(data.drop(2))
    case Some(Op.S16(f, _)) => f(data(1).toByte) +: decompile(data.drop(2))
    case Some(Op.U24(f, _)) => f(UInt16(data(1), data(2))) +: decompile(data.drop(3))


val OpcodeLookup: Map[Int, Op[Unit]] = Map(
  0x00 -> Op.U8(() => f"NOP", opNOP),
  0x01 -> Op.U24((x: UInt16) => f"LD BC,${x.toInt}%4x", (x: UInt16) => opLDrv16(R.BC, x : UInt16)),
  0x02 -> Op.U8(() => f"LD (BC),A", opLDar(Addr(R.BC), R.A)),
  0x03 -> Op.U8(() => f"INC BC", opINCr(R.BC)),
  0x04 -> Op.U8(() => f"INC B", opINCr(R.B)),
  0x05 -> Op.U8(() => f"DEC B", opDECr(R.B)),
  0x06 -> Op.U16((x: UInt8) => f"LD B,${x.toInt}%2x", (x: UInt8) => opLDrv8(R.B, x : UInt8)),
  0x07 -> Op.U8(() => f"RLCA", opRLCA),
  0x08 -> Op.U24((x: UInt16) => f"LD (${x.toInt}%4x),SP", (x: UInt16) => opLDa16r(Addr(x : UInt16), R.SP)),
  0x09 -> Op.U8(() => f"ADD HL,BC", opADDrr(R.HL, R.BC)),
  0x0a -> Op.U8(() => f"LD A,(BC)", opLDra(R.A, Addr(R.BC))),
  0x0b -> Op.U8(() => f"DEC BC", opDECr(R.BC)),
  0x0c -> Op.U8(() => f"INC C", opINCr(R.C)),
  0x0d -> Op.U8(() => f"DEC C", opDECr(R.C)),
  0x0e -> Op.U16((x: UInt8) => f"LD C,${x.toInt}%2x", (x: UInt8) => opLDrv8(R.C, x : UInt8)),
  0x0f -> Op.U8(() => f"RRCA", opRRCA),
  0x10 -> Op.U16((x: UInt8) => f"STOP", (x: UInt8) => opSTOP),
  0x11 -> Op.U24((x: UInt16) => f"LD DE,${x.toInt}%4x", (x: UInt16) => opLDrv16(R.DE, x : UInt16)),
  0x12 -> Op.U8(() => f"LD (DE),A", opLDar(Addr(R.DE), R.A)),
  0x13 -> Op.U8(() => f"INC DE", opINCr(R.DE)),
  0x14 -> Op.U8(() => f"INC D", opINCr(R.D)),
  0x15 -> Op.U8(() => f"DEC D", opDECr(R.D)),
  0x16 -> Op.U16((x: UInt8) => f"LD D,${x.toInt}%2x", (x: UInt8) => opLDrv8(R.D, x : UInt8)),
  0x17 -> Op.U8(() => f"RLA", opRLA),
  0x18 -> Op.S16((x: Byte) => f"JR $x%2x", (x: Byte) => opJRv8(x : Byte)),
  0x19 -> Op.U8(() => f"ADD HL,DE", opADDrr(R.HL, R.DE)),
  0x1a -> Op.U8(() => f"LD A,(DE)", opLDra(R.A, Addr(R.DE))),
  0x1b -> Op.U8(() => f"DEC DE", opDECr(R.DE)),
  0x1c -> Op.U8(() => f"INC E", opINCr(R.E)),
  0x1d -> Op.U8(() => f"DEC E", opDECr(R.E)),
  0x1e -> Op.U16((x: UInt8) => f"LD E,${x.toInt}%2x", (x: UInt8) => opLDrv8(R.E, x : UInt8)),
  0x1f -> Op.U8(() => f"RRA", opRRA),
  0x20 -> Op.S16((x: Byte) => f"JR NZ,$x%2x", (x: Byte) => opJRfv8(F.NZ, x : Byte)),
  0x21 -> Op.U24((x: UInt16) => f"LD HL,${x.toInt}%4x", (x: UInt16) => opLDrv16(R.HL, x : UInt16)),
  0x22 -> Op.U8(() => f"LD (HL+),A", opLDar(Addr(R.HL, increment = true), R.A)),
  0x23 -> Op.U8(() => f"INC HL", opINCr(R.HL)),
  0x24 -> Op.U8(() => f"INC H", opINCr(R.H)),
  0x25 -> Op.U8(() => f"DEC H", opDECr(R.H)),
  0x26 -> Op.U16((x: UInt8) => f"LD H,${x.toInt}%2x", (x: UInt8) => opLDrv8(R.H, x : UInt8)),
  0x27 -> Op.U8(() => f"DAA", opDAA),
  0x28 -> Op.S16((x: Byte) => f"JR Z,$x%2x", (x: Byte) => opJRfv8(F.Z, x : Byte)),
  0x29 -> Op.U8(() => f"ADD HL,HL", opADDrr(R.HL, R.HL)),
  0x2a -> Op.U8(() => f"LD A,(HL+)", opLDra(R.A, Addr(R.HL, increment = true))),
  0x2b -> Op.U8(() => f"DEC HL", opDECr(R.HL)),
  0x2c -> Op.U8(() => f"INC L", opINCr(R.L)),
  0x2d -> Op.U8(() => f"DEC L", opDECr(R.L)),
  0x2e -> Op.U16((x: UInt8) => f"LD L,${x.toInt}%2x", (x: UInt8) => opLDrv8(R.L, x : UInt8)),
  0x2f -> Op.U8(() => f"CPL", opCPL),
  0x30 -> Op.S16((x: Byte) => f"JR NC,$x%2x", (x: Byte) => opJRfv8(F.NC, x : Byte)),
  0x31 -> Op.U24((x: UInt16) => f"LD SP,${x.toInt}%4x", (x: UInt16) => opLDrv16(R.SP, x : UInt16)),
  0x32 -> Op.U8(() => f"LD (HL-),A", opLDar(Addr(R.HL, decrement = true), R.A)),
  0x33 -> Op.U8(() => f"INC SP", opINCr(R.SP)),
  0x34 -> Op.U8(() => f"INC (HL)", opINCa(Addr(R.HL))),
  0x35 -> Op.U8(() => f"DEC (HL)", opDECa(Addr(R.HL))),
  0x36 -> Op.U16((x: UInt8) => f"LD (HL),${x.toInt}%2x", (x: UInt8) => opLDav8(Addr(R.HL), x : UInt8)),
  0x37 -> Op.U8(() => f"SCF", opSCF),
  0x38 -> Op.S16((x: Byte) => f"JR C,$x%2x", (x: Byte) => opJRrv8(R.C, x : Byte)),
  0x39 -> Op.U8(() => f"ADD HL,SP", opADDrr(R.HL, R.SP)),
  0x3a -> Op.U8(() => f"LD A,(HL-)", opLDra(R.A, Addr(R.HL, decrement = true))),
  0x3b -> Op.U8(() => f"DEC SP", opDECr(R.SP)),
  0x3c -> Op.U8(() => f"INC A", opINCr(R.A)),
  0x3d -> Op.U8(() => f"DEC A", opDECr(R.A)),
  0x3e -> Op.U16((x: UInt8) => f"LD A,${x.toInt}%2x", (x: UInt8) => opLDrv8(R.A, x : UInt8)),
  0x3f -> Op.U8(() => f"CCF", opCCF),
  0x40 -> Op.U8(() => f"LD B,B", opLDrr(R.B, R.B)),
  0x41 -> Op.U8(() => f"LD B,C", opLDrr(R.B, R.C)),
  0x42 -> Op.U8(() => f"LD B,D", opLDrr(R.B, R.D)),
  0x43 -> Op.U8(() => f"LD B,E", opLDrr(R.B, R.E)),
  0x44 -> Op.U8(() => f"LD B,H", opLDrr(R.B, R.H)),
  0x45 -> Op.U8(() => f"LD B,L", opLDrr(R.B, R.L)),
  0x46 -> Op.U8(() => f"LD B,(HL)", opLDra(R.B, Addr(R.HL))),
  0x47 -> Op.U8(() => f"LD B,A", opLDrr(R.B, R.A)),
  0x48 -> Op.U8(() => f"LD C,B", opLDrr(R.C, R.B)),
  0x49 -> Op.U8(() => f"LD C,C", opLDrr(R.C, R.C)),
  0x4a -> Op.U8(() => f"LD C,D", opLDrr(R.C, R.D)),
  0x4b -> Op.U8(() => f"LD C,E", opLDrr(R.C, R.E)),
  0x4c -> Op.U8(() => f"LD C,H", opLDrr(R.C, R.H)),
  0x4d -> Op.U8(() => f"LD C,L", opLDrr(R.C, R.L)),
  0x4e -> Op.U8(() => f"LD C,(HL)", opLDra(R.C, Addr(R.HL))),
  0x4f -> Op.U8(() => f"LD C,A", opLDrr(R.C, R.A)),
  0x50 -> Op.U8(() => f"LD D,B", opLDrr(R.D, R.B)),
  0x51 -> Op.U8(() => f"LD D,C", opLDrr(R.D, R.C)),
  0x52 -> Op.U8(() => f"LD D,D", opLDrr(R.D, R.D)),
  0x53 -> Op.U8(() => f"LD D,E", opLDrr(R.D, R.E)),
  0x54 -> Op.U8(() => f"LD D,H", opLDrr(R.D, R.H)),
  0x55 -> Op.U8(() => f"LD D,L", opLDrr(R.D, R.L)),
  0x56 -> Op.U8(() => f"LD D,(HL)", opLDra(R.D, Addr(R.HL))),
  0x57 -> Op.U8(() => f"LD D,A", opLDrr(R.D, R.A)),
  0x58 -> Op.U8(() => f"LD E,B", opLDrr(R.E, R.B)),
  0x59 -> Op.U8(() => f"LD E,C", opLDrr(R.E, R.C)),
  0x5a -> Op.U8(() => f"LD E,D", opLDrr(R.E, R.D)),
  0x5b -> Op.U8(() => f"LD E,E", opLDrr(R.E, R.E)),
  0x5c -> Op.U8(() => f"LD E,H", opLDrr(R.E, R.H)),
  0x5d -> Op.U8(() => f"LD E,L", opLDrr(R.E, R.L)),
  0x5e -> Op.U8(() => f"LD E,(HL)", opLDra(R.E, Addr(R.HL))),
  0x5f -> Op.U8(() => f"LD E,A", opLDrr(R.E, R.A)),
  0x60 -> Op.U8(() => f"LD H,B", opLDrr(R.H, R.B)),
  0x61 -> Op.U8(() => f"LD H,C", opLDrr(R.H, R.C)),
  0x62 -> Op.U8(() => f"LD H,D", opLDrr(R.H, R.D)),
  0x63 -> Op.U8(() => f"LD H,E", opLDrr(R.H, R.E)),
  0x64 -> Op.U8(() => f"LD H,H", opLDrr(R.H, R.H)),
  0x65 -> Op.U8(() => f"LD H,L", opLDrr(R.H, R.L)),
  0x66 -> Op.U8(() => f"LD H,(HL)", opLDra(R.H, Addr(R.HL))),
  0x67 -> Op.U8(() => f"LD H,A", opLDrr(R.H, R.A)),
  0x68 -> Op.U8(() => f"LD L,B", opLDrr(R.L, R.B)),
  0x69 -> Op.U8(() => f"LD L,C", opLDrr(R.L, R.C)),
  0x6a -> Op.U8(() => f"LD L,D", opLDrr(R.L, R.D)),
  0x6b -> Op.U8(() => f"LD L,E", opLDrr(R.L, R.E)),
  0x6c -> Op.U8(() => f"LD L,H", opLDrr(R.L, R.H)),
  0x6d -> Op.U8(() => f"LD L,L", opLDrr(R.L, R.L)),
  0x6e -> Op.U8(() => f"LD L,(HL)", opLDra(R.L, Addr(R.HL))),
  0x6f -> Op.U8(() => f"LD L,A", opLDrr(R.L, R.A)),
  0x70 -> Op.U8(() => f"LD (HL),B", opLDar(Addr(R.HL), R.B)),
  0x71 -> Op.U8(() => f"LD (HL),C", opLDar(Addr(R.HL), R.C)),
  0x72 -> Op.U8(() => f"LD (HL),D", opLDar(Addr(R.HL), R.D)),
  0x73 -> Op.U8(() => f"LD (HL),E", opLDar(Addr(R.HL), R.E)),
  0x74 -> Op.U8(() => f"LD (HL),H", opLDar(Addr(R.HL), R.H)),
  0x75 -> Op.U8(() => f"LD (HL),L", opLDar(Addr(R.HL), R.L)),
  0x76 -> Op.U8(() => f"HALT", opHALT),
  0x77 -> Op.U8(() => f"LD (HL),A", opLDar(Addr(R.HL), R.A)),
  0x78 -> Op.U8(() => f"LD A,B", opLDrr(R.A, R.B)),
  0x79 -> Op.U8(() => f"LD A,C", opLDrr(R.A, R.C)),
  0x7a -> Op.U8(() => f"LD A,D", opLDrr(R.A, R.D)),
  0x7b -> Op.U8(() => f"LD A,E", opLDrr(R.A, R.E)),
  0x7c -> Op.U8(() => f"LD A,H", opLDrr(R.A, R.H)),
  0x7d -> Op.U8(() => f"LD A,L", opLDrr(R.A, R.L)),
  0x7e -> Op.U8(() => f"LD A,(HL)", opLDra(R.A, Addr(R.HL))),
  0x7f -> Op.U8(() => f"LD A,A", opLDrr(R.A, R.A)),
  0x80 -> Op.U8(() => f"ADD A,B", opADDrr(R.A, R.B)),
  0x81 -> Op.U8(() => f"ADD A,C", opADDrr(R.A, R.C)),
  0x82 -> Op.U8(() => f"ADD A,D", opADDrr(R.A, R.D)),
  0x83 -> Op.U8(() => f"ADD A,E", opADDrr(R.A, R.E)),
  0x84 -> Op.U8(() => f"ADD A,H", opADDrr(R.A, R.H)),
  0x85 -> Op.U8(() => f"ADD A,L", opADDrr(R.A, R.L)),
  0x86 -> Op.U8(() => f"ADD A,(HL)", opADDra(R.A, Addr(R.HL))),
  0x87 -> Op.U8(() => f"ADD A,A", opADDrr(R.A, R.A)),
  0x88 -> Op.U8(() => f"ADC A,B", opADCrr(R.A, R.B)),
  0x89 -> Op.U8(() => f"ADC A,C", opADCrr(R.A, R.C)),
  0x8a -> Op.U8(() => f"ADC A,D", opADCrr(R.A, R.D)),
  0x8b -> Op.U8(() => f"ADC A,E", opADCrr(R.A, R.E)),
  0x8c -> Op.U8(() => f"ADC A,H", opADCrr(R.A, R.H)),
  0x8d -> Op.U8(() => f"ADC A,L", opADCrr(R.A, R.L)),
  0x8e -> Op.U8(() => f"ADC A,(HL)", opADCra(R.A, Addr(R.HL))),
  0x8f -> Op.U8(() => f"ADC A,A", opADCrr(R.A, R.A)),
  0x90 -> Op.U8(() => f"SUB A,B", opSUBrr(R.A, R.B)),
  0x91 -> Op.U8(() => f"SUB A,C", opSUBrr(R.A, R.C)),
  0x92 -> Op.U8(() => f"SUB A,D", opSUBrr(R.A, R.D)),
  0x93 -> Op.U8(() => f"SUB A,E", opSUBrr(R.A, R.E)),
  0x94 -> Op.U8(() => f"SUB A,H", opSUBrr(R.A, R.H)),
  0x95 -> Op.U8(() => f"SUB A,L", opSUBrr(R.A, R.L)),
  0x96 -> Op.U8(() => f"SUB A,(HL)", opSUBra(R.A, Addr(R.HL))),
  0x97 -> Op.U8(() => f"SUB A,A", opSUBrr(R.A, R.A)),
  0x98 -> Op.U8(() => f"SBC A,B", opSBCrr(R.A, R.B)),
  0x99 -> Op.U8(() => f"SBC A,C", opSBCrr(R.A, R.C)),
  0x9a -> Op.U8(() => f"SBC A,D", opSBCrr(R.A, R.D)),
  0x9b -> Op.U8(() => f"SBC A,E", opSBCrr(R.A, R.E)),
  0x9c -> Op.U8(() => f"SBC A,H", opSBCrr(R.A, R.H)),
  0x9d -> Op.U8(() => f"SBC A,L", opSBCrr(R.A, R.L)),
  0x9e -> Op.U8(() => f"SBC A,(HL)", opSBCra(R.A, Addr(R.HL))),
  0x9f -> Op.U8(() => f"SBC A,A", opSBCrr(R.A, R.A)),
  0xa0 -> Op.U8(() => f"AND A,B", opANDrr(R.A, R.B)),
  0xa1 -> Op.U8(() => f"AND A,C", opANDrr(R.A, R.C)),
  0xa2 -> Op.U8(() => f"AND A,D", opANDrr(R.A, R.D)),
  0xa3 -> Op.U8(() => f"AND A,E", opANDrr(R.A, R.E)),
  0xa4 -> Op.U8(() => f"AND A,H", opANDrr(R.A, R.H)),
  0xa5 -> Op.U8(() => f"AND A,L", opANDrr(R.A, R.L)),
  0xa6 -> Op.U8(() => f"AND A,(HL)", opANDra(R.A, Addr(R.HL))),
  0xa7 -> Op.U8(() => f"AND A,A", opANDrr(R.A, R.A)),
  0xa8 -> Op.U8(() => f"XOR A,B", opXORrr(R.A, R.B)),
  0xa9 -> Op.U8(() => f"XOR A,C", opXORrr(R.A, R.C)),
  0xaa -> Op.U8(() => f"XOR A,D", opXORrr(R.A, R.D)),
  0xab -> Op.U8(() => f"XOR A,E", opXORrr(R.A, R.E)),
  0xac -> Op.U8(() => f"XOR A,H", opXORrr(R.A, R.H)),
  0xad -> Op.U8(() => f"XOR A,L", opXORrr(R.A, R.L)),
  0xae -> Op.U8(() => f"XOR A,(HL)", opXORra(R.A, Addr(R.HL))),
  0xaf -> Op.U8(() => f"XOR A,A", opXORrr(R.A, R.A)),
  0xb0 -> Op.U8(() => f"OR A,B", opORrr(R.A, R.B)),
  0xb1 -> Op.U8(() => f"OR A,C", opORrr(R.A, R.C)),
  0xb2 -> Op.U8(() => f"OR A,D", opORrr(R.A, R.D)),
  0xb3 -> Op.U8(() => f"OR A,E", opORrr(R.A, R.E)),
  0xb4 -> Op.U8(() => f"OR A,H", opORrr(R.A, R.H)),
  0xb5 -> Op.U8(() => f"OR A,L", opORrr(R.A, R.L)),
  0xb6 -> Op.U8(() => f"OR A,(HL)", opORra(R.A, Addr(R.HL))),
  0xb7 -> Op.U8(() => f"OR A,A", opORrr(R.A, R.A)),
  0xb8 -> Op.U8(() => f"CP A,B", opCPrr(R.A, R.B)),
  0xb9 -> Op.U8(() => f"CP A,C", opCPrr(R.A, R.C)),
  0xba -> Op.U8(() => f"CP A,D", opCPrr(R.A, R.D)),
  0xbb -> Op.U8(() => f"CP A,E", opCPrr(R.A, R.E)),
  0xbc -> Op.U8(() => f"CP A,H", opCPrr(R.A, R.H)),
  0xbd -> Op.U8(() => f"CP A,L", opCPrr(R.A, R.L)),
  0xbe -> Op.U8(() => f"CP A,(HL)", opCPra(R.A, Addr(R.HL))),
  0xbf -> Op.U8(() => f"CP A,A", opCPrr(R.A, R.A)),
  0xc0 -> Op.U8(() => f"RET NZ", opRETf(F.NZ)),
  0xc1 -> Op.U8(() => f"POP BC", opPOPr(R.BC)),
  0xc2 -> Op.U24((x: UInt16) => f"JP NZ,${x.toInt}%4x", (x: UInt16) => opJPfv16(F.NZ, x : UInt16)),
  0xc3 -> Op.U24((x: UInt16) => s"JP ${x}", (x: UInt16) => opJPv16(x : UInt16)),
  0xc4 -> Op.U24((x: UInt16) => f"CALL NZ,${x.toInt}%4x", (x: UInt16) => opCALLfv16(F.NZ, x : UInt16)),
  0xc5 -> Op.U8(() => f"PUSH BC", opPUSHr(R.BC)),
  0xc6 -> Op.U16((x: UInt8) => f"ADD A,${x.toInt}%2x", (x: UInt8) => opADDrv8(R.A, x : UInt8)),
  0xc7 -> Op.U8(() => f"RST 00h", opRSTl(UInt8(0x00))),
  0xc8 -> Op.U8(() => f"RET Z", opRETf(F.Z)),
  0xc9 -> Op.U8(() => f"RET", opRET),
  0xca -> Op.U24((x: UInt16) => f"JP Z,${x.toInt}%4x", (x: UInt16) => opJPfv16(F.Z, x : UInt16)),
  // PREFIX CB
  0xcc -> Op.U24((x: UInt16) => f"CALL Z,${x.toInt}%4x", (x: UInt16) => opCALLfv16(F.Z, x : UInt16)),
  0xcd -> Op.U24((x: UInt16) => f"CALL ${x.toInt}%4x", (x: UInt16) => opCALLv16(x : UInt16)),
  0xce -> Op.U16((x: UInt8) => f"ADC A,${x.toInt}%2x", (x: UInt8) => opADCrv8(R.A, x : UInt8)),
  0xcf -> Op.U8(() => f"RST 08h", opRSTl(UInt8(0x08))),
  0xd0 -> Op.U8(() => f"RET NC", opRETf(F.NC)),
  0xd1 -> Op.U8(() => f"POP DE", opPOPr(R.DE)),
  0xd2 -> Op.U24((x: UInt16) => f"JP NC,${x.toInt}%4x", (x: UInt16) => opJPfv16(F.NC, x : UInt16)),
  // 0xd3 is unused
  0xd4 -> Op.U24((x: UInt16) => f"CALL NC,${x.toInt}%4x", (x: UInt16) => opCALLfv16(F.NC, x : UInt16)),
  0xd5 -> Op.U8(() => f"PUSH DE", opPUSHr(R.DE)),
  0xd6 -> Op.U16((x: UInt8) => f"SUB A,${x.toInt}%2x", (x: UInt8) => opSUBrv8(R.A, x : UInt8)),
  0xd7 -> Op.U8(() => f"RST 10h", opRSTl(UInt8(0x10))),
  0xd8 -> Op.U8(() => f"RET C", opRETr(R.C)),
  0xd9 -> Op.U8(() => f"RETI", opRETI),
  0xda -> Op.U24((x: UInt16) => f"JP C,${x.toInt}%4x", (x: UInt16) => opJPrv16(R.C, x : UInt16)),
  // 0xdb is unused
  0xdc -> Op.U24((x: UInt16) => f"CALL C,${x.toInt}%4x", (x: UInt16) => opCALLrv16(R.C, x : UInt16)),
  // 0xdd is unused
  0xde -> Op.U16((x: UInt8) => f"SBC A,${x.toInt}%2x", (x: UInt8) => opSBCrv8(R.A, x : UInt8)),
  0xdf -> Op.U8(() => f"RST 18h", opRSTl(UInt8(0x18))),
  0xe0 -> Op.U16((x: UInt8) => f"LD (FF00+${x.toInt}%2x),A", (x: UInt8) => opLDa8r(Addr(x : UInt8, offset = 0xFF00), R.A)),
  0xe1 -> Op.U8(() => f"POP HL", opPOPr(R.HL)),
  0xe2 -> Op.U8(() => f"LD (FF00+C),A", opLDar(Addr(R.C, offset = 0xFF00), R.A)),
  // 0xe3 is unused
  // 0xe4 is unused
  0xe5 -> Op.U8(() => f"PUSH HL", opPUSHr(R.HL)),
  0xe6 -> Op.U16((x: UInt8) => f"AND A,${x.toInt}%2x", (x: UInt8) => opANDrv8(R.A, x : UInt8)),
  0xe7 -> Op.U8(() => f"RST 20h", opRSTl(UInt8(0x20))),
  0xe8 -> Op.S16((x: Byte) => f"ADD SP,$x%2x", (x: Byte) => opADDrv8(R.SP, x : Byte)),
  0xe9 -> Op.U8(() => f"JP HL", opJPr(R.HL)),
  0xea -> Op.U24((x: UInt16) => f"LD (${x.toInt}%4x),A", (x: UInt16) => opLDa16r(Addr(x : UInt16), R.A)),
  // 0xeb is unused
  // 0xec is unused
  // 0xed is unused
  0xee -> Op.U16((x: UInt8) => f"XOR A,${x.toInt}%2x", (x: UInt8) => opXORrv8(R.A, x : UInt8)),
  0xef -> Op.U8(() => f"RST 28h", opRSTl(UInt8(0x28))),
  0xf0 -> Op.U16((x: UInt8) => f"LD A,(FF00+${x.toInt}%2x)", (x: UInt8) => opLDra8(R.A, Addr(x : UInt8, offset = 0xFF00))),
  0xf1 -> Op.U8(() => f"POP AF", opPOPr(R.AF)),
  0xf2 -> Op.U8(() => f"LD A,(FF00+C)", opLDra(R.A, Addr(R.C, offset = 0xFF00))),
  0xf3 -> Op.U8(() => f"DI", opDI),
  // 0xf4 is unused
  0xf5 -> Op.U8(() => f"PUSH AF", opPUSHr(R.AF)),
  0xf6 -> Op.U16((x: UInt8) => f"OR A,${x.toInt}%2x", (x: UInt8) => opORrv8(R.A, x : UInt8)),
  0xf7 -> Op.U8(() => f"RST 30h", opRSTl(UInt8(0x30))),
  0xf8 -> Op.S16((x: Byte) => f"LD HL,SP+$x%2x", (x: Byte) => opLDrv8(R.HL, Add(x : Byte, R.SP))),
  0xf9 -> Op.U8(() => f"LD SP,HL", opLDrr(R.SP, R.HL)),
  0xfa -> Op.U24((x: UInt16) => f"LD A,(${x.toInt}%4x)", (x: UInt16) => opLDra16(R.A, Addr(x : UInt16))),
  0xfb -> Op.U8(() => f"EI", opEI),
  // 0xfc is unused
  // 0xfd is unused
  0xfe -> Op.U16((x: UInt8) => f"CP A,${x.toInt}%2x", (x: UInt8) => opCPrv8(R.A, x : UInt8)),
  0xff -> Op.U8(() => f"RST 38h", opRSTl(UInt8(0x38)))
)
