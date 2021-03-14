package gameboy

import scala.util.Try

import util.{given, *}
import cats.*
import cats.implicits.given

enum CGB(val flag: UInt8):
  case CGBAndGB extends CGB(0x80.u8)
  case CGBOnly  extends CGB(0xC0.u8)

def readCGB: Binary[CGB] = 
  readUInt8.map { 
    case UInt8(0x80) => CGB.CGBAndGB
    case UInt8(0xC0) => CGB.CGBOnly
  }

enum SGB(val flag: UInt8):
  case NoSGB extends SGB(0x00.u8)
  case SGB   extends SGB(0x03.u8)

def readSGB: Binary[SGB] =
  readUInt8.map {
    case UInt8(0x00) => SGB.NoSGB
    case UInt8(0x03) => SGB.SGB
  }

enum CartridgeType(val cart: UInt8):
  case RomOnly                    extends CartridgeType(0x00.u8)
  case MBC1                       extends CartridgeType(0x01.u8)
  case MBC1Ram                    extends CartridgeType(0x02.u8)
  case MBC1RamBattery             extends CartridgeType(0x03.u8)
  case MBC2                       extends CartridgeType(0x05.u8)
  case MBC2Battery                extends CartridgeType(0x06.u8)
  case RomRam                     extends CartridgeType(0x08.u8)
  case RomRamBattery              extends CartridgeType(0x09.u8)
  case MMM01                      extends CartridgeType(0x0B.u8)
  case MMM01Ram                   extends CartridgeType(0x0C.u8)
  case MMM01RamBattery            extends CartridgeType(0x0D.u8)
  case MBC3TimerBattery           extends CartridgeType(0x0F.u8)
  case MBC3TimerRamBattery        extends CartridgeType(0x10.u8) 
  case MBC3                       extends CartridgeType(0x11.u8) 
  case MBC3Ram                    extends CartridgeType(0x12.u8) 
  case MBC3RamBattery             extends CartridgeType(0x13.u8) 
  case MBC5                       extends CartridgeType(0x19.u8) 
  case MBC5Ram                    extends CartridgeType(0x1A.u8) 
  case MBC5RamBattery             extends CartridgeType(0x1B.u8) 
  case MBC5Rumble                 extends CartridgeType(0x1C.u8) 
  case MBC5RumbleRam              extends CartridgeType(0x1D.u8) 
  case MBC5RumbleRamBattery       extends CartridgeType(0x1E.u8) 
  case MBC6                       extends CartridgeType(0x20.u8) 
  case MBC7SensorRumbleRamBattery extends CartridgeType(0x22.u8) 
  case PocketCamera               extends CartridgeType(0xFC.u8) 
  case BandaiTAMA5                extends CartridgeType(0xFD.u8) 
  case HuC3                       extends CartridgeType(0xFE.u8) 
  case HuC1RamBattery             extends CartridgeType(0xFF.u8)

def readCartridgeType: Binary[CartridgeType] =
  readUInt8.map {
    case UInt8(0x00) => CartridgeType.RomOnly
    case UInt8(0x01) => CartridgeType.MBC1
    case UInt8(0x02) => CartridgeType.MBC1Ram
    case UInt8(0x03) => CartridgeType.MBC1RamBattery
    case UInt8(0x05) => CartridgeType.MBC2
    case UInt8(0x06) => CartridgeType.MBC2Battery
    case UInt8(0x08) => CartridgeType.RomRam
    case UInt8(0x09) => CartridgeType.RomRamBattery
    case UInt8(0x0B) => CartridgeType.MMM01
    case UInt8(0x0C) => CartridgeType.MMM01Ram
    case UInt8(0x0D) => CartridgeType.MMM01RamBattery
    case UInt8(0x0F) => CartridgeType.MBC3TimerBattery
    case UInt8(0x10) => CartridgeType.MBC3TimerRamBattery
    case UInt8(0x11) => CartridgeType.MBC3
    case UInt8(0x12) => CartridgeType.MBC3Ram
    case UInt8(0x13) => CartridgeType.MBC3RamBattery
    case UInt8(0x19) => CartridgeType.MBC5
    case UInt8(0x1A) => CartridgeType.MBC5Ram
    case UInt8(0x1B) => CartridgeType.MBC5RamBattery
    case UInt8(0x1C) => CartridgeType.MBC5Rumble
    case UInt8(0x1D) => CartridgeType.MBC5RumbleRam
    case UInt8(0x1E) => CartridgeType.MBC5RumbleRamBattery
    case UInt8(0x20) => CartridgeType.MBC6
    case UInt8(0x22) => CartridgeType.MBC7SensorRumbleRamBattery
    case UInt8(0xFC) => CartridgeType.PocketCamera
    case UInt8(0xFD) => CartridgeType.BandaiTAMA5
    case UInt8(0xFE) => CartridgeType.HuC3
    case UInt8(0xFF) => CartridgeType.HuC1RamBattery
  }

enum ROMSize(val flag: UInt8): 
  case Rom32KB  extends ROMSize(0x00.u8) 
  case Rom64KB  extends ROMSize(0x01.u8) 
  case Rom128KB extends ROMSize(0x02.u8) 
  case Rom256KB extends ROMSize(0x03.u8) 
  case Rom512KB extends ROMSize(0x04.u8) 
  case Rom1MB   extends ROMSize(0x05.u8) 
  case Rom2MB   extends ROMSize(0x06.u8) 
  case Rom4MB   extends ROMSize(0x07.u8) 
  case Rom8MB   extends ROMSize(0x08.u8) 
  case Rom1_1MB extends ROMSize(0x52.u8) 
  case Rom1_2MB extends ROMSize(0x53.u8) 
  case Rom1_5MB extends ROMSize(0x54.u8)

def readROMSize: Binary[ROMSize] =
  readUInt8.map {
    case UInt8(0x00) => ROMSize.Rom32KB
    case UInt8(0x01) => ROMSize.Rom64KB
    case UInt8(0x02) => ROMSize.Rom128KB
    case UInt8(0x03) => ROMSize.Rom256KB
    case UInt8(0x04) => ROMSize.Rom512KB
    case UInt8(0x05) => ROMSize.Rom1MB
    case UInt8(0x06) => ROMSize.Rom2MB
    case UInt8(0x07) => ROMSize.Rom4MB
    case UInt8(0x08) => ROMSize.Rom8MB
    case UInt8(0x52) => ROMSize.Rom1_1MB
    case UInt8(0x53) => ROMSize.Rom1_2MB
    case UInt8(0x54) => ROMSize.Rom1_5MB
  }

enum RAMSize(val flag: UInt8): 
  case Ram0     extends RAMSize(0x00.u8) 
  case Ram8KB   extends RAMSize(0x02.u8) 
  case Ram32KB  extends RAMSize(0x03.u8) 
  case Ram128KB extends RAMSize(0x04.u8) 
  case Ram64KB  extends RAMSize(0x05.u8)

def readRAMSize: Binary[RAMSize] =
  readUInt8.map {
    case UInt8(0x00) => RAMSize.Ram0
    case UInt8(0x02) => RAMSize.Ram8KB
    case UInt8(0x03) => RAMSize.Ram32KB
    case UInt8(0x04) => RAMSize.Ram128KB
    case UInt8(0x05) => RAMSize.Ram64KB
  }

enum DestinationCode(val flag: UInt8): 
  case Japanese    extends DestinationCode(0x00.u8) 
  case NonJapanese extends DestinationCode(0x01.u8)

def readDestinationCode: Binary[DestinationCode] =
  readUInt8.map {
    case UInt8(0x00) => DestinationCode.Japanese
    case UInt8(0x01) => DestinationCode.NonJapanese
  }

val NewLicensee = Map(
  0x00 -> "None",
  0x01 -> "Nintendo R&D",
  0x08 -> "Capcom",
  0x13 -> "Electronic Arts",
  0x18 -> "Hudson Soft",
  0x19 -> "B-AI",
  0x20 -> "KSS",
  0x22 -> "POW",
  0x24 -> "PCM Complete",
  0x25 -> "San-X",
  0x28 -> "Kemco Japan",
  0x29 -> "SETA",
  0x30 -> "Viacom",
  0x31 -> "Nintendo",
  0x32 -> "Bandai",
  0x33 -> "Ocean/Acclaim",
  0x34 -> "Konami",
  0x35 -> "Hector",
  0x37 -> "Taito",
  0x38 -> "Hudson",
  0x39 -> "Banpresto",
  0x41 -> "Ubisoft",
  0x42 -> "Atlus",
  0x44 -> "Malibu",
  0x46 -> "Angel",
  0x47 -> "Bullet-Proof",
  0x49 -> "Irem.",
  0x50 -> "Absolute",
  0x51 -> "Acclaim",
  0x52 -> "Activision",
  0x53 -> "American Sammy Corporation",
  0x54 -> "Konami",
  0x55 -> "Hi Tech Entertainment",
  0x56 -> "LJN",
  0x57 -> "Matchbox",
  0x58 -> "Mattel",
  0x59 -> "Milton Bradley",
  0x60 -> "Titus",
  0x61 -> "Virgin",
  0x64 -> "LucasArts",
  0x67 -> "Ocean",
  0x69 -> "Electronic Arts",
  0x70 -> "Infogrames",
  0x71 -> "Interplay",
  0x72 -> "Broderbund",
  0x73 -> "Sculptured",
  0x75 -> "SCI",
  0x78 -> "THQ",
  0x79 -> "Accolade",
  0x80 -> "Misawa",
  0x83 -> "LOZC",
  0x86 -> "Tokuma Shoten",
  0x87 -> "Tsukuda Original",
  0x91 -> "Chunsoft",
  0x92 -> "Video System",
  0x93 -> "Ocean/Acclaim",
  0x95 -> "Varie",
  0x96 -> "Yonezawa / S\'pal",
  0x97 -> "Kaneko",
  0x99 -> "Pack-In-Video",
  0xa4 -> "Konami (Yu-Gi-Oh!)"
)

case class CartridgeHeader(logo: Vector[UInt8],              // Vec[UInt8, 30],
                           title: String,             // Vec[UInt8, 11],
                           manufacturingCode: String, // Vector[UInt8], // Vec[UInt8, 4],
                           cgb: CGB,
                           licensee: String,          // Vec[UInt8, 2],
                           sgb: SGB,
                           cartridgeType: CartridgeType,
                           romSize: ROMSize,
                           ramSize: RAMSize,
                           destinationCode: DestinationCode,
                           oldLicensee: UInt8,
                           maskRom: UInt8,
                           headerChecksum: UInt8,
                           globalChecksum: UInt16      // Vec[UInt8, 2]
                          )


def verifyLogo(logo: Vector[UInt8]): Unit = 
  val nintendoLogo: Vector[UInt8] = Vector(
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E
  ).map(_.u8)

  if logo.sameElements(nintendoLogo) then 
    println("* Logo check passed")
  else 
    println("* Logo check failed, bootleg rom?") 

def verifyHeaderCksum(toCksum: Vector[UInt8], cksum: UInt8): Unit =
  if toCksum.length != 0x19 then 
    println(s"* Header payload passed in for checksum incorrect size: ${toCksum.length} expected 25")

  val actualCksum = List.range(0, toCksum.length).foldLeft(0.u8) { (acc: UInt8, i: Int) => 
    acc - toCksum(i) - 1.u8
  }

  if actualCksum == cksum then
    println("* Header Checksum passed")
  else 
    println(s"* Header Checksum failed ${actualCksum} != ${cksum}, corrupt rom?")

def verifyGlobalCksum(toCksum: Vector[UInt8], cksum: UInt16): Unit =
  val actualCksum = List.range(0, toCksum.length).foldLeft(0.u16) { (acc: UInt16, i: Int) => 
    if i == 0x014E || i == 0x014F then acc 
    else acc + UInt16(toCksum(i).toShort)
  }

  if (actualCksum & 0xFFFF.u16) == cksum then
    println("* Global Checksum passed")
  else 
    println(s"* Global Checksum failed ${actualCksum} != ${cksum}, corrupt rom?")

def readCartridgeHeader: Binary[CartridgeHeader] =
  for
    logo <- readUInt8s(0x30)
    title <- readString(0x0b)
    manu <- readString(4)
    cgb <- readCGB
    licensee_code_1 <- readChar
    licensee_code_2 <- readChar
    licensee_code = Try { Integer.parseInt(Vector(licensee_code_1, licensee_code_2).mkString(""), 16) }.toOption
    licensee = licensee_code.flatMap(NewLicensee.get).getOrElse(s"Unknown: ${licensee_code_1}+${licensee_code_2}")
    sgb <- readSGB
    cartTy <- readCartridgeType
    romSz <- readROMSize
    ramSz <- readRAMSize
    dest <- readDestinationCode
    oldLic <- readUInt8
    maskRom <- readUInt8
    headerCk <- readUInt8
    globalCk <- readUInt16
  yield CartridgeHeader(logo, title, manu, cgb, licensee, sgb, cartTy, 
          romSz, ramSz, dest, oldLic, maskRom, headerCk, globalCk)

def prettyPrintHeader(header: CartridgeHeader): Unit = 
  println(s"""
           |Title: ${header.title}
           |Manufacturing Code: ${header.manufacturingCode}
           |Licensee: ${header.licensee}
           |Ram Size: ${header.ramSize}
           |Rom Size: ${header.romSize}
           |Cartridge Type: ${header.cartridgeType}
           |Destination: ${header.destinationCode}
           |Colour: ${header.cgb}
           |Super Gameboy: ${header.sgb}
           """.stripMargin)