package gameboy

import util.{UInt8, Vec}
import util.u8

enum CGB(val flag: UInt8):
  case CGBAndGB extends CGB(0x80.u8)
  case CGBOnly extends CGB(0xC0.u8)

enum SGB(val flag: UInt8):
  case NoSGB extends SGB(0x00.u8)
  case SGB extends SGB(0x03.u8)

enum CartridgeType(val cart: UInt8):
  case RomOnly extends CartridgeType(0x00.u8)
  case MBC1 extends CartridgeType(0x01.u8)
  case MBC1Ram extends CartridgeType(0x02.u8)
  case MBC1RamBattery extends CartridgeType(0x03.u8)
  case MBC2 extends CartridgeType(0x05.u8)
  case MBC2Battery extends CartridgeType(0x06.u8)
  case RomRam extends CartridgeType(0x08.u8)
  case RomRamBattery extends CartridgeType(0x09.u8)
  case MMM01 extends CartridgeType(0x0B.u8)
  case MMM01Ram extends CartridgeType(0x0C.u8)
  case MMM01RamBattery extends CartridgeType(0x0D.u8)
  case MBC3TimerBattery extends CartridgeType(0x0F.u8)
  case MBC3TimerRamBattery extends CartridgeType(0x10.u8) 
  case MBC3 extends CartridgeType(0x11.u8) 
  case MBC3Ram extends CartridgeType(0x12.u8) 
  case MBC3RamBattery extends CartridgeType(0x13.u8) 
  case MBC5 extends CartridgeType(0x19.u8) 
  case MBC5Ram extends CartridgeType(0x1A.u8) 
  case MBC5RamBattery extends CartridgeType(0x1B.u8) 
  case MBC5Rumble extends CartridgeType(0x1C.u8) 
  case MBC5RumbleRam extends CartridgeType(0x1D.u8) 
  case MBC5RumbleRamBattery extends CartridgeType(0x1E.u8) 
  case MBC6 extends CartridgeType(0x20.u8) 
  case MBC7SensorRumbleRamBattery extends CartridgeType(0x22.u8) 
  case PocketCamera extends CartridgeType(0xFC.u8) 
  case BandaiTAMA5 extends CartridgeType(0xFD.u8) 
  case HuC3 extends CartridgeType(0xFE.u8) 
  case HuC1RamBattery extends CartridgeType(0xFF.u8)

enum ROMSize(val flag: UInt8): 
  case Rom32KB extends ROMSize(0x00.u8) 
  case Rom64KB extends ROMSize(0x01.u8) 
  case Rom128KB extends ROMSize(0x02.u8) 
  case Rom256KB extends ROMSize(0x03.u8) 
  case Rom512KB extends ROMSize(0x04.u8) 
  case Rom1MB extends ROMSize(0x05.u8) 
  case Rom2MB extends ROMSize(0x06.u8) 
  case Rom4MB extends ROMSize(0x07.u8) 
  case Rom8MB extends ROMSize(0x08.u8) 
  case Rom1_1MB extends ROMSize(0x52.u8) 
  case Rom1_2MB extends ROMSize(0x53.u8) 
  case Rom1_5MB extends ROMSize(0x54.u8)

enum RAMSize(val flag: UInt8): 
  case Ram0 extends RAMSize(0x00.u8) 
  case Ram8KB extends RAMSize(0x02.u8) 
  case Ram32KB extends RAMSize(0x03.u8) 
  case Ram128KB extends RAMSize(0x04.u8) 
  case Ram64KB extends RAMSize(0x05.u8)

enum DestinationCode(val flag: UInt8): 
  case Japanese extends DestinationCode(0x00.u8) 
  case NonJapanese extends DestinationCode(0x01.u8)

case class CartridgeHeader(logo: Vec[UInt8, 30],
                           title: Vec[UInt8, 11],
                           manufacturingCode: Vec[UInt8, 4],
                           cgb: CGB,
                           licensee: Vec[UInt8, 2],
                           sgb: SGB,
                           cartridgeType: CartridgeType,
                           romSize: ROMSize,
                           ramSize: RAMSize,
                           destinationCode: DestinationCode,
                           oldLicensee: UInt8,
                           maskRom: UInt8,
                           headerChecksum: UInt8,
                           globalChecksum: Vec[UInt8, 2]
                          )
