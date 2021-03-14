import gameboy.*
import util.*
import scodec.codecs.uint8

import java.nio.file.{Files, Path}

def readROM[N <: Int](file: String): Vector[Byte] =
  Files
    .readAllBytes(Path.of(file))
    .toVector

object Main:
  def main(args: Array[String]): Unit =
    val rom = readROM("space_invaders.gbc")
    val header = runWithData(readCartridgeHeader, rom.drop(0x0104))
    verifyLogo(header.logo)
    verifyHeaderCksum(rom.slice(0x0134, 0x014D).map(_.u8), header.headerChecksum)
    verifyGlobalCksum(rom.map(_.u8), header.globalChecksum)
    prettyPrintHeader(header)
    println("Checks passed, ROM ready to boot")

