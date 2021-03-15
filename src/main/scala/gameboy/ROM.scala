package gameboy

import util.*

import cats.free.*
import cats.free.Free.liftF
import cats.{given, *}

enum RomA[A]:
  case Peek(addr: UInt16) extends RomA[UInt16]

type Rom[A] = Free[RomA, A]

// def romPeek(addr: UInt16): Rom[UInt16] = liftF[]

enum RamA[A]:
  case Peek(addr: UInt16) extends RamA[UInt16]
  case Poke(addr: UInt16, v: UInt16) extends RamA[Unit]

type Ram[A] = Free[RamA, A]