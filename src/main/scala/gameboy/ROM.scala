package gameboy

import util.*

import cats.free.*
import cats.free.Free.liftF
import cats.{given, *}

import cats.data.StateT
import cats.data.ReaderT

type Rom[F[_], A] = ReaderT[F, Vector[UInt8], A]
type Ram[F[_], A] = StateT[F, Vector[UInt8], A]


// case class Rom(private val data: Vector[UInt8]):
//   def peek(addr: UInt16): UInt16 = ???
// 
// case class Ram(private val data: Vector[UInt8]):
//   def peek(addr: UInt16): UInt16 = ???
//   def poke(addr: UInt16, value: UInt16): Unit = ???

// enum RomA[A]:
//   case Peek(addr: UInt16) extends RomA[UInt16]
// 
// type Rom[A] = Free[RomA, A]
// 
// // def romPeek(addr: UInt16): Rom[UInt16] = liftF[]
// 
// enum RamA[A]:
//   case Peek(addr: UInt16) extends RamA[UInt16]
//   case Poke(addr: UInt16, v: UInt16) extends RamA[Unit]
// 
// type Ram[A] = Free[RamA, A]