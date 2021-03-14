package util

import cats.data.State
import cats.implicits.{given, *}

case class BinaryCtx(cursor: Int, data: Vector[Byte]):
  def next: (BinaryCtx, Byte) = (BinaryCtx(cursor + 1, data), data(cursor))

type Binary[A] = State[BinaryCtx, A]

def runWithData[A](m: Binary[A], data: Vector[Byte]): A =
  m.runA(BinaryCtx(0, data)).value

def readByte: Binary[Byte] = State(_.next)

def readChar: Binary[Char] = readByte.map(_.toChar)

def readBytes(n: Int): Binary[Vector[Byte]] = State { ctx => 
  (ctx.copy(cursor = ctx.cursor + n), ctx.data.slice(ctx.cursor, ctx.cursor + n))
}

def readString(n: Int): Binary[String] = readBytes(n).map(_.map(_.toChar).mkString(""))

def readUInt8s(n: Int): Binary[Vector[UInt8]] = readBytes(n).map(_.map(UInt8.apply))

def readUInt8: Binary[UInt8] = readByte.map((b: Byte) => UInt8(b))

def readUInt16: Binary[UInt16] =
  for
    b1 <- readByte
    b2 <- readByte
  yield UInt16((b1 << 8) | (b2 & 0xFF))

// def readUInt8Vec[N <: Int]: Binary[Vec[UInt8, N]] = 
//   readBytes(constValue[N])

