import gameboy.CartridgeHeader
import scodec.codecs.uint8

import scala.compiletime.S
import scala.compiletime.ops.int._
import scala.compiletime.ops.boolean._
import scala.compiletime.ops.any._
import scala.compiletime.{constValue, error, summonInline}
import util.Vec
import util.UInt8
import util.u8

import scala.util.Successgiven

import java.nio.file.{Files, Path}

sealed trait Decoder[A]
case class Fail[A](rest: Vector[Byte], message: String) extends Decoder[A]
case class Partial[A](rest: Option[Vector[Byte]] => Decoder[A]) extends Decoder[A]
case class Done[A](rest: Vector[Byte], res: A) extends Decoder[A]
case class BytesRead[A](pos: Long, next: Long => Decoder[A]) extends Decoder[A]

type Success[A, R] = Vector[Byte] => A => Decoder[R]

// Binary parser state/codensity monad
case class Get[A](runCont: [R] => Vector[Byte] => (Success[A, R] => Decoder[R])) {
  def flatMap[A, B](m: Get[A])(f: A => Get[B]): Get[B] =
    Get([R] => (i: Vector[Byte]) => (ks: Success[B, R]) => m.runCont(i)((ii: Vector[Byte]) => (a: A) => f(a).runCont(ii)(ks)))
  
  def map[A, B](m: Get[A])(f: A => B): Get[B] =
    Get([R] => (i: Vector[Byte]) => (ks: Success[B, R]) => m.runCont(i)((ii: Vector[Byte]) => (a: A) => ks(ii)(f(a))))
}

def getUInt8: Get[UInt8] =
  unsafeReadN(1, (v: Vector[Byte]) => UInt8(v.head))

def unsafeReadN[A](n: Int, f: Vector[Byte] => A): Get[A] =
  Get([R] => (input: Vector[Byte]) => (ks: Success[A, R]) => ks(input.drop(n))(f(input)))

def bind[A, B](m: Get[A])(f: A => Get[B]): Get[B] =
  Get([R] => (i: Vector[Byte]) => (ks: Success[B, R]) => m.runCont(i)((ii: Vector[Byte]) => (a: A) => f(a).runCont(ii)(ks)))

def test: Get[UInt8] = 
  for {
    logo: UInt8 <- getUInt8
  } yield logo

// def u16: UInt16 = UInt16(c)
// def u32: UInt32 = UInt32(c)

/*

 */

def readROM[N <: Int](file: String): Vector[UInt8] =
  Files
    .readAllBytes(Path.of(file))
    .toVector
    .map(_.u8)

class UInt16(val signed: Char):
  def toByte = signed.toByte

class UInt32(val signed: Int):
  def toByte = signed.toByte

object Main:
  //def readHeader(rom: Vector[UInt8]): Option[CartridgeHeader] = {
    
    //Vec.vectorSlice[UInt8, 0x0104, 0x0104+0x30](rom) match {
    //  case Some(x) => CartridgeHeader(x.slice[0, 30], x.slice[30, 30+11])
    //  case None => None
    //}
  //}

  def main(args: Array[String]): Unit =
    val rom = readROM("space_invaders.gbc")
    //readHeader(rom)
    println("Hello world!")
    println(msg)

  def msg = "I was compiled by dotty :)"

