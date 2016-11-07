package network

import java.io._

import scala.io._
import scala.language.implicitConversions
import scala.sys.process._

import fi.drizzle.core._
import fi.drizzle.core.Config._
import fi.drizzle.core.DrizzleCore._
import fi.drizzle.imputation.components._



object Harmonizer extends App {

  val ref       = TextFile(filename = args(0), ci = "")
  val samples   = TextFile(filename = args(1), ci = "")
  val width     = args(2).toInt

  println(s"ref: $ref  samples: $samples")
  LD(samples, ref, width)('ld)

}

