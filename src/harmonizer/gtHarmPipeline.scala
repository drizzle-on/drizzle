package network

import java.io._

import scala.io._
import scala.language.implicitConversions
import scala.sys.process._

import fi.drizzle.core._
import fi.drizzle.core.Config._
import fi.drizzle.core.DrizzleCore._
import fi.drizzle.imputation.components._



object GenotypeHarmonizerPipeline extends App {

  val refVcf   = TextFile(filename = args(0), ci = "")
  val studyBim = TextFile(filename = args(1), ci = "")
  val nonHarmo = Harmonizable(refVcf, studyBim)('merging)

  val studyVcf = TextFile(filename = args(2), ci = "")
  val width    = args(3).toInt
  val ld       = LD(studyVcf, refVcf, width, nonHarmo)('LD)

  val studyFrq = TextFile(filename = args(4), ci = "")
  val strandAm = StrandAmbProfiling(refVcf, studyBim, studyFrq)('strandAmbProfiling)

}

