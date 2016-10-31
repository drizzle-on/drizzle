package fi.drizzle.imputation.components

import java.io._

import scala.io._
import scala.language.implicitConversions
import scala.sys.process._

import fi.drizzle.core._
import fi.drizzle.core.Config._
import fi.drizzle.core.DrizzleCore._


/**
* Extract chromosomes for a given VCF file.
*/
case class ScatterPlot(inVcf: TextFile) {

  def apply(ci: Symbol): TextFile = {
  
    println(s"running $ci (ScatterPlot)...")

    val scatterPlot = TextFile("scatterPlot", ci)

    // bash"python matplotlib call ..."

    scatterPlot
  }

}