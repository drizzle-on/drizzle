package network

import java.io._

import scala.io._
import scala.language.implicitConversions
import scala.sys.process._

import fi.drizzle.core._
import fi.drizzle.core.Config._
import fi.drizzle.core.DrizzleCore._
import fi.drizzle.imputation.components._


// sbt "run arg1 arg2 ... argN"  
// args: current/working dir, dataPath, ...
object Network extends App {

  val dataPath  = "/data/"
  val inRef   	= TextFile(filename = s"$dataPath/ref.vcf", ci = "")
  val inSamples = TextFile(filename = s"$dataPath/samples.vcf", ci = "")

  val xs = Array((inRef, 'afRef), (inSamples, 'afSamples)).par.map { case(data, ci) => VariantQC(data)(ci) }
  val ys = xs.map { inCSV => ExtractCols(inCSV, List(0,1,6))(inCSV.ci + "_cols") }
  
}
