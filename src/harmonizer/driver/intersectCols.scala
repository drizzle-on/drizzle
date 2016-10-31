package fi.drizzle.imputation.components

import java.io._

import scala.io._
import scala.language.{implicitConversions, postfixOps}
import scala.sys.process._

import fi.drizzle.core._
import fi.drizzle.core.Config._
import fi.drizzle.core.DrizzleCore._


/**
* Note: cols index; first col with value 0.
*/
case class IntersectCols(inCSV1: TextFile, cols1: List[Int], inCSV2: TextFile, cols2: List[Int]) {

  def apply(ci: Symbol): TextFile = {
  
    println(s"running $ci (IntersectCols)...")

    val out = TextFile("out.vcf", ci)
    val fw = new PrintWriter(new File(out.path))

    val set1 = Source.fromFile(inCSV.path1).getLines.foreach { line => 
    	val xs = line.split("\t")
    	// XXX cols1.map(xs)
    }

    fw.close()

    out
  }

}