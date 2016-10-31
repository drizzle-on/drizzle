package fi.drizzle.imputation.components

import java.io._

import scala.io._
import scala.language.{implicitConversions, postfixOps}
import scala.sys.process._

import fi.drizzle.core._
import fi.drizzle.core.Config._
import fi.drizzle.core.DrizzleCore._


/**
* Extract chromosomes for a given VCF file.
* Note: cols index; first col with value 0.
*/
case class ExtractCols(inCSV: TextFile, cols: List[Int]) {

  def apply(ci: Symbol): TextFile = {
  
    println(s"running $ci (ExtractCols)...")

    val out = TextFile("out.vcf", ci)
    val fw = new PrintWriter(new File(out.path))

    Source.fromFile(inCSV.path).getLines.foreach { line => 
    	val xs = line.split("\t", 9)
    	fw.print(cols.map(xs).mkString("", "\t", "\n"))
    }

    fw.close()

    out
  }

}