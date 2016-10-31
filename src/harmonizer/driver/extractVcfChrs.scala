package fi.drizzle.imputation.components

import java.io._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io._
import scala.language.implicitConversions
import scala.sys.process._

import fi.drizzle.core._
import fi.drizzle.core.Config._
import fi.drizzle.core.DrizzleCore._


/**
* Extract chromosomes for a given VCF file.
*/
case class ExtractVcfChrs(inVcf: TextFile) {

  def apply(ci: Symbol): FolderFile = {
  
    println(s"running $ci (ExtractVcfChrs)...")

    val outExtracts = FolderFile("outExtracts", ci)

    val chrs = (1 to 22).map(_.toString).toSet ++ Set("X")
    val path = outExtracts.path
    val pws  = chrs.map(c => c -> new PrintWriter(new FileWriter(s"${outExtracts.path}/chr$c.vcf", true))).toMap

    val lines = Source.fromFile(inVcf.path).getLines
    for (line <- lines) {
        print(".")
        val Array(head, _*) = line.split("\t",2)
        if (head.startsWith("#")) for ((_, pw) <- pws) pw.write(line + "\n")
        else pws(head).write(line + "\n")  
    }

    for ((_, pw) <- pws) pw.close()

    outExtracts
  }

}