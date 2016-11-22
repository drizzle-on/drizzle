package fi.drizzle.imputation.components

import java.io._
import java.util.Calendar

import scala.io._
import scala.language.{implicitConversions, postfixOps}

import fi.drizzle.core._
import fi.drizzle.core.Config._
import fi.drizzle.core.DrizzleCore._


case class VcfVariantFreq(chr: Byte, pos: Long, snp: String, ref: Char, a2: Char, afRef: Double, afAlt: Double) {
  override def toString() = this.productIterator.map(_.toString).mkString("", "\t", "\n")
}


case class VcfFreqs(vcfFile: TextFile) {  

  def now() = Calendar.getInstance().getTime


  def readVcf(in: TextFile, idx: Map[String,Int], skip: String = "#") = {    
    println(s"${now()} :: reading $in")

    Source.fromFile(new java.io.File(in), bufferSize = Source.DefaultBufSize * 2).getLines.dropWhile(_.startsWith(skip)).flatMap { line =>
        val xs  = line.split("\t")
        val (ref,alt) = (xs(idx("ref")),xs(idx("alt")))
        if (ref.size > 1 || alt.size > 1) None
        else {
          val chr = xs(idx("chr")).toInt.toByte
          val pos = xs(idx("pos")).toInt
          val (zeroes, ones) = xs.drop(9).foldLeft( (0,0) ) { case ((z,n),s) => (z+s.take(3).count(_ == '0'), n+s.take(3).count(_ == '1')) }
          val afRef = zeroes / (zeroes + ones).toDouble
          val afAlt = ones / (zeroes + ones).toDouble
          Some(VcfVariantFreq(chr, pos, xs(idx("snp")), ref.head.toChar, alt.head.toChar, afRef, afAlt).toString)
        }
    }.toArray
  }


	def apply(ci: Symbol): TextFile = {

    println(s"${now()} :: running $ci (VcfFreqs). Parsing")

    val vcfFormat = Map("chr" ->  0, "pos" -> 1, "snp" -> 2, "ref" -> 3, "alt" -> 3)
    val vcf = readVcf(vcfFile, vcfFormat)

    println(s"${now()} :: writing down AF's...") 
    val header = Array("chr", "pos", "snp", "ref", "alt", "afRef", "afAlt").mkString("", "\t", "\n")

	  TextFile(vcfFile + ".freqs", ci).write(header + vcf.mkString)
	}
}

object VcfFreqsRun extends App {
  VcfFreqs(TextFile(args(0), Symbol("")))('vcfFreqsRun)
}