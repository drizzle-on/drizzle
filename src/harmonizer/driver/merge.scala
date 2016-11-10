package fi.drizzle.imputation.components

import java.io._
import java.util.Calendar

import scala.io._
import scala.language.{implicitConversions, postfixOps}

import fi.drizzle.core._
import fi.drizzle.core.Config._
import fi.drizzle.core.DrizzleCore._


case class Variant(snp: String, a1: Char, a2: Char) {
  
  val comp = Map('A' -> 'T', 'T' -> 'A', 'C' -> 'G', 'G' -> 'C')
  
  def isHarmonized(that: Variant) = Set(a1, a2, that.a1, that.a2).size == 2

  def complement(): Variant = Variant(snp, comp(a1), comp(a2))

  override def toString() = Array(snp, a1, a2).mkString("\t")
}


case class Harmonizable(refFile: TextFile, studyFile: TextFile) {  

  def now() = Calendar.getInstance().getTime

  def readVariants(in: TextFile, idx: Map[String,Int], skip: String = "#"): Map[(Byte,Int), Variant] = {    
    println(s"${now()} :: reading $in")

    Source.fromFile(new java.io.File(in), bufferSize = Source.DefaultBufSize * 2).getLines.dropWhile(_.startsWith(skip)).flatMap { line =>
        val xs  = line.split("\t")
        val (a1,a2) = (xs(idx("a1")),xs(idx("a2")))
        if (a1.size > 1 || a2.size > 1) None
        else {
          val chr = xs(idx("chr")).toInt.toByte
          val pos = xs(idx("pos")).toInt
          Some((chr,pos) -> Variant(snp=xs(idx("snp")), a1=a1.head.toChar, a2=a2.head.toChar))
        }
    }.toMap
  }


	def apply(ci: Symbol): TextFile = {

    println(s"${now()} :: running $ci (Merge)...")

    val bim = Map("chr" ->  0, "pos" -> 3, "snp" -> 1, "a1" -> 4, "a2" -> 5)
    val study = readVariants(studyFile, bim) 

    val vcf = Map("chr" ->  0, "pos" -> 1, "snp" -> 2, "a1" -> 4, "a2" -> 3)
    val ref = readVariants(refFile, vcf)

    println(s"${now()} :: calculating non harmonizable (to be excluded)...")
    val metVariants = (ref.keySet intersect study.keySet).toArray.sorted
    val nonHarmonized = metVariants.filterNot { case mv => ref(mv).isHarmonized(study(mv)) }
    val nonHarmonizable = nonHarmonized.filterNot { case mv => ref(mv).isHarmonized(study(mv).complement()) }
    
    nonHarmonizable.foreach { case k@(chr,pos) =>
      val variant = study(k)
      println(Array(chr, variant.snp, "0", pos, variant.a1, variant.a2).mkString("\t"))
    }

    println(s"$now() :: ready.")

		TextFile("mergable.tsv", ci)
	}
}