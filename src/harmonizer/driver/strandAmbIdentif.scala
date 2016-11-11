package fi.drizzle.imputation.components

import java.io._
import java.util.Calendar

import scala.io._
import scala.language.{implicitConversions, postfixOps}

import fi.drizzle.core._
import fi.drizzle.core.Config._
import fi.drizzle.core.DrizzleCore._


case class VariantMaf(snp: String, a1: Char, a2: Char, maf: Double) {

  val joint = a2.toString+a1

  override def toString() = Array(snp, a1, a2).mkString("\t")
}


case class StrandAmbProfiling(refFile: TextFile, studyFile: TextFile, studyFrqFile: TextFile) {  

  def now() = Calendar.getInstance().getTime

  def readFrq(in: TextFile, idx: Map[String,Int]): Map[String,Double] = {
    println(s"${now()} :: reading $in")

    Source.fromFile(new java.io.File(in), bufferSize = Source.DefaultBufSize * 2).getLines.drop(1).flatMap { line =>
        val xs  = line.trim.split("\\s+")
        val a1 = xs(idx("a1"))
        val a2 = xs(idx("a2"))
        if (a1.size > 1 || a2.size > 1) None
        else {
          Some( xs(idx("snp")) -> xs(idx("maf")).toDouble )
        }
    }.toMap    
  }

  def readPlink(in: TextFile, studyFrqFile: TextFile, idx: Map[String,Int]): Map[(Byte,Int), VariantMaf] = {    
    println(s"${now()} :: reading $in")

    val frq = Map("snp" -> 1, "a1" -> 2, "a2" -> 3, "maf" -> 4)
    val studyMafs = readFrq(studyFrqFile, frq)

    Source.fromFile(new java.io.File(in), bufferSize = Source.DefaultBufSize * 2).getLines.flatMap { line =>
        val xs  = line.split("\t")
        val (a1,a2) = (xs(idx("a1")),xs(idx("a2")))
        if (a1.size > 1 || a2.size > 1) None
        else {
          val chr = xs(idx("chr")).toInt.toByte
          val pos = xs(idx("pos")).toInt
          val snp=xs(idx("snp"))
          Some( (chr,pos) -> VariantMaf(snp=snp, a1=a1.head.toChar, a2=a2.head.toChar, maf=studyMafs(snp)) )
        }
    }.toMap
  }

  def readVcf(in: TextFile, idx: Map[String,Int], skip: String = "#"): Map[(Byte,Int), VariantMaf] = {    
    println(s"${now()} :: reading $in")

    Source.fromFile(new java.io.File(in), bufferSize = Source.DefaultBufSize * 2).getLines.dropWhile(_.startsWith(skip)).flatMap { line =>
        val xs  = line.split("\t")
        val (a1,a2) = (xs(idx("a1")),xs(idx("a2")))
        if (a1.size > 1 || a2.size > 1) None
        else {
          val chr = xs(idx("chr")).toInt.toByte
          val pos = xs(idx("pos")).toInt
          // val gts   = xs.drop(9).map { gt => gt(0).toString + gt(2) }.mkString.filterNot("." contains _)
          // val maf   = gts.groupBy(identity).map { case(k,v) => v.size }.min / gts.size.toDouble 
          val (zeroes, ones) = xs.drop(9).foldLeft( (0,0) ) { case ((z,n),s) => (z+s.take(3).count(_ == '0'), n+s.take(3).count(_ == '1')) }
          val maf = math.min(zeroes, ones) / (zeroes + ones).toDouble
          Some((chr,pos) -> VariantMaf(snp=xs(idx("snp")), a1=a1.head.toChar, a2=a2.head.toChar, maf=maf))
        }
    }.toMap
  }



	def apply(ci: Symbol): TextFile = {

    println(s"${now()} :: running $ci (Merge)...")

    val bim = Map("chr" ->  0, "pos" -> 3, "snp" -> 1, "a1" -> 4, "a2" -> 5)
    val study = readPlink(studyFile, studyFrqFile, bim)

    val vcf = Map("chr" ->  0, "pos" -> 1, "snp" -> 2, "a1" -> 4, "a2" -> 3, "vcfFormat" -> 1)
    val ref = readVcf(refFile, vcf)

    println(s"${now()} :: maf based profilinig strand ambiguous snps...")    
    val metVariantMafs: Array[(Byte,Int)] = {
      val iSet = (ref.keySet intersect study.keySet)
      iSet.filter { case k => (ref(k).joint, study(k).joint) match {
          case ("AT", "TA") | ("TA", "AT") => true
          case ("CG", "GC") | ("GC", "CG") => true
          case _ => false
        } 
      }.toArray.sorted
    }

    //ok:0, flip:1, exclude:2
    val groups = metVariantMafs.map { case mv =>
      val diff = math.abs( ref(mv).maf - study(mv).maf )
      if (study(mv).maf > 0.35) mv -> 2
      else if (ref(mv).a1 == study(mv).a1 && diff > 0.05) mv -> 2
      else if (ref(mv).a1 == study(mv).a1 && diff <= 0.05) mv -> 0
      else if (ref(mv).a1 == study(mv).a2 && diff > 0.05) mv -> 2
      else if (ref(mv).a1 == study(mv).a2 && diff <= 0.05) mv -> 1
      else mv -> 2
    }

    groups.foreach { case (k,v) => v match {
      case 0 => println(s"$k   -> ok")
      case 1 => println(s"$k   -> flip")
      case 2 => println(s"$k   -> exclude")
    }}


    println(s"$now() :: ready.")

		TextFile("mergable.tsv", ci)
	}
}