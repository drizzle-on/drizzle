package fi.drizzle.imputation.components

import java.io._
import java.util.{Calendar,StringTokenizer}

import scala.collection.JavaConverters._
import scala.io._
import scala.language.{implicitConversions, postfixOps}
import scala.math._
import scala.sys.process._

import fi.drizzle.core._
import fi.drizzle.core.Config._
import fi.drizzle.core.DrizzleCore._


case class VariantLD(ref: String, alt: String, snp: String, genotypes: Array[Char]) {

  val joint = ref+alt

  def nBits(ch: Char): Int = Integer.bitCount(ch.toInt)

  def p(n: Int): Double = genotypes.map { igt => nBits(igt) }.sum.toDouble / n

  def rCoeff(that: VariantLD, n: Int): Double = { // n samples
    val pAB: Double = genotypes.zip(that.genotypes).map { case(i,j) => nBits( (i & j).toChar ) }.sum.toDouble / n
    val pA: Double  = p(n)
    val pB: Double  = that.p(n)
    val D: Double   = pAB - pA*pB

    if (D == 0.0) D else D / math.sqrt( pA * (1-pA) * pB * (1-pB) )
  }

  override def toString() = genotypes.map(_.toInt.toBinaryString).mkString(",")
}


// Linkage disequilibrium. Input VCF files.
// Upper and lower flanking windows total summed length.
case class LD(samples: TextFile, ref: TextFile, width: Int) {

  def read2VariantLD(in: TextFile): (Int, Map[(Byte,Int), VariantLD]) = {

    val nIndivs = Source.fromFile(in).getLines.dropWhile(!_.startsWith("#CHROM")).take(1).toArray.head.split("\t").drop(9).size
    
    val assocMap = {
      Source.fromFile(new java.io.File(in), bufferSize = Source.DefaultBufSize * 8).getLines.dropWhile(_.startsWith("#")).map { line =>
        val xs    = line.split("\t")
        val chr   = xs(0).toInt.toByte // XXX chrXX 
        val pos   = xs(1).toInt
        val snp   = xs(2)
        val ref   = xs(3).toUpperCase
        val alt   = xs(4).toUpperCase

        // For easing use of Integer.bitCount, VCF 0|0 (ref|ref) becomes 1|1 here.
        val gts   = xs.drop(9).flatMap { gt => Array(gt(0), gt(2)).map(v => if (v != '0') '0' else '1') }
        val filledGts = gts ++ Array.fill(gts.size % 16)("0")  // Char 16 bit to reduce memory use.
        val binGts    = filledGts.grouped(16).map { g => Integer.parseInt(g.mkString, 2).toChar }.toArray
        ((chr, pos), VariantLD(ref, alt, snp, binGts))
      
      }.toMap
    }

    (nIndivs, assocMap)
  }


  def regionLDs(metVariants: Array[(Byte,Int)], assocMap: Map[(Byte,Int), VariantLD], nIndivs: Int, width: Int) = {

    val positions = assocMap.keySet.toArray.sorted
    val positionsMap = positions.zipWithIndex.toMap

    metVariants.par.map { mv => 
      val i = positionsMap(mv)
      val flankingRegion = positions.slice(i-width/2, i) ++ positions.slice(i+1, i+width/2+1)
      val centralVarLD = assocMap(mv)

      (mv, flankingRegion.map { fr => ((fr._1, fr._2, assocMap(fr).joint), centralVarLD.rCoeff(assocMap(fr), 2*nIndivs)) }.filter(_._2 >= 0.3).toMap )
    }.toMap
  }


  def corr(vec1: Array[Double], vec2: Array[Double]): Double = {
    // XXX filter LD's by threshold
    val zs = vec1.zip(vec2)
    val xsMean = vec1.sum / zs.size 
    val ysMean = vec2.sum / zs.size 

    val a = zs.map { case (x,y) => (x-xsMean) * (y-ysMean) }.sum
    val (b1, b2) = zs.map { case (x,y) => ( math.pow(x-xsMean,2), math.pow(y-ysMean,2)) }.unzip
    
    a / (math.sqrt(b1.sum) * math.sqrt(b2.sum))
  }

  // Hausdorff distance (set similarity up to given metric); increase sets to disprove.
  def euclidean(x: Double, y: Double) = math.abs(x-y)

  // Symmetric Hausdorff.
  def hd(xs: Array[Double], ys: Array[Double], d: (Double,Double) => Double): Double = 
    Seq(xs.map(x => ys.map(y => d(x,y)).min).max, ys.map(x => xs.map(y => d(x,y)).min).max).max

  def now() = Calendar.getInstance().getTime

  def apply(ci: Symbol): TextFile = {
  
    println(s"${now()} :: running $ci (LD)...")
    println(s"${now()} :: reading input...")

    // val (nRefIndivs, refGTs)         = read2VariantLD(ref)
    // val (nSamplesIndivs, samplesGTs) = read2VariantLD(samples)
    val xs = Array(ref, samples).par.map(read2VariantLD).toList
    val (nRefIndivs, refGTs)         = xs.head
    val (nSamplesIndivs, samplesGTs) = xs.last

    println(s"${now()} :: estimating LD coeffs over flanking regions...")

    val metVariants: Array[(Byte,Int)] = {
      val iSet = (refGTs.keySet intersect samplesGTs.keySet)
      iSet.filter { case k => (refGTs(k).joint, samplesGTs(k).joint) match {
          case ("AT", "TA") | ("TA", "AT") => true
          case ("CG", "GC") | ("GC", "CG") => true
          case _ => false
        } 
      }.toArray.sorted
    }

    val refLDs     = regionLDs(metVariants, refGTs, nRefIndivs, width)
    val samplesLDs = regionLDs(metVariants, samplesGTs, nSamplesIndivs, width)

    println(s"${now()} :: estimating Pearson correlation...")

    val aligned = metVariants.map { mv =>  
      val refFrMap   = refLDs(mv)
      val studyFrMap = samplesLDs(mv)

      val overlapKeys = (refFrMap.keySet intersect studyFrMap.keySet).toArray.sorted
      val (xs,ys) = overlapKeys.map { k => (refFrMap(k), studyFrMap(k)) }.unzip
      val r = corr(xs.toArray, ys.toArray)
      (samplesGTs(mv), r)
    }

    aligned.foreach { case(variant, r) => println(s"${variant.snp}, r=${r.toString.take(5).mkString}") }
/*
    val res = refLDs.toSeq.zip(samplesLDs.toSeq).map { case ((kRef,xs), (kSam,ys)) =>  (kRef, corr(xs, ys)) }.toList
    res.sortBy( t => (t._3, -t._2) ).foreach { case(kRef, r, h) => println(s"$kRef   r=${r.toString.take(5).mkString}   hd=${h.toString.take(5).mkString}") }
*/
    TextFile("outLD.vcf", ci)
  }

}