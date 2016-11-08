package fi.drizzle.imputation.components

import java.io._

import scala.io._
import scala.language.{implicitConversions, postfixOps}
import scala.math._
import scala.sys.process._

import fi.drizzle.core._
import fi.drizzle.core.Config._
import fi.drizzle.core.DrizzleCore._


case class VariantLD(ref: String, alt: String, genotypes: Array[Char]) {

  val nonAmbiguous = Set ( ("A","T"), ("T", "A"), ("C", "G"), ("G", "C") )

  def isMultiallelic(alt: String) = alt.exists(_ == ',')  // alt contains comma

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
      Source.fromFile(in).getLines.dropWhile(_.startsWith("#")).map { line =>
        val xs    = line.split("\t")
        val chr   = xs(0).toInt.toByte // XXX chrXX 
        val pos   = xs(1).toInt
        val ref   = xs(3).toUpperCase
        val alt   = xs(4).toUpperCase

        // For easing use of Integer.bitCount, VCF 0|0 (ref|ref) becomes 1|1 here.
        val gts   = xs.drop(9).flatMap { gt => Array(gt(0), gt(2)).map(v => if (v != '0') '0' else '1') }
        val filledGts = gts ++ Array.fill(gts.size % 16)("0")  // Char 16 bit to reduce memory use.
        val binGts    = filledGts.grouped(16).map { g => Integer.parseInt(g.mkString, 2).toChar }.toArray
        ((chr, pos), VariantLD(ref, alt, binGts))
      
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

      (mv, flankingRegion.map { fr => centralVarLD.rCoeff(assocMap(fr), 2*nIndivs) })
    }
  }


  def corr(vec1: Array[Double], vec2: Array[Double]): Double = {
    val zs = vec1.zipAll(vec2, 0D, 0D) // Fill up shorter vector with 0D's.
    val xsMean = vec1.sum / zs.size   //vec1.size
    val ysMean = vec2.sum / zs.size   //vec2.size

    val a = zs.map { case (x,y) => (x-xsMean) * (y-ysMean) }.sum
    val (b1, b2) = zs.map { case (x,y) => ( math.pow(x-xsMean,2), math.pow(y-ysMean,2)) }.unzip
    
    a / math.sqrt(b1.sum * b2.sum)
  }

  // Hausdorff distance (set similarity up to given metric); increase sets to disprove.
  def euclidean(x: Double, y: Double) = math.abs(x-y)

  def hd(xs: Array[Double], ys: Array[Double], d: (Double,Double) => Double): Double = xs.map(x => ys.map(y => d(x,y)).min).max


  def apply(ci: Symbol): TextFile = {
  
    println(s"running $ci (LD)...")

    val (nRefIndivs, refGTs)         = read2VariantLD(ref)
    val (nSamplesIndivs, samplesGTs) = read2VariantLD(samples)

    val metVariants: Array[(Byte,Int)] = (refGTs.keySet intersect samplesGTs.keySet).toArray.sorted
    
    val refLDs     = regionLDs(metVariants, refGTs, nRefIndivs, width)
    val samplesLDs = regionLDs(metVariants, samplesGTs, nSamplesIndivs, width)

    refLDs.toSeq.zip(samplesLDs.toSeq).foreach { case ((kRef,xs), (kSam,ys)) => 
      val r = corr(xs, ys).toString.take(5).mkString
      val h = hd(xs, ys, euclidean _).toString.take(5).mkString
      println(s"$kRef r=$r   hd=$h")
    }

    TextFile("outLD.vcf", ci)
  }

}