package fi.drizzle.imputation.components

import java.nio.file.{Files, Paths}

import scala.io._
import scala.language.{implicitConversions, postfixOps}


case class BimVariant(chr: Byte, pos: Int, snp: String, a1: Char, a2: Char) 


case class PlinkVariant(bim: BimVariant, maf: Double) {
  val joint = bim.a2.toString + bim.a1.toString
}


case class PlinkFile(path: String) {
  val magicMode = Array[Byte](108, 27, 1)
  val shifts = Array(0, 2, 4, 6)

  def readBim() = {    
    Source.fromFile(new java.io.File(path + ".bim"), bufferSize = Source.DefaultBufSize * 2).getLines.map { line =>
      val xs  = line.split("\t")
      BimVariant(xs(0).toInt.toByte, xs(3).toInt, xs(1), xs(4).head.toChar, xs(5).head.toChar)
    }.toArray
  }

  def readFam() = Source.fromFile(path + ".fam").getLines.map(_.split("\\s+").head).toArray

  def readBed() = {
    val xs = Files.readAllBytes(Paths.get(path + ".bed"))
    require(xs.take(3).sameElements(magicMode), s"not bed format: $path")
    
    val bim = readBim()
    val fam = readFam()
    val extraFam = fam.size % 4
    val bytesPerVariant = if (extraFam == 0) (fam.size / 4) else (fam.size / 4 + 1)

    xs.drop(3).grouped(bytesPerVariant).zip(bim.iterator).map { case (xsBytes, bimVariant) =>
      val initBytesAlleles = xsBytes.init.flatMap { case b => shifts.map (sh => (b >>> sh & 3) ) }
      val lastByteAlleles = shifts.take(extraFam).map (sh => (xsBytes.last >>> sh & 3))

      val (zeroes,ones) = (initBytesAlleles ++ lastByteAlleles).foldLeft((0,0)) { case ((z,on),c) => c match {
            case 3 => (z,   on+2)
            case 2 => (z+1, on+1)
            case 0 => (z+2, on)
            case _ => (z,   on)
          }}

      val maf = math.min(zeroes, ones) / (zeroes + ones).toDouble

      PlinkVariant(bimVariant, maf) 
    }
  }
}

object PlinkParser extends App {
  val plinkVariants = PlinkFile(args(0)).readBed()
  plinkVariants.foreach(println)
}