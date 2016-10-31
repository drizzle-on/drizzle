#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

import scala.io._
import java.io._

object Trim {

  type sArray = Array[String]

  val rndBase = Map("A" -> "C", "C" -> "T", "T" -> "G", "G" -> "A")

  def isMultiAllelic(alt: String) = alt.split(",").size > 1

  def apply(line: String): (sArray,sArray,sArray) = {
    val xs = line.split("\t", 6)
    val chr = xs(0)
    val pos = xs(1)
    val ref = xs(3)
    val alt = xs(4)

    val eArray = Array[String]()

    if (isMultiAllelic(alt)) (eArray, eArray, eArray)
    else if ((ref == "*") || (alt == "*")) (eArray, eArray, xs)
    else {
      (ref.length, alt.length) match {
        case (1, 1) => (xs, eArray, eArray)
        case (1, _) =>  val newBase = rndBase(ref)
                        xs(4) = newBase
                        (xs, Array(chr, pos, "ins", alt, newBase), eArray)
        case (_, 1) =>  val newBase = rndBase(alt)
                        xs(3) = newBase
                        (xs, Array(chr, pos, "del", ref, newBase), eArray)
        case _      =>  (xs, eArray, eArray)
      }
    }
  }
}

def toStr(xs: Array[String]) = xs.mkString("", "\t", "\n")

val in = args.lift(0).getOrElse(sys.exit{println("usage: indelTrimming vcfFile"); 1})

val exts = Array( "hash", "trimmed.vcf", "exclusion")
val fws @ Array(tableFw, recodedFw, exclusionFw) = exts.map(ext => new PrintWriter(new File(s"trimmed/$in.$ext")))

for (line <- Source.fromFile(in).getLines) {
  if (line.startsWith("#")) {
    recodedFw.write(line+"\n")
  }
  else {
    val (recoded, tableItem, exclusionItem) = Trim(line) 

    if (recoded.nonEmpty) recodedFw.write(toStr(recoded))
    if (tableItem.nonEmpty) tableFw.write(toStr(tableItem))
    if (exclusionItem.nonEmpty) exclusionFw.write(toStr(exclusionItem))
  }
}

fws.foreach(fw => fw.close())
