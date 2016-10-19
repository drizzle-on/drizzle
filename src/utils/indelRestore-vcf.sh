#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

import scala.io._
import java.io._

case class Table(tableFile: String) {
  val table: Map[(String,String), Option[(String,String)]] = {
    Source.fromFile(tableFile).getLines.map {line =>
      val Array(chr, pos, indelType, orig, rndBase) = line.split("\t")
      ((chr, pos), Some((indelType, orig)))
    }.toMap
  }

  def apply(chr: String, pos: String) = table.getOrElse((chr,pos), None) 
}

  
if (args.size < 2) 
  sys.exit{println("usage: indelsRestore trimmedVcfFile hashtableFile"); 1}

def toStr(xs: Array[String]) = xs.mkString("", "\t", "\n")  

val recoded = args(0)
val tableFile = args(1)
val decodedFile = s"$recoded.restored.bim"
val decodedFw = new PrintWriter(new File(decodedFile))

val table = Table(tableFile)

for (line <- Source.fromFile(recoded).getLines) {
  if (line.startsWith("#")) decodedFw.write(line + "\n")
  else {
    val Array(chr, pos, misc, ref, alt, rest @ _*) = line.split("\t")

    val decoded = 
      table(chr, pos) match {
        case Some(("ins", orig)) => toStr(Array(chr, pos, misc, ref, orig) ++ rest)
        case Some(("del", orig)) => toStr(Array(chr, pos, misc, orig, alt) ++ rest)
        case _                   => line + "\n"
      }

    decodedFw.write(decoded)    
  }
}

decodedFw.close()

