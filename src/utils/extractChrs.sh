#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

import scala.io._
import java.io._

val chrs = (1 to 22).map(_.toString).toSet ++ Set("X")

val pws = chrs.map(c => c -> new PrintWriter(new FileWriter(s"chrs/chr$c.vcf", true))).toMap

Source.stdin.getLines.foreach { line =>
  val Array(head, _*) = line.split("\t",2)
  if (head.startsWith("#")) for ((_, pw) <- pws) pw.write(line + "\n")
  else pws(head).write(line + "\n")
}

for ((_, pw) <- pws) pw.close()
