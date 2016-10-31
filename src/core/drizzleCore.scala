package fi.drizzle.core

import java.io._

import scala.util._
import scala.io._
import scala.sys.process._

import scala.language.implicitConversions


// wd: working dir
object Config {
  var wd: String = "execute"
}


trait CFile { 
  val filename: String 
  val ci: Symbol 
  val path = {
    if (ci.name.isEmpty) filename 
    else Seq(Config.wd, ci.name, filename).mkString(File.separator)
  }
  val lastModified = new File(path).lastModified
  val inOut = filename + "=" + path
  val dir = {
    val dir2 = new File (Seq(Config.wd, ci.name).mkString(File.separator))
    if (!dir2.exists()) dir2.mkdir()
    dir2
  }

  def read() = Source.fromFile(path).getLines.mkString 

  def readCSV(header: Boolean = true) = {
    val n = if (header) 1 else 0
    Source.fromFile(path).getLines.drop(n).map(_.split("\t")).toArray
  }

  def writeFile(msg: String, dest: String = path)(append: Boolean = false) = { 
    val out = new FileWriter(dest, append)
    try { out.write(msg) } finally { out.close() }
  }
}


object DrizzleCore {

  implicit def str2Sym(s: String):   Symbol    = Symbol(s)
  implicit def sym2Str(sym: Symbol): String    = sym.name
  
  implicit class Interp (ctx: StringContext) {
    def bash(args:String*): Int = {
      val str = ctx.parts.zip(args).map{case (p,a)=>s"$p$a"}.mkString("")
      str.!
    }
  }

  def getChromosomes(chromos: Option[String]) = {
    val arg = chromos.getOrElse({println("Missing chromosome(s)."); sys.exit(1)})
    if (arg == "*") (1 to 22).toSeq.map(i => s"chr$i")
    else arg.split(",").toSeq
  }
}


case class TextFile(filename: String, ci: Symbol) extends CFile {
  def write(res: String, append: Boolean = false): TextFile = {
    writeFile(res.toString)(append)
    println(s"$ci => $res")
    this
  }
}


case class FolderFile(filename: String, ci: Symbol, absolute: Boolean = false) extends CFile {
  override val dir = {
    if (absolute) new File(filename) 
    else new File (Seq(Config.wd, ci.name, filename).mkString(File.separator))
  }
  if (!dir.exists()) dir.mkdirs()
  def files() = dir.listFiles.filter(_.isFile)
}
