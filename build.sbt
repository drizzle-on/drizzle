name := "drizzlePipeline"

organization := "fi.drizzle"

version := "0.1"

scalaVersion := "2.10.6"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation", "-Xlint")

libraryDependencies ++= Seq(
    "org.scala-lang"    % "scala-compiler"  % scalaVersion.value,
    "org.scala-lang"    % "scala-reflect"   % scalaVersion.value,
    "org.apache.spark"  % "spark-core_2.10" % "1.6.1",
    "org.scalatest"     % "scalatest_2.11"  % "2.2.6" % Test,
    "com.databricks"    % "spark-csv_2.10"  % "1.4.0",
    "com.typesafe"      % "config"          % "1.0.2"
)

fork in run := true


