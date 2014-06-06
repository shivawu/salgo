import sbt._
import sbt.Keys._

object Meta {
  val name = "salgo"
  val organization = "com.github.shivawu"
  val version = "0.1.0-SNAPSHOT"
}

object Versions {
  val scalaVersion = "2.11.1"
}

object ScalgoBuild extends Build {

  lazy val commonSettings = Seq(
    organization := Meta.organization,
  	version := Meta.version,
  	scalaVersion := Versions.scalaVersion,
  	scalacOptions ++= Seq("-deprecation", "-feature"),
    scalacOptions in (Compile, console) += "-Ymacro-debug-lite",
      incOptions := incOptions.value.withNameHashing(nameHashing = true)
  )

  lazy val macros = Project(id = "macros", base = file("macros")).settings(commonSettings:_*).settings {
    libraryDependencies += "org.scala-lang" % "scala-reflect" % Versions.scalaVersion
  }

  lazy val io = Project(id = "io", base = file("io")).settings(commonSettings:_*).dependsOn(macros)

  lazy val root = Project(id = "salgo", base = file(".")).settings(commonSettings:_*).aggregate(macros, io)
}
