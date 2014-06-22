import sbt._
import sbt.Keys._

object Meta {
  val name = "salgo"
  val organization = "com.github.shivawu"
  val version = "0.1.0-SNAPSHOT"
}

object Versions {
  val scala = "2.11.1"

  val scalaTest = "2.2.0"

  val paradiseVersion = "2.0.0"
}

object ScalgoBuild extends Build {

  lazy val commonSettings = Seq(
    organization := Meta.organization,
  	version := Meta.version,
  	scalaVersion := Versions.scala,
  	scalacOptions ++= Seq("-deprecation", "-feature"),
    scalacOptions in (Compile, console) += "-Ymacro-debug-lite",
    incOptions := incOptions.value.withNameHashing(nameHashing = true),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % Versions.paradiseVersion cross CrossVersion.full)
  )

  lazy val macros = Project(id = "macros", base = file("macros")).settings(commonSettings:_*).settings {
    libraryDependencies += "org.scala-lang" % "scala-reflect" % Versions.scala
  }

  lazy val io = Project(id = "io", base = file("io")).settings(commonSettings:_*).dependsOn(macros).settings {
    libraryDependencies += "org.scalatest" %% "scalatest" % Versions.scalaTest % "test"
  }

  lazy val root = Project(id = "salgo", base = file(".")).settings(commonSettings:_*).aggregate(macros, io)
}
