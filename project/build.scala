import sbt._
import Keys._

object LmxmlBuild extends Build {
  val generalSettings: Seq[Setting[_]] = Defaults.defaultSettings ++ Seq(
    organization := "com.github.philcali",
    version := "0.1.0"
  )

  lazy val app = Project(
    "lmxml-app",
    file("app"),
    settings = generalSettings ++ Seq (
      scalaVersion := "2.9.1",
      libraryDependencies += 
        "org.scala-tools.sbt" % "launcher-interface_2.8.1" % "0.10.1" % "provided"
    )
  ) dependsOn core

  lazy val core = Project(
    "lmxml-core",
    file("core"),
    settings = generalSettings ++ Seq (
      scalaVersion := "2.9.1",
      crossScalaVersions ++=
        Seq("2.8.0", "2.8.1", "2.9.0", "2.9.0-1", "2.9.1")
    )
  )
}
