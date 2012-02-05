import sbt._
import Keys._

object LmxmlBuild extends Build {

  val generalSettings: Seq[Setting[_]] = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.9.1",
    organization := "com.github.philcali",
    version := "0.1.0",
    publishTo := Some("Scala Tools Nexus" at
                      "http://nexus.scala-tools.org/content/repositories/releases/"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
  )

  val scalaTest = Seq (
    crossScalaVersions ++=
      Seq("2.8.0", "2.8.1", "2.9.0", "2.9.0-1", "2.9.1"),
    libraryDependencies <+= (scalaVersion) {
      case v if v.startsWith("2.8") =>
        "org.scalatest" % "scalatest" % "1.3"
      case _ =>
        "org.scalatest" %% "scalatest" % "1.6.1"
    }
  )

  lazy val root = Project(
    "lmxml", file("."), settings = generalSettings
  ) aggregate (app, core)

  lazy val app = Project(
    "lmxml-app",
    file("app"),
    settings = generalSettings ++ Seq (
      scalaVersion := "2.9.1",
      libraryDependencies <+= (sbtVersion) {
        "org.scala-tools.sbt" %% "launcher-interface" % _ % "provided"
      }
    )
  ) dependsOn core

  lazy val cache = Project(
    "lmxml-cache",
    file("cache"),
    settings = generalSettings ++ scalaTest
  ) dependsOn core

  lazy val template = Project(
    "lmxml-template",
    file("template"),
    settings = generalSettings ++ scalaTest
  ) dependsOn core

  lazy val core = Project(
    "lmxml-core",
    file("core"),
    settings = generalSettings ++ scalaTest
  )

  lazy val example = Project(
    "lmxml-example",
    file("example"),
    settings = generalSettings ++ Seq(
      libraryDependencies ++= Seq(
        "net.databinder" %% "unfiltered-filter" % "0.5.3",
        "net.databinder" %% "unfiltered-jetty" % "0.5.3"
      )
    )
  ) dependsOn (template, cache)
}
