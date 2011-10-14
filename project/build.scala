import sbt._
import Keys._

object LmxmlBuild extends Build {
  val generalSettings: Seq[Setting[_]] = Defaults.defaultSettings ++ Seq(
    organization := "com.github.philcali",
    version := "0.1.0",
    publishTo := Some("Scala Tools Nexus" at 
                      "http://nexus.scala-tools.org/content/repositories/releases/"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
  )

  lazy val app = Project(
    "lmxml-app",
    file("app"),
    settings = generalSettings ++ Seq (
      scalaVersion := "2.9.1",
      libraryDependencies += 
        "org.scala-tools.sbt" %% "launcher-interface" % "0.11.0" % "provided"
    )
  ) dependsOn core

  lazy val core = Project(
    "lmxml-core",
    file("core"),
    settings = generalSettings ++ Seq (
      scalaVersion := "2.9.1",
      crossScalaVersions ++=
        Seq("2.8.0", "2.8.1", "2.9.0", "2.9.0-1", "2.9.1"),
      libraryDependencies <+= (scalaVersion) {
        case v if v.startsWith("2.8") =>
          "org.scalatest" % "scalatest" % "1.3"
        case _ =>
          "org.scalatest" %% "scalatest" % "1.6.1"
      }
    )
  )
}
