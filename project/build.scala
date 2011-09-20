import sbt._
import Keys._

object LmxmlBuild extends Build {
  val generalSettings: Seq[Setting[_]] = Defaults.defaultSettings ++ Seq(
    organization := "com.github.philcali",
    version := "0.1.0"
  )

  lazy val root = Project(
    "lmxml",
    file("."),
    settings = generalSettings
  ) aggregate (app, core)

  lazy val app = Project(
    "lmxml-app",
    file("app"),
    settings = generalSettings
  ) dependsOn core

  lazy val core = Project(
    "lmxml-core",
    file("core"),
    settings = generalSettings
  )
}
