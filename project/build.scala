import sbt._
import Keys._

object LmxmlBuild extends Build {

  val generalSettings: Seq[Setting[_]] = Defaults.defaultSettings ++ Seq(
    scalacOptions += "-deprecation",
    scalaVersion := "2.9.1",
    crossScalaVersions := Seq(
      "2.9.2", "2.9.1-1", "2.9.1", "2.9.0-1", "2.9.0", "2.8.2", "2.8.1"
    ),
    organization := "com.github.philcali",
    version := "0.1.2",
    publishTo <<= version { v =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>https://github.com/philcali/lmxml</url>
      <licenses>
        <license>
          <name>The MIT License</name>
          <url>http://www.opensource.org/licenses/mit-license.php</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:philcali/lmxml.git</url>
        <connection>scm:git:git@github.com:philcali/lmxml.git</connection>
      </scm>
      <developers>
        <developer>
          <id>philcali</id>
          <name>Philip Cali</name>
          <url>http://philcalicode.blogspot.com/</url>
        </developer>
      </developers>
    )
  )

  val scalaTest = Seq (
    libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.2" % "test"
  )

  lazy val root = Project(
    "lmxml", file("."), settings = generalSettings
  ) aggregate (markdown, cache, template, html, json, core)

  lazy val app = Project(
    "lmxml-app",
    file("app"),
    settings = generalSettings ++ Seq (
      libraryDependencies <+= (sbtVersion) {
        "org.scala-tools.sbt" %% "launcher-interface" % _ % "provided"
      }
    )
  ) dependsOn (template, html, markdown)

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

  lazy val html = Project(
    "lmxml-html",
    file("html"),
    settings = generalSettings ++ scalaTest
  ) dependsOn core

  lazy val json = Project(
    "lmxml-json",
    file("json"),
    settings = generalSettings ++ scalaTest
  ) dependsOn core

  lazy val markdown = Project(
    "lmxml-markdown",
    file("markdown"),
    settings = generalSettings ++ scalaTest ++ Seq(
      libraryDependencies <+= (scalaVersion) {
        case v if v startsWith "2.9" =>
         "com.tristanhunt" % "knockoff_2.9.1" % "0.8.0-16"
        case "2.8.2" =>
         "com.tristanhunt" % "knockoff_2.8.1" % "0.8.0-16"
        case _ =>
         "com.tristanhunt" %% "knockoff" % "0.8.0-16"
      }
    )
  ) dependsOn core

  lazy val core = Project(
    "lmxml-core",
    file("core"),
    settings = generalSettings ++ scalaTest
  )

  lazy val example = Project(
    "lmxml-example",
    file("example"),
    settings = Defaults.defaultSettings ++ Seq(
      scalaVersion := "2.9.1",
      organization := "com.github.philcali",
      libraryDependencies ++= Seq(
        "net.databinder" %% "unfiltered-filter" % "0.6.1",
        "net.databinder" %% "unfiltered-jetty" % "0.6.1"
      )
    )
  ) dependsOn (template, cache, html)
}
