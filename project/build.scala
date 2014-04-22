import sbt._
import Keys._

object LmxmlBuild extends Build {

  val generalSettings: Seq[Setting[_]] = Defaults.defaultSettings ++ Seq(
    scalacOptions += "-deprecation",
    scalaVersion := "2.11.0",
    crossScalaVersions := Seq(
      "2.11.0",
      "2.10.3"
    ),
    libraryDependencies <++= (scalaVersion) {
      case sv if sv startsWith "2.11" => Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
      )
      case _ => Seq()
    },
    organization := "com.github.philcali",
    version := "0.1.3",
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

  val knockOff = RootProject(uri("git://github.com/philcali/knockoff.git"))

  val scalaTest = Seq (
    libraryDependencies <+= scalaVersion {
      case sv if sv startsWith "2.11" =>
        "org.scalatest" %% "scalatest" % "2.1.3" % "test"
      case sv if sv startsWith "2.10" =>
        "org.scalatest" %% "scalatest" % "1.9" % "test"
      case _ => "org.scalatest" %% "scalatest" % "1.8" % "test"
    }
  )

  lazy val root = Project(
    "lmxml", file("."), settings = generalSettings
  ) aggregate (markdown, cache, template, html, json, resource, core)

  lazy val app = Project(
    "lmxml-app",
    file("app"),
    settings = generalSettings ++ Seq (
      libraryDependencies <+= (sbtVersion) {
        "org.scala-sbt" % "launcher-interface" % _ % "provided"
      }
    )
  ) dependsOn (template, html, markdown, json, resource)

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
    settings = generalSettings ++ scalaTest
  ) dependsOn (core, knockOff)

  lazy val resource = Project(
    "lmxml-resource",
    file("resource"),
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
    settings = Defaults.defaultSettings ++ Seq(
      scalaVersion := "2.10.0",
      organization := "com.github.philcali",
      libraryDependencies ++= Seq(
        "net.databinder" %% "unfiltered-filter" % "0.6.7",
        "net.databinder" %% "unfiltered-jetty" % "0.6.7"
      )
    )
  ) dependsOn (template, cache, html)
}
