package lmxml
package app

import markdown.MarkdownParsing
import template.FileTemplates
import shortcuts.html.HtmlShortcuts

import scala.io.Source.fromFile

import java.io.File

import java.net.InetAddress.{ getLocalHost => local }
import util.control.Exception.{ allCatch => all }

import transforms.{ Transform, Value }
import transforms.json.JSTransform

class AppBundle(path: File) extends LmxmlFactory with FileLoading {
  def createParser(step: Int) =
    new PlainLmxmlParser(step)
      with FileTemplates with HtmlShortcuts with MarkdownParsing {
      val working = path.getParentFile()
    }
}

object LmxmlApp {
  val version = "0.1.2"

  def printHelp = {
    println(
    """
  lmxml v%s, Copyright Philip Cali

  lmxml [-j data.json] file.lmxml [output.ext]
     output defaults to stdout""" format version
    )
  }

  def file(path: String) = new File(path)

  def validFile(f: String) = {
    file(f).exists && file(f).getName.endsWith(".lmxml")
  }

  def process(path: String, jfile: Option[String], out: Option[String]) {
    val output = (data: String) =>
      out.map { f =>
        val writer = new java.io.FileWriter(f)
        writer.write(data)
        writer.close()
      } orElse {
        Some(println(data))
      }

    val default = Transform(
      "lmxml-user" -> Value(util.Properties.userName),
      "lmxml-host" -> Value(all.opt(local().getHostName()).getOrElse("unknown")),
      "lmxml-date" -> Value(new java.util.Date(System.currentTimeMillis)),
      "lmxml-version" -> Value(version)
    )

    try {
      val factory = new AppBundle(file(path))

      val trans = jfile
        .map(fromFile)
        .map(_.getLines.mkString("\n"))
        .map(JSTransform().parse)
        .map(_ + default)
        .getOrElse(default)

      val xmlOps = XmlConvert andThen XmlFormat(300, 2)

      factory.fromFile(path)(trans andThen xmlOps andThen output)
    } catch {
      case e => println(e.getMessage())
    }
  }

  def main(args: Array[String]) {
    val JSONOption = """ -j (.*\.json)""".r

    val argStr = args.mkString(" ")

    val jfile = JSONOption.findFirstMatchIn(argStr).map(_.group(1))

    JSONOption.replaceFirstIn(argStr, "").split(" ") match {
      case Array(filepath, output) if (validFile(filepath)) =>
        process(filepath, jfile, Some(output))
      case Array(filepath) if (validFile(filepath)) =>
        process(filepath, jfile, None)
      case _ =>
        printHelp
    }
  }
}

class LmxmlApp extends xsbti.AppMain {
  case class Exit(code: Int) extends xsbti.Exit

  def run(configuration: xsbti.AppConfiguration) = {
    LmxmlApp.main(configuration.arguments)
    Exit(0)
  }
}
