package lmxml
package app

import markdown.MarkdownConvert
import template.FileTemplates
import resource.{ FileResources, LmxmlResource }
import shortcuts.html.HtmlShortcuts

import scala.io.Source.fromFile

import java.io.File

import java.net.InetAddress.{ getLocalHost => local }
import util.control.Exception.{ allCatch => all }

import transforms.{ Transform, Value }
import transforms.json.JSTransform

import converters.json.{ JsonConvert, JsonFormat }

class AppBundle(path: File) extends LmxmlFactory with FileLoading {
  trait FullParser extends FileResources with FileTemplates with HtmlShortcuts {
    val working = path.getParentFile()
  }

  def createParser(step: Int) =
    new PlainLmxmlParser(step) with FullParser { self =>
      val resourceLoader = LmxmlResource(this)
    }
}

object LmxmlApp {
  val version = "0.1.3"

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

      val trans = jfile.map(fromFile)
        .map(_.getLines.mkString("\n"))
        .map(JSTransform().parse).map(_ + default)
        .getOrElse(default) andThen MarkdownConvert

      val ops = out.filter(_.endsWith(".json")).map(_ =>
        JsonConvert andThen JsonFormat
      ).getOrElse(
        XmlConvert andThen XmlFormat(300, 2)
      )

      factory.fromFile(path)(trans andThen ops andThen output)
    } catch {
      case e => println(e.getMessage())
    }
  }

  def main(args: Array[String]) {
    val JSONOption = """\s*-j (.*\.json)\s*""".r

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
