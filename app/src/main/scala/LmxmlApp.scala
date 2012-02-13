package lmxml
package app

import template.FileTemplates
import shortcuts.html.HtmlShortcuts

import scala.io.Source.{fromFile => open}

import java.io.File

class AppBundle(path: File) extends LmxmlFactory with FileLoading {
  def createParser(step: Int) =
    new PlainLmxmlParser(step) with FileTemplates with HtmlShortcuts {
      val working = path.getParentFile()
    }
}

object LmxmlApp {
  def printHelp = {
    println(
    """
  lmxml file.lmxml [output.ext]
     output defaults to stdout""" 
    )
  }

  def file(path: String) = new File(path)

  def validFile(f: String) = {
    file(f).exists && file(f).getName.endsWith(".lmxml")
  }

  def process(path: String, out: Option[String]) {
    val format = new xml.PrettyPrinter(300, 2).formatNodes(_:xml.NodeSeq)

    val output = (data: String) =>
      out.map { f =>
        val writer = new java.io.FileWriter(f)
        writer.write(data)
        writer.close()
      } orElse {
        Some(println(data))
      }

    try {
      val factory = new AppBundle(file(path))

      factory.fromFile(path)(XmlConvert andThen format andThen output)
    } catch {
      case e => println(e.getMessage())
    }
  }

  def main(args: Array[String]) {
    args match {
      case Array(filepath, output) if (validFile(filepath)) =>
        process(filepath, Some(output))
      case Array(filepath) if (validFile(filepath)) =>
        process(filepath, None)
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
