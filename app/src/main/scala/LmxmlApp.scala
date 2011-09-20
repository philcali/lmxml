package lmxml
package app

import scala.io.Source.{fromFile => open}

import java.io.File

object LmxmlApp {
  def printHelp = {
    println(
    """  lmxml file.lmxml [output.ext]
        output defaults to stdout""" 
    )
  }

  def file(path: String) = new File(path)

  def validFile(f: String) = {
    file(f).exists && file(f).getName.endsWith(".lmxml")
  }

  def process(path: String, output: Option[String]) {
    val contents = open(path).getLines.mkString("\n")

    val converted = Lmxml.convert(contents)(XmlConverter)

    output.map { f =>
      val writer = new java.io.FileWriter(f)
      writer.write(converted.toString)
      writer.close()
    } orElse {
      Some(println(converted))
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

