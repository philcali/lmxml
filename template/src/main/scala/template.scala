package lmxml

package template

import java.io.File
import scala.io.Source.{fromFile => open}

trait FileTemplate extends LmxmlParsers {

  val working: File

  def fileNodes(file: File) = {
    val contents = open(file).getLines.mkString("\n")
    parseNodes(contents)
  }

  def isAvailable(name: String) =
    new File(working, name + ".lmxml").exists()

  override def rebuild(n: Nodes, link: LinkDefinition): Nodes = n match {
    case (h: TemplateLink) :: rest if isAvailable(h.name) =>
      val temp = new File(working, h.name + ".lmxml")
      val parentNodes = fileNodes(temp)
      rebuild(parentNodes, link) ++ rebuild(rest, link)
    case _ => super.rebuild(n, link)
  }
} 
