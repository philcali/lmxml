package lmxml

package template

import java.io.File
import scala.io.Source.{fromFile => open}

trait FileTemplates extends LmxmlParsers {
  val working: File

  object AvailableFile {
    def unapply(node: ParsedNode) = node match {
      case TemplateLink(name, _) =>
        val file = new File(working, name + ".lmxml")
        if (file.exists) Some(file) else None
      case _ => None
    }
  }

  def fileNodes(file: File) = {
    val contents = open(file).getLines.mkString("\n")
    parseNodes(contents)
  }

  def isAvailable(name: String) =
    new File(working, name + ".lmxml").exists()

  override def rebuild(n: Nodes, link: LinkDefinition): Nodes = n match {
    case AvailableFile(temp) :: rest =>
      rebuild(fileNodes(temp), link) ++ rebuild(rest, link)
    case _ => super.rebuild(n, link)
  }
} 
