package lmxml

package template

import java.io.File
import scala.io.Source.{fromFile => open}

class TemplateConvert(replaced: Seq[ParsedNode]) extends LmxmlConvert[Seq[ParsedNode]] {
  def apply(nodes: Seq[ParsedNode]): Seq[ParsedNode] = nodes match {
    case n :: rest =>
      if (n.name == "wrap-contents")
        replaced ++ apply(rest)
      else apply(rest) :+ copyNode(n) 
  } 

  def copyNode(n : ParsedNode) = n match {
    case l: LmxmlNode => l.copy(children = apply(l.children))
    case t: TextNode => t.copy(children = apply(t.children))
    case _ => n
  }
}

trait FileTemplate extends LmxmlParsers {

  val working: File

  def fileNodes(file: File) = {
    val contents = open(file).getLines.mkString("\n")
    parseNodes(contents)
  }

  override def rebuild(n: Nodes, link: LinkDefinition): Nodes = n match {
    case (h: TemplateLink) :: rest =>
      val temp = new File(working, h.name)
      val converter = new TemplateConvert(rebuild(h.children, link))
      converter(fileNodes(temp)) ++ rebuild(rest, link)
    case _ => super.rebuild(n, link)
  }
} 
