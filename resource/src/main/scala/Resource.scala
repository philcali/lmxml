package lmxml
package resource

import java.io.File
import io.Source.{ fromFile => open }

trait ResourceLoader extends (Map[String, String] => Seq[ParsedNode] => ParsedNode)

object TextResource extends ResourceLoader {
  def fromFile(file: String) = open(file).getLines.mkString("\n")

  def apply(attrs: Map[String, String]) = {
    TextNode(fromFile(attrs("file")), false, _)
  }
}

case class LmxmlResource(parser: LmxmlParsers) extends ResourceLoader {
  val reserved = List("type", "node", "file")

  def apply(attrs: Map[String, String]) = attrs match {
    case _ if attrs.get("type") == Some("lmxml") =>
      val source = TextResource.fromFile(attrs("file"))
      parser.safeParseNodes(source) fold ({
        _ => TextNode(source, false, _)
      }, { nodes => (level: Seq[ParsedNode]) => {
          if (attrs.contains("node")) {
            LmxmlNode(attrs("node"), attrs -- reserved, nodes ++ level)
          } else {
            EmptyNode(nodes ++ level)
          }
        }
      })
    case _ => TextResource(attrs)
  }
}

trait FileResources extends LmxmlParsers {
  val resourceLoader: ResourceLoader

  def defaultLoad: PartialFunction[Attrs, TopLevel] = {
    case attrs => LmxmlNode("load", attrs, _)
  }

  def load: PartialFunction[Attrs, TopLevel] = {
    case attrs if (attrs.contains("file") && new File(attrs("file")).exists) =>
      resourceLoader(attrs)
  }

  val resources: Parser[TopLevel] = "load" ~> inlineParams ^^ (
    load orElse defaultLoad
  )

  override def topLevel = resources | super.topLevel
}
