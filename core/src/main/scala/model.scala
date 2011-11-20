package lmxml

trait ParsedNode {
  val name: String
  val children: Seq[ParsedNode]
}

case class LmxmlNode(
  name: String, 
  attrs: Map[String, String] = Map(), 
  children: Seq[ParsedNode] = Nil
) extends ParsedNode

case class TextNode(
  contents: String, 
  unescaped: Boolean = false,
  children: Seq[ParsedNode] = Nil
) extends ParsedNode {
  val name = "text"
}

case class TemplateLink(
  name: String,
  children: Seq[ParsedNode] = Nil
) extends ParsedNode

case class LinkDefinition(
  name: String,
  children: Seq[ParsedNode] = Nil
) extends ParsedNode
