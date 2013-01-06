package lmxml

trait LmxmlConvert[A] extends (Seq[ParsedNode] => A)

trait SinglePass[A] extends LmxmlConvert[Seq[A]] {
  def single(node: ParsedNode): A

  def apply(nodes: Seq[ParsedNode]): List[A] = nodes.headOption match {
    case Some(n) => single(n) :: apply(nodes.tail)
    case None => Nil
  }
}

case class XmlFormat(width: Int, step: Int) extends (Seq[xml.Node] => String) {
  val printer = new xml.PrettyPrinter(width, step)

  def apply(nodes: Seq[xml.Node]) = printer.formatNodes(nodes)
}

object XmlConvert extends LmxmlConvert[xml.NodeSeq] {

  import xml._

  def apply(nodes: Seq[ParsedNode]): NodeSeq = nodes.headOption match {
    case None => Nil
    case Some(node) => single(node) ++ apply(nodes.tail)
  }

  def single(node: ParsedNode): xml.NodeSeq = node match {
    case LmxmlNode(name, attrs, children) =>
      val meta = attrs.map { attr =>
        Attribute(None, attr._1, Text(attr._2), Null)
      }.toList.sortWith(_.key < _.key)

      val input = if (meta.isEmpty) Null else meta.reduceLeft((i, m) => m.copy(i))

      Elem(null, name, input, TopScope, apply(children): _*)
    case TextNode(contents, unparsed, children) =>
      if (unparsed)
        Group(Unparsed(contents) ++ apply(children))
      else
        Group(Text(contents) ++ apply(children))
    case CommentNode(children) =>
      Group(Unparsed("\n<!--\n") ++ apply(children) ++ Unparsed("\n-->\n"))
    case _ =>
      Elem(null, node.name, Null, TopScope, apply(node.children): _*)
  }
}
