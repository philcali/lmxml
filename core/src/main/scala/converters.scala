package lmxml

trait LmxmlConvert[A] extends Function1[Seq[ParsedNode], A]

object XmlConvert extends LmxmlConvert[xml.NodeSeq] {

  import xml._

  def apply(nodes: Seq[ParsedNode]): NodeSeq = nodes match {
    case n :: ns => n match {
      case LmxmlNode(name, attrs, children) =>
        val meta = attrs.map { attr => 
          Attribute(None, attr._1, Text(attr._2), Null)
        }.toList.sortWith(_.key < _.key)

        val input = if (meta.isEmpty) Null else meta.reduceLeft((i, m) => m.copy(i))

        Elem(null, name, input, TopScope, apply(children): _*) ++ apply(ns)
      case TextNode(contents, unparsed, children) =>
        if (unparsed)
          Group(Unparsed(contents) ++ apply(children)) ++ apply(ns)
        else
          Group(Text(contents) ++ apply(children)) ++ apply(ns)
      case CommentNode(children) =>
        Group(Unparsed("\n<!--\n") ++ apply(children) ++ Unparsed("\n-->\n")) ++ apply(ns)
      case _ =>
        Elem(null, n.name, Null, TopScope, apply(n.children): _*) ++ apply(ns)
    }
    case Nil => Nil
  }
}
