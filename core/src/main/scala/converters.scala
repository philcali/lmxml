package lmxml

import xml._

trait LmxmlConverter[A] {
  def convert(nodes: List[ParsedNode]): A 
}

object XmlConverter extends LmxmlConverter[xml.NodeSeq] {
  import xml._

  def convert(nodes: List[ParsedNode]): NodeSeq = nodes match {
    case n :: ns => n match {
      case LmxmlNode(name, attrs, children) =>
        val meta = attrs.map { attr => 
          Attribute(None, attr._1, Text(attr._2), Null)
        }.toList.sortWith(_.key < _.key)

        val input = if (meta.isEmpty) Null else meta.reduceLeft((i, m) => m.copy(i))

        Elem(null, name, input, TopScope, convert(children): _*) ++ convert(ns)
      case TextNode(contents, unparsed, children) =>
        if (unparsed)
          Group(Unparsed(contents) ++ convert(children)) ++ convert(ns)
        else
          Group(Text(contents) ++ convert(children)) ++ convert(ns)
      case _ =>
        Elem(null, n.name, Null, TopScope, convert(n.children): _*) ++ convert(ns)
    }
    case Nil => Nil
  }
}
