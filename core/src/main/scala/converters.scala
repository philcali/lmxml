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
        }

        val input = if (meta.isEmpty) Null else meta.reduceLeft((i, m) => i.copy(m))

        Elem(null, name, input, TopScope, convert(children): _*) ++ convert(ns)
      case TextNode(contents, children) =>
        Group(Text(contents) ++ convert(children)) ++ convert(ns)
      case _ =>
        Elem(null, n.name, Null, TopScope, convert(n.children): _*) ++ convert(ns)
    }
    case Nil => Nil
  }
}
