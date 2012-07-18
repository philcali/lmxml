package lmxml
package converters
package json

import util.parsing.json.{
  JSONArray,
  JSONObject
}

object JsonConvert extends (Seq[ParsedNode] => JSONObject) {
  val intString = """^\d+$""".r

  val decimalString = """^\d+\.\d+$""".r

  val booleanString = """true|false""".r

  def apply(nodes: Seq[ParsedNode]) = JSONObject(Map(recurse(nodes):_ *))

  def recurse(nodes: Seq[ParsedNode]): List[(String, Any)] = nodes match {
    case (l: LmxmlNode) :: ns => single(l) :: recurse(ns)
    case n :: ns => recurse(ns)
    case Nil => Nil
  }

  def single(node: LmxmlNode): (String, Any) = {
    val LmxmlNode(name, attrs, c) = node
    val isArray = c.headOption.map(_.name == "arr").getOrElse(false)
    val isText = c.headOption.map(_.isInstanceOf[TextNode]).getOrElse(false)
    (name -> (
      if (attrs.isEmpty && isArray)
        JSONArray(flattenArray(c.head.children))
      else if (isText)
        c.headOption.map(_.asInstanceOf[TextNode].contents).map(numeric).get
      else JSONObject(numericAttrs(attrs) ++ Map(recurse(c):_*))
    ))
  }

  def numeric(str: String): Any = {
    booleanString.findFirstIn(str).map(_.toBoolean).getOrElse(
      decimalString.findFirstIn(str).map(_.toDouble).getOrElse(
        intString.findFirstIn(str).map(_.toInt).getOrElse(str)
      )
    )
  }

  def numericAttrs(attrs: Map[String, String]): Map[String, Any] = attrs.map {
    case (k, v) => k -> numeric(v)
  }

  def flattenArray(nodes: Seq[ParsedNode]): List[Any] = nodes match {
    case LmxmlNode(n, a, c) :: ns if (n == "obj") =>
      JSONObject(numericAttrs(a) ++ Map(recurse(c): _*)) :: flattenArray(ns)
    case (l: LmxmlNode) :: ns =>
      JSONObject(Map(single(l))) :: flattenArray(ns)
    case TextNode(t, _, c) :: ns =>
      t :: flattenArray(c) ::: flattenArray(ns)
    case _ => Nil
  }
}
