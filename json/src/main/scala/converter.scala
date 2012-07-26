package lmxml
package converters
package json

import util.parsing.json.{
  JSONArray,
  JSONObject,
  JSONFormat
}

object JsonFormat extends (Any => String) {
  def apply(j: Any): String = single(j)

  def hDepth(i: Int) = (0 until i).map(_ => " ").foldLeft("")(_ + _)

  def single(j: Any, d: Int = 0): String = j match {
    case JSONObject(obj) =>
      obj.map(single(_, d + 2))
         .mkString("{\n", ",\n", "\n%s}" format (hDepth(d)))
    case JSONArray(arr) => arr.map(single(_, d)).mkString("[", ", ", "]")
    case (k, v) => "%s%s : %s" format (hDepth(d), single(k, d), single(v, d))
    case s: String => "\"%s\"" format JSONFormat.quoteString(s)
    case n => n.toString()
  }
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
    // Catch the transformed output
    case TextNode(t, _, c) :: ns if (!t.isEmpty) =>
      t :: flattenArray(c) ::: flattenArray(ns)
    case node :: ns =>
      flattenArray(node.children) ::: flattenArray(ns)
    case _ => Nil
  }
}
