package lmxml

trait Processor extends Function2[Transform, ParsedNode, TextNode]

case class Value[A](data: A) extends Processor {
  def apply(transform: Transform, node: ParsedNode) = {
    TextNode(data.toString, children = transform(node.children))
  }
}

case class Fill[A](data: A => String) extends Processor {
  def apply(transform: Transform, node: ParsedNode) = {
    val original = node.name.split("\\-").take(1).mkString

    val potential = transform.data.find(_._1 == original + "_value")

    val result = potential.map( opt => 
      data(opt._2.asInstanceOf[Value[A]].data)
    ).getOrElse("")

    TextNode(result, children = transform(node.children))
  }
}

case class Foreach[A](data: List[A], sub: (String, Processor)*) extends Processor {
  def apply(transform: Transform, node: ParsedNode) = {
    val processors = transform.data ++ sub

    TextNode("", children = data.flatMap{ d =>
      val local: (String, Processor) = node.name + "_value" -> Value(d)

      val that = Transform((processors :+ local): _*)

      that(node.children)
    })
  }
}

case class Transform(
  data: (String, Processor)*
) extends LmxmlConvert[List[ParsedNode]] {

  private val mapped = Map[String, Processor](data: _*)

  def isApplicable(node: ParsedNode) = mapped.contains(node.name)

  def transform(node: ParsedNode) = {
    val transformed = mapped.get(node.name).map(_.apply(this, node)) 
    
    transformed getOrElse node
  }

  def copyNode(n: ParsedNode, nodes: List[ParsedNode]) = n match {
    case l: LmxmlNode => l.copy(children = nodes)
    case t: TextNode => t.copy(children = nodes)
    case _ => n
  }

  def apply(nodes: List[ParsedNode]) = nodes match {
    case n :: ns if isApplicable(n) => transform(n) :: apply(ns)
    case n :: ns => copyNode(n, apply(n.children)) :: apply(ns)
    case Nil => Nil
  }
}
