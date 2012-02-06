package lmxml

package transforms

trait Processor extends Function2[Transform, ParsedNode, TextNode]

case class Value[A](data: A, unparsed: Boolean = false) extends Processor {
  def apply(transform: Transform, node: ParsedNode) = {
    TextNode(data.toString, unparsed, transform(node.children))
  }
}

case object Empty extends Processor {
  def apply(transform: Transform, node: ParsedNode) = {
    TextNode("", children = Nil)
  }
}

trait Pred extends Processor {
  def isTrue: Boolean

  def booleanProcessors(node: ParsedNode) = {
    if (isTrue) {
      Seq (
        node.name + "-true" -> Value(""),
        node.name + "-false" -> Empty
      )
    } else {
      Seq (
        node.name + "-true" -> Empty,
        node.name + "-false" -> Value("")
      )
    }
  }

  def generateData(fromNode: ParsedNode): Seq[(String, Processor)]

  def apply(transform: Transform, node: ParsedNode) = {
    val data = generateData(node)

    val that = Transform((transform.data ++ data): _*)

    TextNode("", children = that(node.children))
  }
}

case class Else(fail: Seq[(String, Processor)]) extends Processor with Pred {
  def isTrue = false

  def generateData(fromNode: ParsedNode) = fail ++ booleanProcessors(fromNode)
}

case class If(pred: Boolean)
             (success: Seq[(String, Processor)] = Nil) extends Processor with Pred {
 
  def isTrue = true
 
  def orElse(fail: Seq[(String, Processor)]) = if (!pred) Else(fail) else this

  def generateData(fromNode: ParsedNode) = if (pred) {
    success ++ booleanProcessors(fromNode)
  } else {
    Else(Nil).booleanProcessors(fromNode)
  }
}

case class Fill[A](data: A => String, unparsed: Boolean = false) extends Processor {
  def apply(transform: Transform, node: ParsedNode) = {
    val original = node.name.split("\\-").take(1).mkString

    val potential = transform.data.find(_._1 == original + "_value")

    val result = potential.map( opt => 
      data(opt._2.asInstanceOf[Value[A]].data)
    ).getOrElse("")

    TextNode(result, unparsed, transform(node.children))
  }
}

case class Foreach[A](data: Seq[A])(f: A => Seq[(String, Processor)]) extends Processor {
  def apply(transform: Transform, node: ParsedNode) = {
    TextNode("", children = data.flatMap{ d =>
      val locals = transform.data ++ f(d)

      val that = Transform(locals: _*)

      that(node.children)
    })
  }
}

case class Transform(
  data: (String, Processor)*
) extends LmxmlConvert[List[ParsedNode]] {

  private val mapped = Map[String, Processor](data: _*)

  private lazy val Embed = """\{\s*([A-Za-z0-9_\-]+)\s*\}""".r

  private def valReplace(value: String) = {
    Embed.findAllIn(value).foldLeft(value) { (in, found) =>
      val Embed(key) = found

      val item = mapped.get(key).map(_.asInstanceOf[Value[_]])
      val replacer = item.map(_.data.toString).getOrElse("")

      in.replace(found, replacer)
    }
  }

  def isApplicable(node: ParsedNode) = mapped.contains(node.name)

  def transform(node: ParsedNode) = {
    val transformed = mapped.get(node.name).map(_.apply(this, node)) 
    
    transformed getOrElse node
  }

  def copyNode(n: ParsedNode, nodes: Seq[ParsedNode]) = n match {
    case l: LmxmlNode =>
      val attrs = l.attrs.map{ 
        case (k, v) => k -> valReplace(v)
      }

      l.copy(attrs = attrs, children = nodes)
    case t: TextNode => t.copy(children = nodes)
    case _ => n
  }

  def apply(nodes: Seq[ParsedNode]) = nodes match {
    case n :: ns if isApplicable(n) => transform(n) :: apply(ns)
    case n :: ns => copyNode(n, apply(n.children)) :: apply(ns)
    case Nil => Nil
  }
}
