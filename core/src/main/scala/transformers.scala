package lmxml

package transforms

trait Processor extends ((Transform, ParsedNode) => ParsedNode)

case class Values(data: Seq[(String, Processor)]) extends Processor {
  def apply(transform: Transform, node: ParsedNode) = {
    val that = transform + Transform(data: _*)
    EmptyNode(that(node.children))
  }
}

case class Value[A](data: A, unparsed: Boolean = false) extends Processor {
  def apply(transform: Transform, node: ParsedNode) = {
    val value = data.toString
    if (value.isEmpty) EmptyNode(transform(node.children)) else {
      TextNode(value, unparsed, transform(node.children))
    }
  }
}

case object Empty extends Processor {
  def apply(transform: Transform, node: ParsedNode) = {
    EmptyNode(Nil)
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

    EmptyNode(that(node.children))
  }
}

// Objects to facilitate call-by-name parameters
object Else {
  def apply(fail: => Seq[(String, Processor)]) = new Else(fail)

  def unapply(node: ParsedNode) =
    if (node.name == "else") Some(node) else None
}

class Else(fail: => Seq[(String, Processor)]) extends Processor with Pred {
  def isTrue = false

  def generateData(fromNode: ParsedNode) = fail ++ booleanProcessors(fromNode)
}

object If {
  def apply(pred: => Boolean)
           (block: => Seq[(String, Processor)] = Nil) = new If(pred)(block)

  def unapply(node: ParsedNode) =
    if (node.name.startsWith("if-")) Some(node) else None
}

class If (pred: => Boolean)
         (success: => Seq[(String, Processor)]) extends Processor with Pred {

  def isTrue = pred

  def orElse(f: => Seq[(String, Processor)]) = if (isTrue) this else Else(f)

  def generateData(fromNode: ParsedNode) = if (pred) {
    success ++ booleanProcessors(fromNode)
  } else {
    Else(Nil).booleanProcessors(fromNode)
  }
}

case class Foreach[A](data: Seq[A])(f: A => Seq[(String, Processor)]) extends Processor {
  def apply(transform: Transform, node: ParsedNode) = {
    EmptyNode(data.flatMap{ d =>
      val locals = transform.data ++ f(d)

      val that = Transform(locals: _*)

      that(node.children)
    })
  }
}

case class Transform(data: (String, Processor)*) extends SinglePass[ParsedNode] {

  private val mapped = Map[String, Processor](data: _*)

  private lazy val Embed = """\{\s*([A-Za-z0-9_\-]+)\s*\}""".r

  private def valReplace(value: String) = {
    Embed.findAllIn(value).foldLeft(value) { (in, found) =>
      val Embed(key) = found

      val item = mapped.get(key).filter(_.isInstanceOf[Value[_]])
      val replacer = item.map(_.asInstanceOf[Value[_]]).map(_.data.toString)

      in.replace(found, replacer.getOrElse(""))
    }
  }

  def + (other: Transform) = Transform((data ++ other.data): _*)

  def isApplicable(node: ParsedNode) = mapped.contains(node.name)

  def transform(node: ParsedNode) = {
    val transformed = mapped.get(node.name).map(_.apply(this, node))
    transformed getOrElse node
  }

  def copyNode(n: ParsedNode, nodes: Seq[ParsedNode]) = n match {
    case l: LmxmlNode =>
      val attrs = l.attrs.filter {
        case (k, v) => valReplace(k).length > 0
      }.map {
        case (k, v) => valReplace(k) -> valReplace(v)
      }

      l.copy(attrs = attrs, children = nodes)
    case t: TextNode =>
      t.copy(contents = valReplace(t.contents), children = nodes)
    case e: EmptyNode =>
      e.copy(children = nodes)
    case _ => n
  }

  override def apply(nodes: Seq[ParsedNode]) = nodes match {
    case If(success) :: Else(fail) :: rest =>
      conditional(success, Some(fail)) :: apply(rest)
    case If(success) :: rest =>
      conditional(success, None) :: apply(rest)
    case _ => super.apply(nodes)
  }

  def single(n: ParsedNode) =
    if (isApplicable(n)) transform(n) else copyNode(n, apply(n.children))

  def conditional(success: ParsedNode, fail: Option[ParsedNode]) = {
    def fallback = copyNode(success, apply(success.children))

    if (isApplicable(success)) {
      mapped.get(success.name).map {
        case cond: Pred if cond.isTrue => cond.apply(this, success)
        case c => fail.map(c.apply(this, _)).getOrElse(Empty(this, success))
      }.getOrElse(transform(success))
    } else fallback
  }
}
