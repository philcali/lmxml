package lmxml

import util.parsing.{input, combinator}
import combinator._
import syntactical._
import input.CharSequenceReader

trait LmxmlParsers extends RegexParsers {
  val increment: Int

  type Nodes = List[ParsedNode]
  type TopLevel = (Nodes => ParsedNode)

  // Meaningful whitespace
  override val whiteSpace = """\s+?""".r

  lazy val allwp = """\s*""".r

  lazy val everything = """(?!\s*[```])[^\n]+\n""".r

  lazy val end = """(\r?\n)*"""

  lazy val ident = """[A-Za-z_]+""".r

  lazy val template = "[" ~> ident <~ "]"

  // Multi-line string
  lazy val stringLit = "\".*?\"".r ^^ { s =>
    s.substring(1, s.length -1)
  }

  lazy val alternateWrapper = "```" ~> rep1(everything | end) <~ allwp ~ "```" ^^ {
    ls => ls.reduceLeft(_ + _)
  }

  lazy val node: Parser[TopLevel] = ident ~ inlineParams ^^ {
    case name ~ attrs => LmxmlNode(name, attrs, _)
  }

  lazy val textNode: Parser[TopLevel] = (stringLit | alternateWrapper) ^^ {
    s => TextNode(s, _)
  }

  lazy val templateNode: Parser[TopLevel] = template ^^ {
    s => TemplateLink(s, _)
  }

  lazy val templateDef = template ~ ":" ~ nodesAt(increment) ^^ {
    case t ~ ":" ~ nodes => LinkDefinition(t, nodes)
  }

  lazy val topLevel = (node | textNode | templateNode)

  lazy val idAttr = "#" ~> ident ^^ {
    id => List(("id", id))
  }
  
  lazy val classAttr = "." ~> ident ^^ { 
    clazz => List(("class", clazz))
  }

  lazy val inlineAttrs = "{" ~ allwp ~> repsep(attr, "," <~ allwp) <~ allwp ~ "}" 
  
  lazy val attr = ident ~ ":" ~ stringLit ^^ {
    case key ~ ":" ~ value => (key, value)
  }

  lazy val inlineParams = opt(idAttr) ~ opt(classAttr) ~ opt(inlineAttrs) ^^ {
    case id ~ clazz ~ attrs =>
      val a = id.getOrElse(Nil) ++ clazz.getOrElse(Nil) ++ attrs.getOrElse(Nil)
      Map[String, String]() ++ a
  }

  lazy val separator = """\n*-*\n*""".r

  lazy val lmxml = nodesAt(0) ~ separator ~ repsep(templateDef, allwp) ^^ {
    case top ~ sep ~ linkDefs => 
      linkDefs.foldLeft(top) { rebuild(_, _) }
  }

  def spaces(n: Int) = """\s{%d}""".format(n).r

  def descending(d: Int = 0): Parser[Any] = 
   opt(spaces(d)) ~> topLevel ~ rep(descending(d + increment) | topLevel)

  private def descend(in: List[Any]): Nodes = in match {
    case (h ~ ns) :: rest =>
      val ls = ns.asInstanceOf[List[_]]
      val n = h.asInstanceOf[TopLevel]

      n(descend(ls)) :: descend(rest)
    case Nil => Nil
  }

  def nodesAt(startingDepth: Int = 0) = rep(descending(startingDepth)) ^^ {
    case all => descend(all) 
  }

  def safeParseNodes(contents: String) = {
    phrase(lmxml)(new CharSequenceReader(contents)) match {
      case Success(result, _) => Right(result)
      case Failure(msg, _) => Left(msg)
      case Error(msg, _) => Left(msg)
    }
  }

  def parseNodes(contents:String) = safeParseNodes(contents).fold({ e =>
    throw new IllegalArgumentException(e)
  }, nodes => nodes )

  def fullParse[A](contents: String)(implicit converter: LmxmlConverter[A]) = {
    converter.convert(parseNodes(contents))
  }

  private def rebuild(n: Nodes, link: LinkDefinition): Nodes = n match {
    case h :: rest => 
      def rebuildNode(y: ParsedNode): ParsedNode = y.children match {
        case (t: TemplateLink) :: cs if (t.name == link.name) =>
          copyNode(y, link.children ++ rebuild(t.children, link) ++ rebuild(cs, link))
        case t :: cs => copyNode(y, rebuildNode(t) :: rebuild(cs, link))
        case _ => copyNode(y, rebuild(y.children, link))
      }

      rebuildNode(h) :: rebuild(rest, link)
    case Nil => Nil
  }

  private def copyNode(n: ParsedNode, nodes: Nodes) = n match {
    case l: LmxmlNode => l.copy(children = nodes)
    case t: TextNode => t.copy(children = nodes)
    case _ => n
  }
}

object DefaultLmxmlParser extends LmxmlParsers {
  val increment = 2
}

case class PlainLmxmlParser(increment: Int) extends LmxmlParsers
