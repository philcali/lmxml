package lmxml

import util.parsing.{input, combinator}
import combinator._
import syntactical._
import input.CharSequenceReader

trait LmxmlParsers extends RegexParsers {
  val increment: Int

  type Nodes = Seq[ParsedNode]
  type TopLevel = (Nodes => ParsedNode)

  // Meaningful whitespace
  override val whiteSpace = """\s+?""".r

  lazy val allwp = """\s*""".r

  lazy val everything = """(?!\s*```)[^\n]+\n""".r

  lazy val end = """[ ]*\r?\n""".r ^^ { case _ => "\n" }

  lazy val ident = """\b[A-Za-z0-9_-]+""".r

  lazy val template = "[" ~> ident <~ "]"

  lazy val stringLit = "\".*?\"".r ^^ { s =>
    s.substring(1, s.length -1)
  }

  lazy val strWrapper = "```" ~> rep1(everything | end) <~ allwp ~ "```" ^^ {
    ls => ls.reduceLeft(_ + _)
  }

  lazy val commentNode: Parser[TopLevel] = "//" ^^ { _ => CommentNode(_) }

  lazy val node: Parser[TopLevel] = ident ~ inlineParams ^^ {
    case name ~ attrs => LmxmlNode(name, attrs, _)
  }

  lazy val textNode: Parser[TopLevel] = 
    (stringLit | strWrapper) ~ opt(unescapedAttr) ^^ {
      case s ~ e =>
        TextNode(s, e.getOrElse(false), _)
    }

  lazy val templateNode: Parser[TopLevel] = template ^^ {
    s => TemplateLink(s, _)
  }

  lazy val templateDef = template ~ ":" ~ nodesAt(increment) ^^ {
    case t ~ ":" ~ nodes => LinkDefinition(t, nodes)
  }

  lazy val unescapedAttr = "is" ~> "unescaped" ^^ { _ => true }

  lazy val idAttr = "#" ~> ident ^^ {
    id => ("id", id)
  }
  
  lazy val classAttr = "." ~> ident ^^ { 
    clazz => ("class", clazz)
  }

  lazy val atAttr = "@" ~> ident ~ opt(("=" | ":") ~> stringLit) ^^ {
    case key ~ someValue => (key, someValue.getOrElse(key))
  }

  lazy val inlineAttrs = "{" ~ allwp ~> repsep(attr, "," <~ allwp) <~ allwp ~ "}" 
  
  lazy val attr = stringLit ~ ":" ~ stringLit ^^ {
    case key ~ ":" ~ value => (key, value)
  }

  lazy val separator = """\n*-*\n*""".r

  def someAttr = idAttr | classAttr | atAttr

  def inlineParams = rep(someAttr) ~ opt(inlineAttrs) ^^ {
    case other ~ attrs =>
      val a = flattenAttrList(other) ++ attrs.getOrElse(Nil) 
      Map[String, String]() ++ a
  }

  def topLevel = (node | textNode | templateNode | commentNode)

  def lmxml = nodesAt(0) ~ separator ~ repsep(templateDef, allwp) ^^ {
    case top ~ sep ~ linkDefs => 
      linkDefs.foldLeft(top) { rebuild(_, _) }
  }

  def flattenAttrList(tups: List[(String, String)]) = {
    if (tups.isEmpty) Nil else { 
      val unique = tups.map(_._1).distinct
      unique.map { key =>
        key -> tups.filter(_._1 == key).map(_._2).mkString(" ")
      }
    }
  }

  def spaces(n: Int) = """\s{%d}""".format(n).r

  def descending(d: Int): Parser[Any] = 
   opt(spaces(d)) ~> topLevel ~ rep(descending(d + increment) | topLevel)

  private def descend(in: List[Any]): Nodes = in match {
    case (h ~ ns) :: rest =>
      val ls = ns.asInstanceOf[List[_]]
      val n = h.asInstanceOf[TopLevel]

      Seq(n(descend(ls))) ++ descend(rest)
    case Nil => Nil
  }

  def nodesAt(startingDepth: Int) = rep(descending(startingDepth)) ^^ {
    case all => descend(all) 
  }

  def safeParseNodes(contents: String) = {
    phrase(lmxml)(new CharSequenceReader(contents)) match {
      case Success(result, _) => Right(result)
      case n: ParseResult[_] => Left(n)
    }
  }

  def parseNodes(contents: String) = safeParseNodes(contents).fold({ e =>
    throw new IllegalArgumentException(e.toString)
  }, nodes => nodes)

  def fullParse[A](contents: String)(implicit converter: Nodes => A) = {
    converter(parseNodes(contents))
  }

  protected def rebuild(n: Nodes, link: LinkDefinition): Nodes = n match {
    case (h: TemplateLink) :: rest if (h.name == link.name) =>
      def rapidDescentAdder(ns: Nodes): Nodes = {
        val fin = ns.lastOption
        if (fin.isEmpty) {
          rebuild(h.children, link)
        } else {
          val rest = ns.dropRight(1)
          rest :+ copyNode(fin.get, rapidDescentAdder(fin.get.children))
        }
      }
      rapidDescentAdder(link.children) ++ rebuild(rest, link)
    case h :: rest =>
      Seq(copyNode(h, rebuild(h.children, link))) ++ rebuild(rest, link)
    case Nil => Nil
  }

  protected def copyNode(n: ParsedNode, nodes: Nodes) = n match {
    case l: LmxmlNode => l.copy(children = nodes)
    case t: TextNode => t.copy(children = nodes)
    case y: TemplateLink => y.copy(children = nodes)
    case _ => n
  }
}

object DefaultLmxmlParser extends LmxmlParsers {
  val increment = 2
}

case class PlainLmxmlParser(increment: Int) extends LmxmlParsers
