package lmxml

import scala.io.Source.{fromFile => open}
import util.parsing.{input, combinator}
import combinator._
import syntactical._
import input.CharSequenceReader

import xml._
import java.io.File

object LmxmlApp {
  def file(path: String) = new File(path)

  def main(args: Array[String]) {
    args match {
      case Array(filepath) if (file(filepath).exists) =>
        Lmxml(open(filepath).getLines.mkString("\n"))
      case _ =>
        println("Please supply a lmxml file")
    }
  }
}

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

  lazy val separator = """\n*-*\n*""".r

  lazy val lmxml = nodesAt(0) ~ separator ~ repsep(templateDef, allwp) ^^ {
    case top ~ sep ~ linkDefs => 
      linkDefs.foldLeft(top) { rebuild(_, _) }
  }

  def parseNodes(contents: String) = {
    phrase(lmxml)(new CharSequenceReader(contents)) match {
      case Success(result, _) => Right(result)
      case n @ _ => Left(n)
    }
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

trait ParsedNode {
  val name: String
  val children: List[ParsedNode]
}

case class LmxmlNode(
  name: String, 
  attrs: Map[String, String], 
  children: List[ParsedNode] = Nil
) extends ParsedNode

case class TextNode(
  contents: String, 
  children: List[ParsedNode] = Nil
) extends ParsedNode {
  val name = "text"
}

case class TemplateLink(
  name: String,
  children: List[ParsedNode] = Nil
) extends ParsedNode

case class LinkDefinition(
  name: String,
  children: List[ParsedNode] = Nil
) extends ParsedNode

object Lmxml extends LmxmlParsers {
  import xml._

  val increment = 2

  def separateNode(n: ParsedNode) = n match {
    case LmxmlNode(name, attrs, _) =>
      val k = attrs.map(kv => "%s=\"%s\"".format(kv._1, kv._2)).mkString(" ")
      "%s %s".format(name, k)
    case TextNode(contents, _) =>
      "%s %s".format("Text: ", contents)
    case _ => n.name
  }

  def pretty(nodes: List[ParsedNode], depth: Int = 0) {
    val tab = (0 to depth).map(_ => " ").mkString("")

    for(node <- nodes) {
      println("%s%s".format(tab, separateNode(node))) 
      pretty(node.children, depth + 2)
    }
  }

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

  def apply(contents: String) = {
    parseNodes(contents).fold(println, { nodes =>
      println(convert(nodes))
    })
  }
}
