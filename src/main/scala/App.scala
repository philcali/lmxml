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

  type NodeLike = (String, Map[String, String])
  type Nodes = List[ParsedNode]

  // Meaningful whitespace
  override val whiteSpace = """\s+?""".r

  lazy val allwp = """\s*""".r

  lazy val everything = """(?!\s*[```])[^\n]+\n""".r

  lazy val template = "[" ~ ident ~ "]"

  lazy val templateDef = template ~ ":" ~ lmxml(increment) ^^ {
    case ("[" ~ t ~ "]") ~ ":" ~ nodes => LinkDefinition(t, nodes)
  }

  lazy val end = """(\r?\n)*"""

  lazy val ident = """[A-Za-z_]+""".r

  // Multi-line string
  lazy val stringLit = "\".*?\"".r ^^ { s =>
    s.substring(1, s.length -1)
  }

  lazy val alternateWrapper = "```" ~> rep1(everything | end) <~ allwp ~ "```" ^^ {
    ls => ls.reduceLeft(_ + _)
  }

  lazy val node = ident ~ inlineParams ^^ {
    case name ~ attrs => (name, attrs)
  }

  lazy val textNode = (stringLit | alternateWrapper)

  lazy val allNodes = (node | textNode | templateDef | template)

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
   opt(spaces(d)) ~> allNodes ~ rep(descending(d + increment) | allNodes)

  private def descend(in: List[Any]): Nodes = in match {
    case (h ~ ns) :: rest =>
      val ls = ns.asInstanceOf[List[_]]

      val n = h match {
        // Pass Through
        case l: LinkDefinition => l
        case "[" ~ l ~ "]" =>
          TemplateLink(l.toString, descend(ls))
        case s: String => 
          TextNode(s, descend(ls))
        case _ =>
          val s = h.asInstanceOf[NodeLike]
          LmxmlNode(s._1, s._2, descend(ls))
      }

      n :: descend(rest)
    case Nil => Nil
  }

  def lmxml(startingDepth: Int = 0) = rep(descending(startingDepth)) ^^ {
    case all => 
      val nodes = descend(all)
      val (linkDefs, other) = nodes.partition(_.isInstanceOf[LinkDefinition])
      linkDefs.foldLeft(other) { (rebuilt, linkDef) =>
        rebuild(rebuilt, linkDef.asInstanceOf[LinkDefinition])
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
  val increment = 2

  def pretty(nodes: List[ParsedNode], depth: Int = 0) {
    val tab = (0 to depth).map(_ => " ").mkString("")

    for(node <- nodes) {
      println("%s%s".format(tab, node.name)) 
      pretty(node.children, depth + 2)
    }
  }

  def apply(contents: String) = {

    phrase(lmxml(0))(new CharSequenceReader(contents)) match {
      case Success(result, _) => pretty(result)
      case n @ _ => println(n)
    }
  }
}
