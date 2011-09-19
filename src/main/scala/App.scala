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
  type NodeLike = (String, Map[String, String])

  // Meaningful whitespace
  override val whiteSpace = """\s+?""".r

  lazy val allwp = """\s*""".r

  lazy val everything = """(?!\s*[```])[^\n]+\n""".r

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

  lazy val allNodes = (node | textNode)

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

  def descending(d: Int = 0, incr: Int = 2): Parser[Any] = 
   opt(spaces(d)) ~> allNodes ~ rep(descending(d + incr) | allNodes)

  private def descend(in: List[Any]): List[ParsedNode] = in match {
    case (h ~ ns) :: rest =>
      val ls = ns.asInstanceOf[List[_]]

      val n = h match {
        case s: String => 
          TextNode(s, descend(ls))
        case _ =>
          val s = h.asInstanceOf[NodeLike]
          LmxmlNode(s._1, s._2, descend(ls))
      }

      n :: descend(rest)
    case Nil => Nil
  }

  lazy val lmxml = rep(descending(0)) ^^ {
    case all => descend(all)
  }
}

trait ParsedNode {
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
) extends ParsedNode

object Lmxml extends LmxmlParsers {

  def pretty(nodes: List[ParsedNode], depth: Int = 0) {
    val tab = (0 to depth).map(_ => " ").mkString("")

    for(node <- nodes) {
      println("%s%s".format(tab, node)) 
      pretty(node.children, depth + 2)
    }
  }

  def apply(contents: String) = {

    phrase(lmxml)(new CharSequenceReader(contents)) match {
      case Success(result, _) => println(result)
      case n @ _ => println(n)
    }
  }
}
