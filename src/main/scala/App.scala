package lmxml

import scala.io.Source.{fromFile => open}
import util.parsing.{input, combinator}
import combinator._
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
  type NodeLike = (String, Map[String, String], Option[String])

  override def skipWhitespace = false

  lazy val end = """\n?""".r

  lazy val ident = """[A-Za-z_]+""".r

  lazy val stringLit = "\".*\"".r

  lazy val node = ident ~ inlineParams ~ opt(stringLit) <~ end ^^ {
    case name ~ attrs ~ text => (name, attrs, text)
  }

  lazy val idAttr = "#" ~> ident ^^ { id => List(("id", id)) }
  
  lazy val classAttr = "." ~> ident ^^ { clazz => List(("class", clazz)) }

  lazy val inlineAttrs = "{" ~> repsep(attr, ",") <~ "}" 
  
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
   opt(spaces(d)) ~> node ~ rep(descending(d + 2) | node)

  private def descend(in: List[Any]): List[LmxmlNode] = in match {
    case (h ~ ns) :: rest =>
      val s = h.asInstanceOf[NodeLike]
      val ls = ns.asInstanceOf[List[_]]

      LmxmlNode(s._1, s._2, descend(ls)) :: descend(rest)
    case Nil => Nil
  }

  lazy val lmxml = rep(descending(0)) ^^ {
    case all => descend(all)
  }
}

trait ParsedNode

case class LmxmlNode(
  name: String, 
  attrs: Map[String, String], 
  children: List[LmxmlNode] = Nil
) extends ParsedNode

case class TextNode(contents: String) extends ParsedNode

object Lmxml extends LmxmlParsers {

  def pretty(nodes: List[LmxmlNode], depth: Int = 0) {
    val tab = (0 to depth).map(_ => " ").mkString("")

    for(node <- nodes) {
      val attrs = node.attrs.map(kv => "%s=\"%s\"".format(kv._1, kv._2))

      println("%s%s %s".format(tab, node.name, attrs.mkString(" ")))
      pretty(node.children, depth + 2)
    }
  }

  def apply(contents: String) = {

    phrase(lmxml)(new CharSequenceReader(contents)) match {
      case Success(result, _) =>  pretty(result)
      case n @ _ => println(n)
    }
  }
}
