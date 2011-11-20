package lmxml

object Lmxml {
  lazy val Line = """^(\s+)""".r

  def apply(contents: String) = {
    val cleaned = contents.replaceAll("\r\n", "\n")

    val found = cleaned.split("\n").find {
      line => !Line.findFirstIn(line).isEmpty
    }.map { l =>
      Line.findFirstMatchIn(l).get.group(1)
    }

    val incrementer = found.map(_.length).getOrElse(2)

    PlainLmxmlParser(incrementer) 
  }

  def convert[A](contents: String)(implicit converter: Seq[ParsedNode] => A) = {
    apply(contents).fullParse(contents)(converter)
  }

  def fromFile[A](path: String)(implicit converter: Seq[ParsedNode] => A) = {
    import scala.io.Source.{fromFile => open}

    val text = open(path).getLines.mkString("\n")

    convert(text)(converter)
  }
}
