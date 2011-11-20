package lmxml

trait LmxmlFactory {
  def createParser(step: Int): LmxmlParsers

  def indention(contents: String) = {
    val Line = """^(\s+)""".r

    val cleaned = contents.replaceAll("\r\n", "\n")

    val found = cleaned.split("\n").find {
      line => !Line.findFirstIn(line).isEmpty
    }.map { l =>
      Line.findFirstMatchIn(l).get.group(1)
    }

    found.map(_.length).getOrElse(2)
  }

  def apply(contents: String) = createParser(indention(contents))
}

trait Conversion extends LmxmlFactory {
  def convert[A](contents: String)(implicit converter: Seq[ParsedNode] => A) = {
    apply(contents).fullParse(contents)(converter)
  }
}

trait FileLoader extends Conversion {
  def fromFile[A](path: String)(implicit converter: Seq[ParsedNode] => A) = {
    import scala.io.Source.{fromFile => open}

    val text = open(path).getLines.mkString("\n")

    convert(text)(converter)
  }
}

object Lmxml extends LmxmlFactory with Conversion with FileLoader {
  def createParser(step: Int) = PlainLmxmlParser(step)
}
