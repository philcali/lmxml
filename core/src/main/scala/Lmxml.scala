package lmxml

trait PlainLmxmlFactory extends LmxmlFactory {
  def createParser(step: Int) = PlainLmxmlParser(step)
}

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

trait FileLoading extends Conversion {
  import java.io.File

  def fromFile[A](path: String)(implicit converter: Seq[ParsedNode] => A): A = {
    fromFile(new File(path))(converter)
  }

  def fromFile[A](file: File)(implicit converter: Seq[ParsedNode] => A) = {
    import scala.io.Source.{fromFile => open}

    val text = open(file).getLines.mkString("\n")

    convert(text)(converter)
  }
}
