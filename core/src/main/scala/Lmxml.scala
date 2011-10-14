package lmxml

object Lmxml {
  lazy val Line = """(\s+?)[A-Za-z_0-9]+""".r

  def apply(contents: String) = {
    val cleaned = contents.replaceAll("\r\n", "\n")

    val found = cleaned.split("\n").find {
      line => !Line.findFirstIn(line).isEmpty
    }.map { l =>
      val Line(line) = l
      line
    }

    val incrementer = found.map(_.length).getOrElse(2)

    PlainLmxmlParser(incrementer) 
  }

  def convert[A](contents: String)(implicit converter: LmxmlConverter[A]) = {
    apply(contents).fullParse(contents)(converter)
  }
}
