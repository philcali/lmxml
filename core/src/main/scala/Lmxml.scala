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

trait StreamReading extends Conversion {
  import java.io.{InputStream => JIn, BufferedReader, InputStreamReader}

  def fromStream[A](in: JIn)(implicit converter: Seq[ParsedNode] => A) = {
    val reader = new BufferedReader(new InputStreamReader(in, "UTF-8"))
    val sb = new StringBuilder()

    def pump: String = reader.readLine match {
      case line if line == null => reader.close(); sb.toString
      case line => sb.append(line + "\n"); pump
    }

    convert(pump)(converter)
  }
}

trait ResourceLoading extends StreamReading {
  def fromResource[A](name: String)(implicit converter: Seq[ParsedNode] => A)={
    fromStream(getClass.getResourceAsStream("/" + name))(converter)
  }
}

trait FileLoading extends StreamReading {
  import java.io.{File, FileInputStream}

  def fromFile[A](path: String)(implicit converter: Seq[ParsedNode] => A): A = {
    fromFile(new File(path))(converter)
  }

  def fromFile[A](file: File)(implicit converter: Seq[ParsedNode] => A) = {
    fromStream(new FileInputStream(file))(converter)
  }
}
