package lmxml
package shortcuts.html

trait HtmlShortcuts extends LmxmlParsers {
  def docContent = """([^\n]+)""".r ^^ { s => s }

  def doctype: Parser[TopLevel] = "!" ~> docContent ^^ {
    case content => TextNode("<!DOCTYPE %s>\n".format(content), true, _)
  }

  def js: Parser[TopLevel] = "js" ~> inlineParams ^^ {
    case attrs =>
      LmxmlNode("script", Map("type" -> "text/javascript") ++ attrs, _)
  }

  def css: Parser[TopLevel] = "css" ~> inlineParams ^^ {
    case attrs if attrs.isEmpty =>
      LmxmlNode("style", Map(), _)
    case attrs =>
      val defaultAttrs = Map(
        "rel" -> "stylesheet",
        "type" -> "text/css"
      )
      LmxmlNode("link", defaultAttrs ++ attrs, _)
  }

  def div: Parser[TopLevel] = ">" ~> inlineParams ^^ {
    case attrs =>
      LmxmlNode("div", attrs, _)
  }

  override def topLevel = (doctype | js | css | div) | super.topLevel
}
