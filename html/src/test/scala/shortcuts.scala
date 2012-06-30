package lmxml
package shortcuts.html
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ShortcutSpec extends FlatSpec with ShouldMatchers {
  object ShortcutParser extends PlainLmxmlParser(2) with HtmlShortcuts

  object DynoParser extends PlainLmxmlParser(2) with DynamicShortcuts {
    val definition = Seq(define("text", "input", Map("type" -> "text")))
  }

  "HtmlShortcuts" should "determine doctypes easily" in {
    val test = """
!html
html
"""
    val expected = List(
      TextNode("<!DOCTYPE html>\n", true, List(
        LmxmlNode("html")
      ))
    )

    ShortcutParser.parseNodes(test) should be === expected
  }

  it should "shorten embedded js scripts" in {
    val test = """
html
  head
    js @src="/jquery.js"
    js "alert('Hello');" is unescaped
"""

    val expected = List(
      LmxmlNode("html", children = List(
        LmxmlNode("head", children = List(
          LmxmlNode("script", Map("type" -> "text/javascript", "src" -> "/jquery.js")),
          LmxmlNode("script", Map("type" -> "text/javascript"), List(
            TextNode("alert('Hello');", true)
          ))
        ))
      ))
    )

    ShortcutParser.parseNodes(test) should be === expected
  }

  it should "shorten embedded css" in {
    val test = """
html
  head
    css @href="/bootstrap.css"
    css "body { color: white; }" is unescaped
"""

    val expected = List(
      LmxmlNode("html", children = List(
        LmxmlNode("head", children = List(
          LmxmlNode("link", Map("rel" -> "stylesheet", "type" -> "text/css", "href" -> "/bootstrap.css")),
          LmxmlNode("style", children = List(
            TextNode("body { color: white; }", true)
          ))
        ))
      ))
    )

    ShortcutParser.parseNodes(test) should be === expected
  }

  it should "shorten div creation" in {
    val test = """
>.container
  >.row
    >.span4 h2 "What's up?"
    >.span8 p "Talk around town"
"""

    val expected = List(
      LmxmlNode("div", Map("class" -> "container"), List(
        LmxmlNode("div", Map("class" -> "row"), List(
          LmxmlNode("div", Map("class" -> "span4"), List(
            LmxmlNode("h2", Map(), List(TextNode("What's up?")))
          )),
          LmxmlNode("div", Map("class" -> "span8"), List(
            LmxmlNode("p", Map(), List(TextNode("Talk around town")))
          ))
        ))
      ))
    )

    ShortcutParser.parseNodes(test) should be === expected
  }

  "DynamicShortcuts" should "be recognized" in {
    import transforms.{ Transform, Value, Foreach }

    val src = """elems text @placeholder="{value}" """

    val elems = List("First name", "Surname")
    val trans = Transform(
      "elems" -> Foreach(elems)(v => Seq("value" -> Value(v)))
    )

    DynoParser.fullParse(src)(trans) should be === List(
      TextNode("", children = List(
        LmxmlNode("input", Map("type" -> "text", "placeholder" -> "First name")),
        LmxmlNode("input", Map("type" -> "text", "placeholder" -> "Surname"))
      ))
    )
  }
}
