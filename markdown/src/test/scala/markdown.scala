package lmxml
package markdown
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MarkdownTest extends FlatSpec with ShouldMatchers {
  val parser = new PlainLmxmlParser(2) with MarkdownParsing

  val source = """html
  body
    md ```
I'm talking with _emphasis_!


Did you __hear__ me!?


 - Work
 - Fun
 - Play
       ```
"""

  val expectedXml = """<p>I'm talking with <em>emphasis</em>!
</p><p>Did you <strong>hear</strong> me!?
</p><ul><li>Work
</li><li>Fun
</li><li>Play
</li></ul>"""

  "Markdown mixin" should "be able to parse mardown nodes" in {

    val expected = Seq(
      LmxmlNode("html", children = Seq(
        LmxmlNode("body", children = Seq(
          TextNode(expectedXml.toString, true, Nil)
        ))
      ))
    )

    parser.parseNodes(source) should be === expected
  }

  "Markdown convert" should "convert markdown upon conversion" in {
    val format = MarkdownConvert andThen XmlConvert andThen (_.toString)

    val expected = """<html><body><md>%s</md></body></html>""".format(expectedXml)

    DefaultLmxmlParser.fullParse(source)(format) should be === expected
  }
}
