package lmxml
package markdown
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MarkdownTest extends FlatSpec with ShouldMatchers {
  val parser = new PlainLmxmlParser(2) with MarkdownParsing

  val source = """html
  head title "test"
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
        LmxmlNode("head", children = Seq(
          LmxmlNode("title", children = Seq(TextNode("test")))
        )),
        LmxmlNode("body", children = Seq(
          TextNode(expectedXml.toString, true, Nil)
        ))
      ))
    )

    parser.parseNodes(source) should be === expected
  }

  "Markdown convert" should "convert markdown upon conversion" in {
    val format = MarkdownConvert andThen XmlConvert andThen (_.toString)

    val header = "<head><title>test</title></head>" 
    val expected = """<html>%s<body>%s</body></html>""".format(header, expectedXml)

    DefaultLmxmlParser.fullParse(source)(format) should be === expected
  }
}
