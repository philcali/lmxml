package lmxml
package template
package test

import java.io.File

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TemplateTest extends FlatSpec with ShouldMatchers {
  val base =
"""
html
  head title [title]
  body
    div #content [rest]
    div #footer p "&copy; Philip Cali" is unescaped
"""

  val extension =
"""
[base]
---
[title]:
  "Guaranteed Victory"

[rest]:
  h1 "This is a test"
  div .pages
    pages
      div.page
        div .page-header page-header
        div .page-body page-body
"""

  def writeContents(name: String, contents: String) {
    val writer = new java.io.FileWriter(name)
    writer.write(contents)
    writer.close()
  }

  case class Page(header: String, body: String)

  val parser = new PlainLmxmlParser(2) with FileTemplate {
    val working = new File(".")
  }

  "FileTemplate" should "dynamically find other templates" in {    
    import scala.io.Source.{fromFile => open}
    import transforms._

    writeContents("base.lmxml", base)
    writeContents("extension.lmxml", extension)

    val contents = open("extension.lmxml").getLines.mkString("\n")
    val nodes = parser.parseNodes(contents)

    val pages = List(
      Page("Test title", "This is the body"),
      Page("Second Page", "Do this, that, and that.")
    )

    val trans = Transform(
      "pages" -> Foreach(pages) { page => Seq(
        "page-header" -> Value(page.header),
        "page-body" -> Value(page.body)
      ) }
    )

    val format = new xml.PrettyPrinter(200, 2).formatNodes(_: xml.NodeSeq)
    val fullOutput = trans andThen XmlConvert andThen format

    val expected =
<html>
  <head>
    <title>Guaranteed Victory</title>
  </head>
  <body>
    <div id="content">
      <h1>This is a test</h1>
      <div class="pages">
        <div class="page">
          <div class="page-header">Test title</div>
          <div class="page-body">This is the body</div>
        </div>
        <div class="page">
          <div class="page-header">Second Page</div>
          <div class="page-body">Do this, that, and that.</div>
        </div>
      </div>
    </div>
    <div id="footer">
      <p>&copy; Philip Cali</p>
    </div>
  </body>
</html>

    fullOutput(nodes).toString() should be === expected.toString()

    new File("base.lmxml").delete()
    new File("extension.lmxml").delete()
  }
}
