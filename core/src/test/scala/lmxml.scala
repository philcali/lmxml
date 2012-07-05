package lmxml
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class LmxmlTest extends FlatSpec with ShouldMatchers {
  "lmxml nodes" should "compile down to object nodes" in {
    val testString = """lmxml
  needs
    more
  testing

please
"""

    val expected = List(
      LmxmlNode("lmxml", children =
        List(
          LmxmlNode("needs", children =
            List(
              LmxmlNode("more")
            )
          ),
          LmxmlNode("testing")
        )
      ),
      LmxmlNode("please")
    )

    DefaultLmxmlParser.parseNodes(testString) should be === expected
  }

  it should "care about configured indention" in {
    val testString = """lmxml
    needs
        more
    testing
"""

    val expected = List(
      LmxmlNode("lmxml", children =
        List(
          LmxmlNode("needs", children =
            List(
              LmxmlNode("more")
            )
          ),
          LmxmlNode("testing")
        )
      )
    )

    PlainLmxmlParser(increment = 4).parseNodes(testString) should be === expected
  }

  "text nodes" should "be written as fenced in multi-liners" in {
    val text = """This is all that text that is written
in multi-lines... Think about javascript and
and css."""

    val testString = "```%s\n```" format text

    val expected = List(TextNode(text + "\n"))

    DefaultLmxmlParser.parseNodes(testString) should be === expected
  }

  it should "be written as alternate fenced in multi-liners" in {
    val text = """This is another long string
that extends multiple lines fenced in the alternate ~~~
characters."""

    val testString = "~~~%s\n~~~" format text

    val expected = List(TextNode(text + "\n"))

    DefaultLmxmlParser.parseNodes(testString) should be === expected
  }

  it should "be written as a single line" in {
    val text = "Single Line string... Like a title!"

    val testString = "\"%s\"" format text

    val expected = List(TextNode(text))

    DefaultLmxmlParser.parseNodes(testString) should be === expected
  }

  it should "have unescaped attributes assigned to them" in {
    val testString = "\"test <like this>\" is unescaped"

    val expected = List(TextNode("test <like this>", true))

    DefaultLmxmlParser.parseNodes(testString) should be === expected
  }

  it should "mix with other nodes peacefully" in {
    val testString = """lmxml
  has
    deeper
      "in here"
    and #it { "attr": "in here", "attrs": "in there" }
      ```
a wonderful

time
      ```
  in
    this
      "place"
"""

    val expected = List(
      LmxmlNode("lmxml", children = List(
        LmxmlNode("has", children = List(
          LmxmlNode("deeper", children = List(
            TextNode("in here")
          )),
          LmxmlNode("and", Map("id" -> "it", "attr" -> "in here", "attrs" -> "in there"), List(
            TextNode("a wonderful\n\ntime\n")
          ))
        )),
        LmxmlNode("in", children = List(
          LmxmlNode("this", children = List(
            TextNode("place")
          ))
        ))
      ))
    )

    DefaultLmxmlParser.parseNodes(testString) should be === expected
  }

  "templates" should "be written like indirect Mardown links" in {
    val testString = """lmxml
  [template]
----
[template]:
  "This is a test"
"""

    val expected = List(
      LmxmlNode("lmxml", children =
        List(TextNode("This is a test")))
    )

    DefaultLmxmlParser.parseNodes(testString) should be === expected
  }

  it should "be used multiple times" in {
    val testString = """lmxml
  one
    [template]
  two
    [template]
---
[template]:
  "test"
"""

    val expected = List(
      LmxmlNode("lmxml", children =
        List(
          LmxmlNode("one", children = List(TextNode("test"))),
          LmxmlNode("two", children = List(TextNode("test")))
        )
      )
    )

    DefaultLmxmlParser.parseNodes(testString) should be === expected
  }

  it should "have multiple template descendants" in {
    val testString = """lmxml
  [head]
    [downbelow]
  [downbelow]
---
[head]:
  test
    "more tests"

[downbelow]:
  more
    "tests and tests"
"""

    val expected = List(
      LmxmlNode("lmxml", children = List (
        LmxmlNode("test", children = List (
          TextNode("more tests", children = List (
            LmxmlNode("more", children = List (
              TextNode("tests and tests")
            ))
          ))
        )),
        LmxmlNode("more", children = List(
          TextNode("tests and tests")
        ))
      ))
    )

    DefaultLmxmlParser.parseNodes(testString) should be === expected
  }

  "Multiple class attribtes" should "be flatten into a single class attribute" in {
    val source = "lmxml .is .really .cool"

    val expected = List(LmxmlNode("lmxml", Map("class" -> "is really cool")))

    DefaultLmxmlParser.parseNodes(source) should be === expected
  }

  "Id, classes, and @ attributes" should "be reversable" in {
    val source = "lmxml #hot .is .really .cool @yeah"

    val source2 = "lmxml .is @yeah .really .cool #hot"

    DefaultLmxmlParser.parseNodes(source) should be === 
    DefaultLmxmlParser.parseNodes(source2)
  }

  "@ attrs" should "be short-hand for JSON style attrs" in {
    val source = "lmxml @checked @type = \"text\" @size: \"30\"" 

    val expected = List(
      LmxmlNode("lmxml",Map("checked" -> "checked", "type" -> "text", "size" -> "30"))
    )

    DefaultLmxmlParser.parseNodes(source) should be === expected
  }

  "Comments" should "be handled" in {
    val source = "html // then some"

    val expected = List(
      LmxmlNode("html", children = List(
        CommentNode(List(
          LmxmlNode("then", children = List(LmxmlNode("some")))
        ))
      ))
    )

    DefaultLmxmlParser.parseNodes(source) should be === expected
  }

  "Parsers" should "be extensible" in {
    trait HtmlShortcuts extends LmxmlParsers {
      val js: Parser[TopLevel] = "~" ~ "js" ~> inlineParams ^^ {
        case attrs =>
          LmxmlNode("script", Map("type" -> "text/javascript") ++ attrs, _)
      }

      override def topLevel = super.topLevel | js
    }

    val parser = new PlainLmxmlParser(2) with HtmlShortcuts

    val contents = """
html
  head
    ~js @src = "jquery.js"
    ~js
      ```
alert("Test");
      ``` is unescaped
"""

    val expected = List(
      LmxmlNode("html", children = List(
        LmxmlNode("head", children = List(
          LmxmlNode("script", Map("type" -> "text/javascript", "src" -> "jquery.js")),
          LmxmlNode("script", Map("type" -> "text/javascript"), List(
            TextNode("alert(\"Test\");\n", true)
          ))
        ))
      ))
    )

    parser.parseNodes(contents) should be === expected
  }

  "Factories" should "be extensible" in {
    object AlwaysFour extends LmxmlFactory with Conversion {
      def createParser(step: Int) = new PlainLmxmlParser(4)
    }

    AlwaysFour.createParser(0).increment should be === 4
  }

  "ResourceLoading" should "read resource files" in {
    object Lmxml extends PlainLmxmlFactory with ResourceLoading

    val xmlFormat = XmlFormat(200, 2)
    val result = Lmxml.fromResource("test.lmxml")(XmlConvert andThen xmlFormat)
    val expected = xmlFormat(
<html>
  <head>
    <title>This here a test!</title>
  </head>
  <body>
    <h1>ahoy me maties!</h1>
  </body>
</html>)

    result should be === expected
  }
}
