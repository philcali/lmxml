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

    val testString = """```
%s
```""" format text

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
    and #it { attr: "in here", attrs: "in there" }
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
}
