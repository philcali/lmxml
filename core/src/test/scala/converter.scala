package lmxml
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class Converter extends FlatSpec with ShouldMatchers {
  val expectedXml =
<content>
  <friends>
    <friend name="Philip Cali" id="1" age="999"/>
    <friend id="2">
      <name>
        {xml.Unparsed("Charley & Bobby")}
      </name>
    </friend>
  </friends>
  <places>
    <place>
      New York
    </place>
    <place>
      Baton Rouge
    </place>
  </places>
</content>

  val expectedString = expectedXml.toString.split("\n").map(_.trim).mkString("")
  
  "XMLConvert" should "create xml from lmxml nodes" in {
    val nodes = List(
      LmxmlNode("content", children = List (
        LmxmlNode("friends", children = List (
          LmxmlNode("friend", 
            Map("name" -> "Philip Cali", "id" -> "1", "age" -> "999")
          ),
          LmxmlNode("friend", Map("id" -> "2"), children = List (
            LmxmlNode("name", children = List (
              TextNode("Charley & Bobby", true)
            ))
          ))
        )),
        LmxmlNode("places", children = List (
          LmxmlNode("place", children = List (
            TextNode("New York")
          )),
          LmxmlNode("place", children = List (
            TextNode("Baton Rouge")
          ))
        ))
      ))
    )

    XmlConvert(nodes).toString should be === expectedString
  }

  it should "work seemlessly as an implicit" in {
    val text = """content
  friends
    friend #1 { "age": "999", "name": "Philip Cali" }
    friend #2
      name
        "Charley & Bobby" is unescaped
  places
    place
      "New York"
    place
      "Baton Rouge"
"""

    DefaultLmxmlParser.fullParse(text)(XmlConvert).toString should be === expectedString
  }

  it should "convert comments successfully" in {
    val source = """
html
  // "This is some arbitrary text"
  head title "Test"
"""

    val expected = """<html>
<!--
This is some arbitrary text
-->
</html>"""

    DefaultLmxmlParser.fullParse(source)(XmlConvert).toString should be === expected
  }

  it should "convert comments nodes" in {
    val source = """
html
  //
    head
      title "Test"
"""

    val expected = """<html>
<!--
<head><title>Test</title></head>
-->
</html>"""

    DefaultLmxmlParser.fullParse(source)(XmlConvert).toString should be === expected
  }
}
