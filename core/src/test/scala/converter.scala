package lmxml
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class Converter extends FlatSpec with ShouldMatchers {

  val expectedXml =
<content>
  <friends>
    <friend id="1">
      <name>
        Philip Cali
      </name>
    </friend>
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
  
  "XML Converter" should "create xml from lmxml nodes" in {
    val nodes = List(
      LmxmlNode("content", children = List (
        LmxmlNode("friends", children = List (
          LmxmlNode("friend", Map("id" -> "1"), children = List (
            LmxmlNode("name", children = List (
              TextNode("Philip Cali")
            ))
          )),
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

    XmlConverter.convert(nodes).toString should be === expectedString
  }

  it should "work seemlessly as an implicit" in {
    implicit val xmlConverter = XmlConverter

    val text = """content
  friends
    friend #1
      name
        "Philip Cali"
    friend #2
      name
        "Charley & Bobby" is unescaped
  places
    place
      "New York"
    place
      "Baton Rouge"
"""

    Lmxml.convert(text).toString should be === expectedString
  }
}
